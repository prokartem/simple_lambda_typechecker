{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module TypeChecker
    (
        someFunc,
        check
        -- eval,
        -- showTerm,
        -- Term,
        -- Bind,
        -- ContextNote
    ) where
import Parser
import Numeric (showEFloat)
import Text.Printf (IsChar(toChar))
import Text.Parsec


-- type Type = String

-- data Ty = TyArr {tyT1 :: Type, tyT2 :: Type}

-- data Term =
--     TmVar { info :: Int, value :: Int, contextLength :: Int }
--     | TmAbs { info :: Int, nameHint :: String, typ :: Type, term :: Term }
--     | TmApp { info :: Int, term1 :: Term, term2 :: Term }

-- data Bind = Bind
--     | VarBind {ty :: Type}

type Name = String

data Formula = Formula { var :: Name, varType :: Type } deriving Show

type Context = [Formula] 

-- data ContextNote = ContextNote { name :: String, bind :: Bind }

-- type Context = [ContextNote]

addFormula :: Context -> Name -> Type -> Context
addFormula ctx name ttype = Formula name ttype : ctx

-- addBinding :: Context -> String -> Bind -> Context
-- addBinding ctx x bind = ContextNote x bind : ctx

getTypeFromContext :: Context -> Name -> Either String Type
getTypeFromContext [] _ = Left "Error: Empty Context !!!"
getTypeFromContext ctx name = case filter (\x -> var x == name) ctx of
    [] -> Left "Error: Variable lookup failure !!!"
    l | null $ tail l ->  Right $ varType $ head l
    l -> Left $ "Error: Ambiguous type definition " ++ show l ++ " !!!" 

-- getBinding :: String -> Context -> Int -> Either String Bind
-- getBinding fi ctx i = case ctx !! max 0 i of
--     ContextNote _ bind -> Right bind
--     _ -> Left "Variable lookup failure"

-- getTypeFromContext :: String -> Context -> Int -> Either String Type
-- getTypeFromContext fi ctx i = do
--     varB <- getBinding fi ctx i 
--     return $ ty varB

typeOf :: Context -> Term -> Either String Type
typeOf ctx term = do 
    case term of
        LVar name -> getTypeFromContext ctx name
        LAbs name tyT1 term ->  tyT2 >>= (Right . Arrow tyT1 )
            where
                ctx' = addFormula ctx name tyT1
                tyT2 = typeOf ctx' term
        LApp term1 term2 -> do
                tT1 <- typeOf ctx term1
                tT2 <- typeOf ctx term2
                case tT1 of
                    Arrow type1 type2 ->
                        if type1 == tT2
                        then Right type2
                        else  Left "Error: Parameter type mismatch !!!"
                    _ -> Left "Error: Arrow type expected !!!"

showType :: Type -> String 
showType t = case t of
    Type name -> name
    Arrow type1 type2 -> 
        "(" ++ showType type1 ++ " -> " ++ showType type2 ++ ")"

checkTypeStr :: String -> String
checkTypeStr input  = case parse lambdaParser "" input of
    Left err -> show err
    Right term -> case typeOf [] term of
        Left err -> err
        Right ttype -> showType ttype

check :: String -> IO()
check = putStrLn . checkTypeStr

-- checkType :: String -> IO()
-- checkType input =  case pars input >>= typeOf [] of

    -- case pars input of
    --     Left err -> print err
    --     Right term -> case typeOf [] term of
    --         Left err -> print err
    --         Right ttype -> print $ showType ttype

-- typeOf :: Context -> Term -> Either String Type 
-- typeOf ctx t = case t of
--     TmVar fi i _ -> getTypeFromContext fi ctx i
--     TmAbs fi x tyT1 t2 -> TyArr tyT1 tyT2
--         where
--             ctx' = addBinding ctx x (VarBind tyT1) 
--             tyT2 = typeOf ctx' t2
--     TmApp fi t1 t2 -> case tyT1 of

--         where
--             tyT1 = typeOf ctx t1
--             tyT2 = typeOf ctx t2

-- showTerm :: Context -> Either String Term -> String
-- showTerm ctx e = case e of
--     Left str -> str
--     Right t -> case t of
--         TmAbs fi x t1 ->
--             let
--                 x' = pickFreshName ctx x
--                 ctx' = ctx ++ [ContextNote x' Bind]
--                 in
--                     "(lambda " ++ x' ++ ". " ++ showTerm ctx' (Right t1) ++ ")"
--         TmApp fi t1 t2 ->
--             "(" ++ showTerm ctx (Right t1) ++ " " ++ showTerm ctx (Right t2) ++ ")"
--         TmVar fi x n ->
--             if length ctx == n
--             then indexToName fi ctx x
--             else "[bad index]"
--     where
--         pickFreshName :: Context -> String -> String
--         pickFreshName [] x = x
--         pickFreshName (ContextNote name  bind :notes) x =
--             if name == x
--             then pickFreshName notes (x ++ "'")
--             else pickFreshName notes x

--         indexToName :: Int -> Context -> Int -> String
--         indexToName info ctx value =
--             let reversedCtx = reverse ctx in
--                 name $ reversedCtx !! max 0 value

-- termShift :: Int -> Term -> Term
-- termShift d = walk 0
--     where
--         walk :: Int -> Term -> Term
--         walk c t = case t of
--             TmVar fi x n ->
--                 if x >= c
--                 then TmVar fi (x+d) (n+d)
--                 else TmVar fi x (n+d)
--             TmAbs fi x t1 ->
--                 TmAbs fi x (walk (c+1) t1)
--             TmApp fi t1 t2 ->
--                 TmApp fi (walk c t1) (walk c t2)

-- termSubst :: Int -> Term -> Term -> Term
-- termSubst j s = walk 0
--     where
--         walk :: Int -> Term -> Term
--         walk c t = case t of
--             TmVar fi x n ->
--                 if x == j+c
--                 then termShift c s
--                 else TmVar fi x n
--             TmAbs fi x t1 ->
--                 TmAbs fi x (walk (c+1) t1)
--             TmApp fi t1 t2 ->
--                 TmApp fi (walk c t1) (walk c t2)

-- termSubstTop :: Term -> Term -> Term
-- termSubstTop s t = termShift (-1) (termSubst 0 (termShift 1 s ) t)

-- isVal :: Context -> Term -> Bool
-- isVal ctx t = case t of
--   TmAbs {} -> True
--   _ -> False

-- eval1 :: Context -> Term -> Either String Term
-- eval1 ctx t = case t of
--     TmApp  fi (TmAbs _ x t12) v2 | isVal ctx v2 ->
--         Right $ termSubstTop v2 t12
--     TmApp fi v1 t2 | isVal ctx v1 -> let t2' = eval1 ctx t2 in
--         TmApp fi v1 <$> t2'
--     TmApp fi t1 t2 -> let t1' = eval1 ctx t1 in
--         do
--             t1'' <- t1'
--             return $ TmApp fi t1'' t2
--     _ -> Left "No Rule Applies!!"

-- eval :: Context -> Term -> Either String Term
-- eval ctx t = do
--     t' <- eval1 ctx t
--     eval ctx t'

someFunc :: IO ()
someFunc = putStrLn "someFunc"
