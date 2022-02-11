{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module TypeChecker
    (
        checkTypeStr
    ) where
import Parser
    ( Term(LVar, LAbs, LApp), Type(Arrow, Type), lambdaParser )
import Text.Parsec ( parse )

type Name = String

data Formula = Formula { var :: Name, varType :: Type } deriving Show

type Context = [Formula] 

addFormula :: Context -> Name -> Type -> Context
addFormula ctx name ttype = Formula name ttype : ctx

showType :: Type -> String 
showType t = case t of
    Type name -> name
    Arrow type1 type2 -> 
        "(" ++ showType type1 ++ " -> " ++ showType type2 ++ ")"

showFormula :: Formula -> String 
showFormula (Formula name ttype) = name ++ " : " ++ showType ttype 

getTypeFromContext :: Context -> Name -> Either String Type
getTypeFromContext [] name = Left "Error: Empty context !!!"
getTypeFromContext ctx name = case filter (\x -> var x == name) ctx of
    [] -> Left $ "Error: Variable lookup failure: " ++ show name ++ " !!!"
    l | null $ tail l ->  Right $ varType $ head l
    l -> Left $ "Error: Ambiguous type binding " ++ show (map showFormula l) ++ " !!!" 

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
                        else  Left $ "Error: Parameter type mismatch. " 
                                     ++ "Expected " ++ show (showType type1)
                                     ++ ", but found " ++ show (showType tT2) ++ " !!!"
                    _ -> Left $ "Error: Arrow type expected, but " ++ show (showType tT1) ++ " found !!!"


checkTypeStr :: String -> [String]
checkTypeStr input  = case parse lambdaParser "" input of
    Left err -> [show err]
    Right terms -> map typeCheck terms
        where
            typeCheck :: Term -> String
            typeCheck term = case typeOf [] term of
                Left err -> err
                Right ttype -> showType ttype