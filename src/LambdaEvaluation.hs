{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module LambdaEvaluation
    ( 
        someFunc,
        eval,
        showTerm,
        Term,
        Bind,
        ContextNote
    ) where
import Numeric (showEFloat)

data Term =
    TmVar { info :: Int, value :: Int, contextLength :: Int }
    | TmAbs { info :: Int, nameHint :: String, term :: Term }
    | TmApp { info :: Int, term1 :: Term, term2 :: Term }

data Bind = Bind

data ContextNote = ContextNote { name :: String, bind :: Bind }

type Context = [ContextNote]

showTerm :: Context -> Either String Term -> String
showTerm ctx e = case e of
    Left str -> str
    Right t -> case t of
        TmAbs fi x t1 ->
            let
                x' = pickFreshName ctx x
                ctx' = ctx ++ [ContextNote x' Bind]
                in
                    "(lambda " ++ x' ++ ". " ++ showTerm ctx' (Right t1) ++ ")"
        TmApp fi t1 t2 ->
            "(" ++ showTerm ctx (Right t1) ++ " " ++ showTerm ctx (Right t2) ++ ")"
        TmVar fi x n ->
            if length ctx == n
            then indexToName fi ctx x
            else "[bad index]"
    where
        pickFreshName :: Context -> String -> String
        pickFreshName [] x = x
        pickFreshName (ContextNote name  bind :notes) x =
            if name == x
            then pickFreshName notes (x ++ "'")
            else pickFreshName notes x

        indexToName :: Int -> Context -> Int -> String
        indexToName info ctx value =
            let reversedCtx = reverse ctx in
                name $ reversedCtx !! max 0 value

termShift :: Int -> Term -> Term
termShift d = walk 0
    where
        walk :: Int -> Term -> Term
        walk c t = case t of
            TmVar fi x n ->
                if x >= c
                then TmVar fi (x+d) (n+d)
                else TmVar fi x (n+d)
            TmAbs fi x t1 ->
                TmAbs fi x (walk (c+1) t1)
            TmApp fi t1 t2 ->
                TmApp fi (walk c t1) (walk c t2)

termSubst :: Int -> Term -> Term -> Term
termSubst j s = walk 0
    where
        walk :: Int -> Term -> Term
        walk c t = case t of
            TmVar fi x n ->
                if x == j+c
                then termShift c s
                else TmVar fi x n
            TmAbs fi x t1 ->
                TmAbs fi x (walk (c+1) t1)
            TmApp fi t1 t2 ->
                TmApp fi (walk c t1) (walk c t2)

termSubstTop :: Term -> Term -> Term
termSubstTop s t = termShift (-1) (termSubst 0 (termShift 1 s ) t)

isVal :: Context -> Term -> Bool
isVal ctx t = case t of
  TmAbs {} -> True
  _ -> False

eval1 :: Context -> Term -> Either String Term
eval1 ctx t = case t of
    TmApp  fi (TmAbs _ x t12) v2 | isVal ctx v2 ->
        Right $ termSubstTop v2 t12
    TmApp fi v1 t2 | isVal ctx v1 -> let t2' = eval1 ctx t2 in 
        TmApp fi v1 <$> t2'
    TmApp fi t1 t2 -> let t1' = eval1 ctx t1 in
        do 
            t1'' <- t1'
            return $ TmApp fi t1'' t2
    _ -> Left "No Rule Applies!!"

eval :: Context -> Term -> Either String Term
eval ctx t = do
    t' <- eval1 ctx t
    eval ctx t'

someFunc :: IO ()
someFunc = putStrLn "someFunc"
