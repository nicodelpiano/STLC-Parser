{-# OPTIONS -fno-warn-overlapping-patterns #-}
module Simplytyped(parseStmt, eval, printType, printTerm, quote, infer, term, conversion, simplytyped) where

  import Control.Monad hiding ((>>=))
  import Data.List
  import Data.Maybe
  import Prelude hiding (abs, (>>=))
  import Text.ParserCombinators.Parsec.Language
  import Text.ParserCombinators.Parsec hiding (parse)
  import qualified Text.ParserCombinators.Parsec as P
  import Text.ParserCombinators.Parsec.Token
  import Text.PrettyPrint.HughesPJ hiding (parens)
  import qualified Text.PrettyPrint.HughesPJ as PP 

  import Common

-----------------------
--- parsers
-----------------------

  -- analizador de Tokens
  simplytyped :: TokenParser u
  simplytyped = makeTokenParser (haskellStyle { identStart = letter <|> P.char '_' <|> digit,
                                                reservedNames = ["def", "B","let","in","as","U","unit"],
                                                reservedOpNames = ["\\", ".", "->", "="],
                                                opStart = oneOf "\\-.:=",
                                                opLetter = oneOf ">"
                                              })

  identifier' = identifier simplytyped
  reserved' = reserved simplytyped
  reservedOp' = reservedOp simplytyped
  parens' = parens simplytyped
 
  -- parser para comandos
  parseStmt :: Parser (Stmt Term)
  parseStmt =
        do
          reserved' "def"
          x <- identifier'
          reserved' "="
          t <- term
          return (Let x (conversion t))
    <|> fmap (Eval . conversion) term

  -- parser para tipos
  type' :: Parser Type
  type' = chainr1 bases $ do reservedOp' "->"
                             return Fun

  bases :: Parser Type
  bases = do reserved' "B"
             return Base
           <|> do reserved' "U"
                  return Unit
                <|> parens' type'

  -- parser para términos
  varabs :: Parser LamTerm
  varabs = try (as unit) <|> unit <|> try (as var) <|> var <|> try (as abs) <|> abs <|> try (as tlet) <|> tlet <|> try (as (parens' term)) <|> parens' term
  --varabs = var <|> abs <|> tlet <|> parens' term

  var :: Parser LamTerm
  var = do id <- identifier'
           return (LVar id)

  abs :: Parser LamTerm
  abs = do reservedOp' "\\"
           id <- identifier'
           reservedOp' ":"
           t <- type'
           reservedOp' "."
           u <- term
           return (Abs id t u)

  term :: Parser LamTerm
  term = chainl1 varabs (return App)

  tlet :: Parser LamTerm
  tlet = do reserved' "let"
            x <- identifier'
            reservedOp' "="
            t1 <- term
            reserved' "in"
            t2 <- term
            return (MLet x t1 t2)

  as :: Parser LamTerm -> Parser LamTerm
  as p = do t <- p
            reserved' "as"
            tipo <- type'
            return (As t tipo) 

  unit :: Parser LamTerm
  unit = do t <- reserved' "unit"
            return (LUnit)


  -- conversion a términos localmente sin nombres
  conversion :: LamTerm -> Term
  conversion = conversion' []

  conversion' :: [String] -> LamTerm -> Term
  conversion' b (LVar n) = maybe (Free (Global n)) Bound (n `elemIndex` b)
  conversion' b (App t u) = conversion' b t :@: conversion' b u
  conversion' b (Abs n t u) = Lam t (conversion' (n:b) u)
  conversion' b (MLet n t u) = TLet (conversion' b t) (conversion' (n:b) u)
  conversion' b (As t u) = TAs (conversion' b t) u
  conversion' b (LUnit) = TUnit

-----------------------
--- pretty printer
-----------------------

  -- lista de posibles nombres para variables
  vars :: [String]
  vars = [ c : n | n <- "" : map show [1..], c <- ['x','y','z'] ++ ['a'..'w'] ]

  fv :: Term -> [String]
  fv (Bound i) = []
  fv (Free (Global n)) = [n]
  fv (t :@: u) = fv t ++ fv u
  fv (Lam t u) = fv u
  fv (TLet t u) = fv t ++ fv u
  fv (TAs t u) = fv t

  -- pretty-printer de términos
  printTerm :: Term -> Doc 
  printTerm t = printTerm' 1 [] (vars \\ fv t) t

  parenIf :: Bool -> Doc -> Doc
  parenIf False d = d
  parenIf True d = PP.parens d

  printTerm' :: Int -> [String] -> [String] -> Term -> Doc	
  printTerm' i bs fs (Bound j) = text $ bs !! j
  printTerm' i bs fs (Free (Global n)) = text n
  printTerm' i bs fs (t :@: u) = parenIf (i > 2) $ printTerm' 2 bs fs t <+> printTerm' 3 bs fs u
  printTerm' i bs (f:fs) (Lam t u) = parenIf (i > 1) $ text "\\" <> text f <> text ":" <> printType t <> text "." <> printTerm' 1 (f:bs) fs u
  printTerm' i bs (f:fs) (TLet t u) = parenIf (i > 1) $ text "let " <> text f <> text " = " <> printTerm' 2 (f:bs) fs t <> text " in " <> printTerm' 1 (f:bs) fs u
  --printTerm' i bs (f:fs) (As t u) = parenIf (i > 1) $ text "as " <> text f


  -- pretty-printer de tipos
  printType :: Type -> Doc
  printType = printType' False

  printType' :: Bool -> Type -> Doc
  printType' b Base = text "B"
  printType' b Unit = text "U"
  printType' False (Fun t1 t2) = printType' True t1 <+> text "->" <+> printType' False t2
  printType' True t = PP.parens $ printType' False t

-----------------------
--- eval
-----------------------

  sub :: Int -> Term -> Term -> Term
  sub i t (Bound j) | i == j = t
  sub i t (Bound j) | i /= j = Bound j
  sub i t (Free n) = Free n
  sub i t (u :@: v) = sub i t u :@: sub i t v
  sub i t (Lam t' u) = Lam t' (sub (i+1) t u)
  sub i t (TLet t' u) = TLet (sub (i+1) t t') (sub (i+1) t u)
  sub i t (TAs t' u) = TAs (sub (i+1) t t') u
  sub i t TUnit = TUnit 

  -- evaluador de términos
  eval :: NameEnv Value Type -> Term -> Value
  eval e (Free n) = fst $ fromJust $ lookup n e
  eval e (TUnit) = VUnit
  eval e (Lam t u) = VLam t u
  eval e (Lam t u :@: v) = case eval e v of
                             VLam t' u' -> eval e (sub 0 (Lam t' u') u)
                             VUnit -> eval e (sub 0 TUnit u)
                             _ -> error "Error de tipo en run-time, verificar type checker"
  eval e (u :@: v) = case eval e u of
                       VLam t u' -> eval e (Lam t u' :@: v)
                       _ -> error "Error de tipo en run-time, verificar type checker"
  eval e (TLet t u) = case eval e t of
                        VLam t' u' -> eval e (sub 0 (Lam t' u') u)
                        VUnit -> eval e (sub 0 TUnit u)
                        _ -> error "Error de tipo en run-time, verificar type checker"
  eval e (TAs t u) = eval e t

----------------------
--- quoting
-----------------------

  quote :: Value -> Term
  quote (VLam t f) = Lam t f

-----------------------
--- type checker
-----------------------

  -- type checker
  infer :: NameEnv Value Type -> Term -> Either String Type
  infer = infer' []

  -- definiciones auxiliares
  ret :: Type -> Either String Type
  ret = Right

  err :: String -> Either String Type
  err = Left

  (>>=) :: Either String Type -> (Type -> Either String Type) -> Either String Type
  (>>=) v f = either Left f v

  -- fcs. de error
  matchError :: Type -> Type -> Either String Type
  matchError t1 t2 = err $ "se esperaba " ++
                           render (printType t1) ++
                           ", pero " ++
                           render (printType t2) ++
                           " fue inferido."

  notfunError :: Type -> Either String Type
  notfunError t1 = err $ render (printType t1) ++ " no puede ser aplicado."

  notfoundError :: Name -> Either String Type
  notfoundError n = err $ show n ++ " no está definida."

  infer' :: Context -> NameEnv Value Type -> Term -> Either String Type
  infer' c e (Bound i) = ret (c !! i)
  infer' c e (Free n) = case lookup n e of
                          Nothing -> notfoundError n
                          Just (v,t) -> ret t
  infer' c e (t :@: u) = infer' c e t >>= \tt -> 
                         infer' c e u >>= \tu ->
                         case tt of
                           Fun t1 t2 -> if (tu == t1) 
                                          then ret t2
                                          else matchError t1 tu
                           _ -> notfunError tt
  infer' c e (Lam t u) = infer' (t:c) e u >>= \tu ->
                         ret $ Fun t tu
  infer' c e (TLet t u) = infer' c e t >>= \tt ->
                          infer' (tt:c) e t
  infer' c e (TAs t u) = infer' c e t >>= \tt ->
                         case tt==u of
                            True -> ret tt
                            _ -> matchError u tt
  infer' c e TUnit = ret Unit                       
{-case infer' c e t of
                             Right a -> infer' (a:c) e u
                             _ -> error "error!." 
-}
                     
                          
