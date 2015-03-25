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
                                                reservedNames = ["def", "B","let","in","as","Unit","unit","fst","snd","suc","0","R","Nat"],
                                                reservedOpNames = ["\\", ".", "->", "=","(",")",","],
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


  
  -- Supuse útil el uso de estas dos funciones que acortan las definiciones de parsers
  -- ya que éstos se repetían en varias ocasiones. Simplemente capturan un patrón de Parser
  
  -- Captura 1: genP
  -- Parser generico para parsers de constructores de dos argumentos
  -- Toma dos parsers, un string y una función (constructor). Ejecuta p1, lee el string,
  -- ejecuta p2, y devuelve (f t1 t2)
  genP :: Parser a -> Parser b -> String -> (a -> b -> c) -> Parser c
  genP p1 p2 r f = do t1 <- p1
                      reservedOp' r
                      t2 <- p2
                      return (f t1 t2)

  -- Captura 2: genP'
  -- Parser generico para parsers de un constructor de un argumento
  -- Toma un parser, un string, la función constructor y una función que toma un tipo b y devuelve un parser de tipo b. 
  -- Lee el string,
  -- luego ejecuta p y devuelve (f t)
  genP' :: Parser a -> String -> (a -> b) -> (b -> Parser b) -> Parser b
  genP' p r f par = do reserved' r
                       t <- p
                       par (f t)

  bases :: Parser Type
  bases = do reserved' "B"
             return Base
           <|> do reserved' "Unit"
                  return Unit
                <|> do reserved' "Nat"
                       return Nat
                     <|> try (parens' (genP type' type' "," Pair))
                       <|> parens' type'
  
   
  -- parser para términos
  varabs :: Parser LamTerm 
  varabs = as <|> nat <|> try pair <|> unit <|> var <|> abs <|> tlet <|> parens' term
  
  -- varabs', se usa en as
  varabs' :: Parser LamTerm
  varabs' = nat <|> try pair <|> unit <|> var <|> abs <|> tlet <|> parens' term

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

  -- Parser de Let
  tlet :: Parser LamTerm
  tlet = do reserved' "let"
            x <- identifier'
            reservedOp' "="
            t1 <- term
            reserved' "in"
            t2 <- term
            return (MLet x t1 t2)

  -- Parser de As
  -- As es un parser que ejecuta en cadena todos los parsers de varabs', y
  -- si no hay más nada (no habia un "as"), devuelve lo que parseo (t), si
  -- hay un "as" luego, parsea el tipo y devuelve "As t tipo".
  -- Obs: Tuve que agregar varabs', debajo de la definición de varabs.
  as :: Parser LamTerm
  as = do t <- chainl1 varabs' (return App);as' t  

  as' :: LamTerm -> Parser LamTerm
  as' t = genP' type' "as" (As t) as' <|> return t 

  -- Parser de Unit
  unit :: Parser LamTerm
  unit = do t <- reserved' "unit"
            return (LUnit)


  -- Parser de pares (tuplas)
  pair :: Parser LamTerm
  pair = parens' (genP term term "," LPair)
          <|> genP' term "fst" LFst return
               <|> genP' term "snd" LSnd return

  -- Parser de Nats 
  -- Tomo como que el espacio dentro de LRec me separa términos 
  -- (asumo como que LRec es una función y el espacio separa argumentos, los toma entre paréntesis), entonces 
  -- uso varabs'
 
  nat :: Parser LamTerm
  nat = do reserved' "0"
           return LZero
         <|> genP' term "suc" LSuc return
              <|> do reserved' "R"
                     t1 <- varabs'
                     t2 <- varabs'
                     t3 <- varabs'
                     return (LRec t1 t2 t3) 

  -- conversion a términos localmente sin nombres
  conversion :: LamTerm -> Term
  conversion = conversion' []

  conversion' :: [String] -> LamTerm -> Term
  conversion' b LUnit = TUnit
  conversion' b LZero = TZero
  conversion' b (LSuc t) = TSuc (conversion' b t)
  conversion' b (LVar n) = maybe (Free (Global n)) Bound (n `elemIndex` b)
  conversion' b (App t u) = conversion' b t :@: conversion' b u
  conversion' b (Abs n t u) = Lam t (conversion' (n:b) u)
  conversion' b (MLet n t u) = TLet (conversion' b t) (conversion' (n:b) u)
  conversion' b (As t u) = TAs (conversion' b t) u
  conversion' b (LFst t) = TFst (conversion' b t)
  conversion' b (LSnd t) = TSnd (conversion' b t)
  conversion' b (LPair t t') = TPair (conversion' b t) (conversion' b t')  
  conversion' b (LRec t1 t2 t3) = TRec (conversion' b t1) (conversion' b t2) (conversion' b t3)

-----------------------
--- pretty printer
-----------------------

  -- lista de posibles nombres para variables
  vars :: [String]
  vars = [ c : n | n <- "" : map show [1..], c <- ['x','y','z'] ++ ['a'..'w'] ]

  fv :: Term -> [String]
  fv (Bound i) = []
  fv TUnit = []
  fv TZero = []
  fv (Free (Global n)) = [n]
  fv (TSuc t) = fv t
  fv (t :@: u) = fv t ++ fv u
  fv (Lam t u) = fv u
  fv (TLet t u) = fv t ++ fv u
  fv (TAs t u) = fv t
  fv (TFst t) = fv t
  fv (TSnd t) = fv t
  fv (TPair p p') = fv p ++ fv p' 
  fv (TRec t1 t2 t3) = fv t1 ++ fv t2 ++ fv t3  

  -- pretty-printer de términos
  printTerm :: Term -> Doc 
  printTerm t = printTerm' 1 [] (vars \\ fv t) t

  parenIf :: Bool -> Doc -> Doc
  parenIf False d = d
  parenIf True d = PP.parens d
   
  -- Ordenados según la precedencia
  printTerm' :: Int -> [String] -> [String] -> Term -> Doc	
  printTerm' _ bs fs (Bound j) = text $ bs !! j
  printTerm' _ bs fs (Free (Global n)) = text n
  printTerm' _ bs fs TUnit = text "unit"
  printTerm' _ bs fs TZero = text "0"
  printTerm' _ bs fs (TPair t1 t2) = parenIf True $ printTerm' 2 bs fs t1 <> text "," <> printTerm' 1 bs fs t2
  printTerm' i bs (f:fs) (Lam t u) = parenIf (i > 1) $ text "\\" <> text f <> text ":" <> printType t <> text "." <> printTerm' 1 (f:bs) fs u
  printTerm' i bs (f:fs) (TLet t1 t2) = parenIf (i > 1) $ text "let" <+> text f <+> text "=" <+> printTerm' 2 (f:bs) fs t1 <+> text "in" <+> printTerm' 1 (f:bs) fs t2
  printTerm' i bs fs (TFst t) = parenIf (i>2) $ text "fst" <+> printTerm' 1 bs fs t
  printTerm' i bs fs (TSnd t) = parenIf (i>2) $ text "snd" <+> printTerm' 1 bs fs t 
  printTerm' i bs fs (TSuc t) = parenIf (i>2) $ text "suc" <+> printTerm' 1 bs fs t
  printTerm' i bs fs (TRec t1 t2 t3) = parenIf (i>2) $ text "R" <+> printTerm' 5 bs fs t1 <+> printTerm' 5 bs fs t2 <+> printTerm' 1 bs fs t3
  printTerm' i bs fs (t :@: u) = parenIf (i > 3) $ printTerm' 3 bs fs t <+> printTerm' 4 bs fs u
  printTerm' i bs fs (TAs t tt) = parenIf (i > 4) $ printTerm' 3 bs fs t <+> text "as" <+> printType tt
  
  -- pretty-printer de tipos
  printType :: Type -> Doc
  printType = printType' False


  printType' :: Bool -> Type -> Doc
  printType' _ Base = text "B"
  printType' _ Unit = text "Unit"
  printType' _ Nat  = text "Nat"
  printType' b (Pair p1 p2) = parenIf True $ (printType' b p1 <> text "," <> printType' b p2)
  printType' False (Fun t1 t2) = printType' True t1 <+> text "->" <+> printType' False t2
  printType' True t = PP.parens $ printType' False t

-----------------------
--- eval
-----------------------
  -- Definí así los casos así queda más claro
  sub :: Int -> Term -> Term -> Term
  sub _ _ (Free n) = Free n
  sub _ t TZero = TZero
  sub _ t TUnit = TUnit
  sub i t (Bound j) | i == j = t
  sub i t (Bound j) | i /= j = Bound j
  sub i t (u :@: v) = sub i t u :@: sub i t v
  sub i t (Lam t' u) = Lam t' (sub (i+1) t u)
  sub i t (TLet t' u) = TLet (sub i t t') (sub (i+1) t u)
  sub i t (TAs t' u) = TAs (sub i t t') u  
  sub i t (TPair p1 p2) = TPair (sub i t p1) (sub i t p2) 
  sub i t (TFst t') = TFst (sub i t t')
  sub i t (TSnd t') = TSnd (sub i t t')
  sub i t (TSuc t') = TSuc (sub i t t')
  sub i t (TRec t1 t2 t3) = TRec (sub i t t1) (sub i t t2) (sub i t t3)
  

  -- evaluador de términos
  -- Acorté los casos que se repetían y acomodé el orden de los argumentos
  eval :: NameEnv Value Type -> Term -> Value
  eval _ TZero = VNat VZero
  eval _ TUnit = VUnit
  eval e (Free n) = fst $ fromJust $ lookup n e
  eval e (Lam t u) = VLam t u
  eval e (u :@: v) = case eval e u of
                       VLam t u' -> eval e (sub 0 (quote (eval e v)) u')
                       _ -> error "Error de tipo en run-time, verificar type checker"
  eval e (TLet t u) = eval e (sub 0 (quote (eval e t)) u)               
  eval e (TAs t u) = eval e t
  eval e (TPair p1 p2) = VPair (eval e p1) (eval e p2)
  eval e (TFst t) = case eval e t of
                      VPair p1 p2 -> p1
                      _ -> error "Error de tipo en eval, se esperaba un valor VPair."
  eval e (TSnd t) = case eval e t of
                      VPair p1 p2 -> p2
                      _ -> error "Error de tipo en eval, se esperaba un valor VPair." 
  eval e (TSuc t) = case eval e t of
                      VNat n ->  VNat (VSuc n)
                      _ -> error "Error de tipo en eval, verificar type checker"
  eval e (TRec t1 t2 t3) = case eval e t3 of
                             VNat VZero -> eval e t1
                             VNat (VSuc t) -> let x = quote $ nvVal t 
                                              in eval e (t2:@:(TRec t1 t2 x):@:x)
                             _ -> error "Error de tipo en eval, verificar type checker"
                             
----------------------
--- quoting
-----------------------

  quote :: Value -> Term
  quote VUnit = TUnit
  quote (VNat (VZero)) = TZero
  quote (VNat (VSuc t)) = TSuc (quote (VNat t))
  quote (VLam t f) = Lam t f
  quote (VPair p1 p2) = TPair (quote p1) (quote p2)

  -- Transformo un termino en un NValue
  evalnv :: Term -> NValue
  evalnv TZero = VZero
  evalnv (TSuc t) = VSuc (evalnv t)
  
  -- De NValue a Value
  nvVal :: NValue -> Value
  nvVal t = VNat t

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
  
  -- fcn de error que fija un par
  fError :: Type -> String -> Either String Type
  fError t s = err $ "se esperaba "++ s ++ "pero " ++
               render (printType t) ++
               " fue inferido."
  -- Inferidor de tipos
  -- Nada que acotar, salvo que respeto las reglas que tenemos para cada caso
  infer' :: Context -> NameEnv Value Type -> Term -> Either String Type
  infer' _ _ TZero = ret Nat
  infer' _ _ TUnit = ret Unit
  infer' c _ (Bound i) = ret (c !! i)
  infer' _ e (Free n) = case lookup n e of
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
                          infer' (tt:c) e u
  infer' c e (TAs t tu) = infer' c e t >>= \tt ->
                         case tt==tu of
                            True -> ret tt
                            _ -> matchError tu tt                       
  infer' c e (TPair p1 p2) = infer' c e p1 >>= \tp1 ->
                             infer' c e p2 >>= \tp2 ->
                             ret (Pair tp1 tp2) 
  infer' c e (TFst t) = infer' c e t >>= \tt ->
                        case tt of
                          (Pair t1 t2) -> ret t1
                          x -> fError x "Pair a b"
  infer' c e (TSnd t) = infer' c e t >>= \tt ->  
                        case tt of
                          (Pair t1 t2) -> ret t2
                          x -> fError x "Pair a b"
  infer' c e (TSuc t) = infer' c e t >>= \tt -> 
                        case tt of
                          Nat -> ret Nat
                          x -> matchError Nat x
  infer' c e (TRec t1 t2 t3) = infer' c e t1 >>= \tt1 ->
                               infer' c e t2 >>= \tt2 ->
                               infer' c e t3 >>= \tt3 ->
                               case tt2 of
                                 Fun f1 f2 -> if (f1 == tt1)
                                              then if (f2 == (Fun Nat tt1)) then 
                                                      if tt3 == Nat then ret tt1 else matchError tt3 Nat 
                                                   else matchError f2 Nat
                                              else matchError tt1 f1
                                 x -> fError x "Fun a b"                   
