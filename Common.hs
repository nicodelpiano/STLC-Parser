module Common where

  -- Comandos interactivos o de archivos
  data Stmt i = Let String i           --  Declarar un nuevo identificador x, let x = t
              | Eval i                 --  Evaluar el término
    deriving (Show)
  
  -- Tipos de los nombres
  data Name
     =  Global  String
     |  Quote   Int
    deriving (Show, Eq)

  -- Entornos
  type NameEnv v t = [(Name, (v, t))]

  -- Tipo de los tipos
  data Type = Base
            | Unit
            | Fun Type Type
            | Pair Type Type
            | Nat
            deriving (Show, Eq)
  
  -- Términos con nombres
  data LamTerm  =  LVar String
                |  Abs String Type LamTerm
                |  App LamTerm LamTerm
                |  MLet String LamTerm LamTerm
                |  As LamTerm Type
                |  LUnit
                |  LPair LamTerm LamTerm
                |  LFst LamTerm
                |  LSnd LamTerm
                |  LZero
                |  LSuc LamTerm
                |  LRec LamTerm LamTerm LamTerm
                deriving (Show, Eq)

  -- Términos localmente sin nombres
  data Term  = Bound Int
             | Free Name 
             | Term :@: Term
             | Lam Type Term
             | TLet Term Term
             | TAs Term Type
             | TUnit
             | TPair Term Term
             | TFst Term
             | TSnd Term
             | TZero
             | TSuc Term
             | TRec Term Term Term
          deriving (Show, Eq)

  -- Valores
  data Value = VLam Type Term 
             | VUnit
             | VPair Value Value
             | VNat NValue
          deriving (Show, Eq)

  data NValue = VZero
              | VSuc NValue
          deriving (Show, Eq)

  -- Contextos del tipado
  type Context = [Type]
