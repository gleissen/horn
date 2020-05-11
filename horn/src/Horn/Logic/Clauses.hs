module Horn.Logic.Clauses where

data Exp =   Var String
            | Num Integer
            | Plus [Exp]
            | Minus [Exp]
            | Times [Exp]
            deriving (Show,Eq,Ord)

data Base =   Eq Exp Exp
            | Geq Exp Exp
            | Neg Base
            | And [Base]
            | Or  [Base]
            | Implies Exp Exp
            deriving (Show,Eq,Ord)

data Pred = Pred String [Exp] deriving (Show,Eq,Ord)

data Horn a =  Horn { hd    :: Pred
               ,      bd    :: [Pred]
               ,      base  :: Base
               ,      annot :: a
               } deriving (Show,Eq,Ord)
