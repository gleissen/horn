{-# LANGUAGE UnicodeSyntax #-}
module Horn.Logic.Clauses where
import           Data.List   (intercalate)
import           Data.Map    (Map)
import qualified Data.Map    as Map
import           Data.Maybe
import           Data.Set    (Set)
import qualified Data.Set    as Set
import           Debug.Trace

data Exp =   Var String
            | Num Integer
            | Plus [Exp]
            | Minus [Exp]
            | Times [Exp]
            deriving (Eq,Ord)

data Base =   Eq Exp Exp
            | Geq Exp Exp
            | Neg Base
            | And [Base]
            | Or  [Base]
            | Implies Base Base
            deriving (Eq,Ord)

type Name = String
type Var = Exp
data Pred = Pred { name :: Name, vars :: [Var]} deriving (Eq,Ord)

data Horn a =  Horn { hd    :: Pred
               ,      bd    :: [Pred]
               ,      base  :: Base
               ,      annot :: a
               } deriving (Eq,Ord)

-- Solutions map each predicate names to a disjunction (set) of base formulas
type Solution = Map Name (Set Base)


-- pretty printing
instance Show Base where
  show (Eq e1 e2)      = (show e1) ++ "=" ++ (show e2)
  show (Geq e1 e2)     = (show e1) ++ "≥" ++ (show e2)
  show (Neg e)         = "¬" ++ (show e)
  show (And es)        = intercalate "∧" (map show es)
  show (Or es)         = intercalate "∨" (map show es)
  show (Implies e1 e2) = (show e1) ++ "⇒" ++ (show e1)

instance Show Exp where
  show (Var s)    = s
  show (Num n)    = (show n)
  show (Plus es)  = intercalate "+" (map show es)
  show (Minus es) = intercalate "-" (map show es)
  show (Times es) = intercalate "*" (map show es)

instance Show (Horn a) where
  show h = (show $ hd h) ++ " :- " ++ bd_ ++ "∧" ++ show (base h) ++ "."
    where
      bd_ = intercalate "∧" (map show $ bd h)

instance Show Pred where
  show p = (show $ name p) ++ "(" ++ vars_ ++ ")"
    where
      vars_ = intercalate "," (map show $ vars p)


-- Helper functions
-------------------------------------
getPredNames :: Horn a -> Set Name
-------------------------------------
getPredNames h = Set.fromList $ map name ([hd h] ++ (bd h))


-------------------------------------------------
dependsOn :: Name -> Horn a -> Bool
-------------------------------------------------
dependsOn p h = or $ map (((==) p).name) (bd h)

------------------------
isBase :: Horn a -> Bool
------------------------
isBase h = (bd h) == []


------------------------------------------
plugin :: Solution -> Pred -> Base
------------------------------------------
plugin sol (Pred p vs) =  substVars vs solVs pSol
  where pSol = Or $ Set.toList $ fromJust $ Map.lookup p sol
        solVs = Set.toList $ get_vars pSol

---------------------------
get_vars :: Base -> Set Exp
---------------------------
get_vars (Eq e1 e2)      = Set.union  (get_vars_exp e1) (get_vars_exp e2)
get_vars (Geq e1 e2)     = Set.union (get_vars_exp e1) (get_vars_exp e2)
get_vars (Neg e)         = get_vars e
get_vars (And es)        = Set.unions $ map get_vars es
get_vars (Or es)         = Set.unions $ map get_vars es
get_vars (Implies e1 e2) = Set.union (get_vars e1) (get_vars e2)

------------------------------
get_vars_exp :: Exp -> Set Exp
------------------------------
get_vars_exp (Var s)    = Set.singleton (Var s)
get_vars_exp (Num n)    = Set.empty
get_vars_exp (Plus es)  = Set.unions $ map get_vars_exp es
get_vars_exp (Minus es) = Set.unions $ map get_vars_exp es
get_vars_exp (Times es) = Set.unions $ map get_vars_exp es


----------------------------------------
substVars :: [Var] -> [Var] -> Base ->  Base
----------------------------------------
substVars vs' vs phi = foldl ((flip.uncurry) subst) phi (zip vs' vs)

-- subst phi y x = phi[y/x]
-------------------------------------
subst :: Var -> Var -> Base ->  Base
-------------------------------------
subst  y x (Eq e1 e2)      =  Eq  (subst_exp y x e1) (subst_exp y x e2)
subst  y x (Geq e1 e2)     =  Geq  (subst_exp y x e1) (subst_exp y x e2)
subst  y x (Neg e)         =  Neg (subst y x e)
subst  y x (And es)        =  And $ map (subst y x) es
subst  y x (Or es)         =  Or  $ map (subst y x) es
subst  y x (Implies e1 e2) = Implies (subst y x e1) (subst y x e2)

---------------------------------------
subst_exp :: Var -> Var -> Exp ->  Exp
---------------------------------------
subst_exp (Var y) (Var x) (Var x')
      | x==x'            = Var y
      | otherwise        = Var x'
subst_exp y x (Num n)    = Num n
subst_exp y x (Plus es)  = Plus $ map (subst_exp y x) es
subst_exp y x (Minus es) = Minus $ map (subst_exp y x) es
subst_exp y x (Times es) = Times $ map (subst_exp y x) es
