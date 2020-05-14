module Horn.SymExec.SymExec where

import           Control.Monad              (filterM)
import           Control.Monad.State.Strict (evalStateT)
import           Control.Monad.Trans.Class  (lift)
import           Data.Map                   (Map)
import qualified Data.Map                   as Map
import           Data.Maybe
import           Data.Set                   (Set)
import qualified Data.Set                   as Set
import           Debug.Trace
import qualified Horn.Bindings.Z3           as Z3
import           Horn.Logic.Clauses
import           Horn.Monad


data WorkItem = Item {state :: Base, predicate :: Name} deriving (Show,Eq,Ord)
type WorkSet  = Set WorkItem
type Predicate = Base

-- helper
--pick :: WorkSet -> WorkItem
--pick ws = hd $ Set.toList ws

-- clause p(X') :- p(X), X'=X+1.
-- example post (X=1, X'=X+1, [X], [X']) = X1=1, X=X1+1
----------------------------------------
post :: Base -> [Var] -> [Var] -> SolveM Base
----------------------------------------
post phi vars nxvars = do
      --lift $ putStrLn $ "dbg post: " ++ (show phi) ++ " vars " ++ (show vars) ++ " nxvars" ++ (show nxvars)
      nvars <- freshVars (length vars)
      --lift $ putStrLn $ "dbg post: nvars" ++ (show nvars)
      let phi' = substVars nvars vars phi
      --lift $ putStrLn $ "dbg post: phi'" ++ (show phi')
      return $ substVars vars nxvars phi'


---------------------------------------------
implies :: Base -> Base -> SolveM Bool
---------------------------------------------
implies p q = lift $ Z3.implies p q

---------------------------------------------
pred_abs :: Base -> [Base] -> SolveM [Base]
---------------------------------------------
pred_abs phi preds =  filterM (implies phi) preds


-------------------------------------------------------------------------------------
fixpoint_step :: Horn a -> [Predicate] -> Solution -> WorkItem -> SolveM [WorkItem]
-------------------------------------------------------------------------------------
fixpoint_step h preds sol w = do
  post_ <- post hBd exVs vs
  lift $ putStrLn $ "\n dbg post " ++ (show post_)
  absPost <- pred_abs post_ preds
  lift $ putStrLn $ "\n dbg abstract post " ++ (show absPost)
  subsumed <- implies (And absPost) reachStates
  lift $ putStrLn $ "\n dbg subsumed" ++ (show subsumed)
  case subsumed of
      True  -> return []
      False -> return [Item {state=(And absPost), predicate=(name $ hd h)}]
  where
    sol' = Map.adjust (const $ Set.singleton $ state w) (predicate w) sol
    bdSol = map (solve sol') (bd h)
    vs = vars $ hd h
    hBd = And [And bdSol, base h]
    exVs = Set.toList $ (get_vars hBd) Set.\\ (Set.fromList $ vs)
    reachStates = Or $ Set.toList $ fromJust $ Map.lookup (predicate w) sol

--------------------------------------------
check :: Solution -> Horn a -> IO Bool
--------------------------------------------
check sol h = do
  let phi = And [base h, solBd, Neg solHd]
  putStrLn $ "\n dbg solHd " ++ (show solHd)
  model <- Z3.get_model phi (Set.toList $ get_vars phi)
  case model of
      Nothing -> return True
      Just m  -> return False
  where
    solBd = And $ map (solve sol) (bd h)
    solHd = solve sol (hd h)


    -- preds [x=2, x=3, x>=0]
    -- p(x') :- p(x), q(x), x'=x+1.
    -- sol(p(x)) := (x=0;x=1).
-------------------------------------------
test :: IO ()
-------------------------------------------
test = do
    nxWs <- evalStateT (fixpoint_step h preds sol w) initState
    putStr $ "Generated work items " ++ (show nxWs)
    where
      w = Item {state=(Eq (Var "x") (Num 1)), predicate="h"}
      preds = [Eq (Var "x") (Num 2), Eq (Var "x") (Num 3), Geq (Var "x") (Num 0)]
      h = Horn {      hd = Pred "h" [Var "x'"]
               ,      bd = [Pred "h" [Var "x"]]
               ,      base = Eq (Var "x'") (Plus [Var "x", Num 1])
               ,      annot=()
               ,      isProp=False }
      sol = Map.fromList [("h", Set.fromList [Eq (Var "x") (Num 0),Eq (Var "x") (Num 1)])]
