module Horn.SymExec.SymExec where

import           Control.Monad.State.Strict
import           Data.Map                   (Map)
import qualified Data.Map                   as Map
import           Data.Set                   (Set)
import qualified Data.Set                   as Set
import           Debug.Trace
import qualified Horn.Bindings.Z3           as Z3
import           Horn.Logic.Clauses
import           Horn.Monad


data WorkItem = Item {state :: Base, pred :: Name}
type WorkSet  = Set WorkItem

-- helper
--pick :: WorkSet -> WorkItem
--pick ws = hd $ Set.toList ws

-- clause p(X') :- p(X), X'=X+1.
-- example post (X=1, X'=X+1, [X], [X']) = X1=1, X=X1+1
----------------------------------------
post :: Base -> [Var] -> [Var] -> SolveM Base
----------------------------------------
post phi vars nxvars = do
      nvars <- freshVars (length vars)
      let phi' = substVars nvars vars phi
      let phi'' = substVars vars nxvars phi'
      return phi''


-- post(x':=x+1, x<=5, x=0, [x]) := (x=1)
test :: IO ()
test = do
  let phi = (Eq (Var "x") (Num 2))
  let nx =  (Eq (Var "x'") (Plus [Var "x", Num 1]))
  phi' <- evalStateT (post (And [phi,nx]) [Var "x"] [Var "x'"]) initState
  putStr $ "QE test: " ++ (show phi')

----------------------------------------------------------------
--nextState :: (Horn a) -> Solution -> WorkItem -> SolveM WorkItem
----------------------------------------------------------------
--nextState h sol w = do
--let bd = And [base h, ]
