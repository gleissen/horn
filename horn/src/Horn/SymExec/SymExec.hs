module Horn.SymExec.SymExec where

import           Control.Monad              (filterM, foldM)
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
---------------------------
pick :: WorkSet -> WorkItem
---------------------------
pick ws = head $ Set.toList ws

----------------------------------------------
post :: Base -> [Var] -> [Var] -> SolveM Base
----------------------------------------------
post phi vars nxvars = do
      nvars <- freshVars (length vars)
      let phi' = substVars nvars vars phi
      return $ substVars vars nxvars phi'

---------------------------------------------
implies :: Base -> Base -> SolveM Bool
---------------------------------------------
implies p q = lift $ Z3.implies p q

---------------------------------------------
pred_abs :: Base -> [Predicate] -> SolveM [Base]
---------------------------------------------
pred_abs phi preds =  filterM (implies phi) preds

-------------------------------------------------------------------------------------
fixpoint_step :: [Predicate] -> Solution -> WorkItem -> Horn a -> SolveM WorkSet
-------------------------------------------------------------------------------------
fixpoint_step preds sol w h = do
  post_ <- post hBd exVs vs
  --lift $ putStrLn $ "\n dbg post " ++ (show post_)
  absPost <- pred_abs post_ preds
  --lift $ putStrLn $ "\n dbg abstract post " ++ (show absPost)
  subsumed <- implies (And absPost) reachStates
  --lift $ putStrLn $ "\n dbg subsumed" ++ (show subsumed)
  case subsumed of
      True  -> return Set.empty
      False -> return $ Set.singleton $ Item {state=(And absPost), predicate=(name $ hd h)}
  where
    sol' = Map.adjust (const $ Set.singleton $ state w) (predicate w) sol
    bdSol = map (plugin sol') (bd h)
    vs = vars $ hd h
    hBd = And [And bdSol, base h]
    exVs = Set.toList $ (get_vars hBd) Set.\\ (Set.fromList $ vs)
    reachStates = Or $ Set.toList $ fromJust $ Map.lookup (predicate w) sol


------------------------------------------------------------------------------
fixpoint :: WorkSet -> Solution -> [Predicate] -> [Horn a]  -> SolveM Solution
------------------------------------------------------------------------------
fixpoint ws sol preds hs = case (Set.size ws) of
    0 -> return sol
    _ -> do
      newWs <- Set.unions <$> mapM (fixpoint_step preds sol w) hs'
      let ws' = Set.union (ws Set.\\ Set.singleton w) newWs
      let sol' = updateSolution newWs sol
      lift $ putStrLn $ "Fixpoint loop: workItems: " ++ (show ws')
      lift $ putStrLn $ "Fixpoint loop: solution: " ++ (show sol')
      fixpoint ws' sol' preds hs
    where
      w    = pick ws
      hs'  = filter (dependsOn (predicate w)) hs

--------------------------------------------------
updateSolution :: WorkSet -> Solution -> Solution
--------------------------------------------------
updateSolution ws sol = foldl update sol (Set.toList ws)
  where
    add st p = Set.union p (Set.singleton $ st)
    update sol w = Map.adjust (add (state w)) (predicate w) sol

---------------------------------------------------------------
initWL_ :: [Predicate] -> Solution -> Horn a -> SolveM (Solution, WorkSet)
---------------------------------------------------------------
initWL_ preds sol h = do
      post_ <- post (base h) exVs vs
      absPost <-  pred_abs post_ preds
      let ws = Set.singleton $ Item {state=(And absPost), predicate=(name $ hd h)}
      let sol' = updateSolution ws sol
      lift $ putStrLn $ "dbg init solution " ++ (show sol')
      lift $ putStrLn $ "dbg init ws " ++ (show ws)
      return (sol', ws)
    where
      vs = vars $ hd h
      exVs = Set.toList $ (get_vars (base h)) Set.\\ (Set.fromList $ vs)

----------------------------------------------------------------------------
initWL :: [Predicate] -> [Horn a] -> SolveM (Solution, WorkSet)
----------------------------------------------------------------------------
initWL preds hs = do
  sols <- mapM (initWL_ preds sol0) hs
  return $ foldl combine (Map.empty, Set.empty) sols
  where
    combine (sol, ws) (sol', ws') = (Map.union sol sol', Set.union ws ws')
    predNames = Set.toList $ Set.unions $ map getPredNames hs
    sol0 = Map.fromList $ zip predNames (repeat (Set.empty))

----------------------------------------------------
solve :: [Horn a] -> [Predicate] -> SolveM Solution
----------------------------------------------------
solve hs preds = do
    (sol0, ws0) <- initWL preds hs'
    fixpoint ws0 sol0 preds hs
  where
    hs' = filter isBase hs

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
    solBd = And $ map (plugin sol) (bd h)
    solHd = plugin sol (hd h)


-------------------------------------------
test :: IO ()
-------------------------------------------
test = do
    --let hs' = (filter isBase hs)
    --(sol,ws) <- evalStateT (initWL preds hs') initState
    --putStrLn $ "Initial Solution and worklist " ++ (show sol) ++ " " ++ (show ws)
     sol <- evalStateT (solve hs preds) initState
     putStr $ "Found solution " ++ (show sol)
    where
      preds = [Eq (Var "x") (Num 0), Eq (Var "x") (Num 1), Geq (Var "x") (Num 0)]
      hs = [Horn {      hd = Pred "h" [Var "x'"]
               ,      bd = [Pred "h" [Var "x"]]
               ,      base = Eq (Var "x'") (Plus [Var "x", Num 1])
               ,      annot=()},
            Horn {      hd = Pred "h" [Var "x"]
                        ,      bd = []
                        ,      base = Eq (Var "x") (Num 0)
                        ,      annot=()
            }
           ]
