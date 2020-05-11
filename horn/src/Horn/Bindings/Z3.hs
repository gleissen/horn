module Horn.Bindings.Z3 where
import           Control.Applicative
import qualified Data.Map            as Map
import           Data.Maybe
import qualified Horn.Logic.Clauses  as Logic
import qualified Z3.Monad            as Z3

-----------------------------------------------------------------
get_model :: Logic.Base -> [Logic.Exp] -> Z3.Z3 (Maybe [Integer])
-----------------------------------------------------------------
get_model phi vs = do
  vars  <- mkVars vs
  let varMap = Map.fromList $ zip vs vars
  phiz3 <- toZ3 varMap phi
  Z3.assert phiz3
  model <- fmap snd $ (Z3.withModel $ \m -> (catMaybes <$> (mapM (Z3.evalInt m) vars)))
  return $ model

----------------------------------------------------------------
toZ3 :: Map.Map Logic.Exp Z3.AST -> Logic.Base -> Z3.Z3 Z3.AST
----------------------------------------------------------------
toZ3 varMap (Logic.Eq e1 e2) = do
      e1' <- toZ3Exp varMap e1
      e2' <- toZ3Exp varMap e2
      Z3.mkEq e1' e2'

toZ3 varMap (Logic.Geq e1 e2) = do
  e1' <- toZ3Exp varMap e1
  e2' <- toZ3Exp varMap e2
  Z3.mkGt e1' e2'

toZ3 varMap (Logic.Neg e) = do
    e' <- toZ3 varMap e
    Z3.mkNot e'

toZ3 varMap (Logic.And es) = do
    es' <- mapM (toZ3 varMap) es
    Z3.mkAnd es'

toZ3 varMap (Logic.Or es) = do
    es' <- mapM (toZ3 varMap) es
    Z3.mkOr es'

toZ3 varMap (Logic.Implies e1 e2) = do
    e1' <- toZ3Exp varMap e1
    e2' <- toZ3Exp varMap e2
    Z3.mkImplies e1' e2'

-----------------------------------------------------------------
toZ3Exp :: Map.Map Logic.Exp Z3.AST -> Logic.Exp ->   Z3.Z3 Z3.AST
-----------------------------------------------------------------
toZ3Exp varMap v@(Logic.Var _) = return $ fromJust $ Map.lookup v varMap

toZ3Exp varMap (Logic.Num n)         = Z3.mkInteger n

toZ3Exp varMap (Logic.Plus es)    = do
      es' <- mapM (toZ3Exp varMap) es
      Z3.mkAdd es'

toZ3Exp varMap (Logic.Minus es)    = do
      es' <- mapM (toZ3Exp varMap) es
      Z3.mkSub es'

toZ3Exp varMap (Logic.Times es)    = do
      es' <- mapM (toZ3Exp varMap) es
      Z3.mkMul es'


mkVar :: Logic.Exp -> Z3.Z3 Z3.AST
mkVar (Logic.Var x) = Z3.mkFreshIntVar x

mkVars :: [Logic.Exp] -> Z3.Z3 [Z3.AST]
mkVars vs = mapM mkVar vs

test ::  Z3.Z3 (Maybe [Integer])
-- x>=y /\ y>=2 -> x>=0
test = do
  x <- Z3.mkFreshIntVar "x"
  y <- Z3.mkFreshIntVar "y"
  _2 <- Z3.mkInteger 2
  _0 <- Z3.mkInteger 0
  a1 <- Z3.mkGe x y
  a2 <- Z3.mkGe y _2
  bd <- Z3.mkAnd [a1, a2]
  hd <- Z3.mkLt x _0
  Z3.assert bd
  Z3.assert hd
  fmap snd $ Z3.withModel $ \m -> catMaybes <$> mapM (Z3.evalInt m) [x,y]
