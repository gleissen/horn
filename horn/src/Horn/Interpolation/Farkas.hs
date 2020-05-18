module Horn.Interpolation.Farkas where
import           Control.Monad.State.Lazy
import           Data.IntMap                    (IntMap)
import qualified Data.IntMap                    as IntMap
import           Data.IntSet                    (IntSet)
import qualified Data.IntSet                    as IntSet
import           Data.Map                       (Map)
import qualified Data.Map                       as Map
import           Data.Matrix                    (Matrix)
import qualified Data.Matrix                    as Mtx
import           Data.Maybe
import           Data.Ratio
import qualified Horn.Logic.Clauses             as C
import           ToySolver.Arith.FourierMotzkin
import           ToySolver.Data.LA

-- a cube is a conjunction of inequalities
type VarMap = Map String Var

--instance Num SymbCoeff where
--  (+), (*), abs, signum, fromInteger, (negate


--------------------------------------------------------
--Conversion: Coefficient Coefficient Lists to Matrices
--------------------------------------------------------

----------------------------------------------------------------------
--atomsToMatrix :: [Atom Rational] -> VarMap -> Matrix Rational
----------------------------------------------------------------------




------------------------------------------------
--Conversion: Expressions to Coefficient Lists
------------------------------------------------

--------------------------------------
baseToAtoms :: VarMap -> C.Base -> [Atom Rational]
--------------------------------------
baseToAtoms vMap (C.Geq e1 e2) = [a1 .<=. a2]
      where
        a1 = fromTerms $ (neg $ normExpToTerm vMap e1)
        a2 = fromTerms $ (neg $ normExpToTerm vMap e2)
baseToAtoms vMap (C.Leq e1 e2) = [a1 .<=. a2]
      where
        a1 = fromTerms $ (normExpToTerm vMap e1)
        a2 = fromTerms $ (normExpToTerm vMap e2)
baseToAtoms vMap (C.Eq e1 e2) = [a1 .>=. a2, a2 .>=. a1]
      where
        a1 = fromTerms $ (normExpToTerm vMap e1)
        a2 = fromTerms $ (normExpToTerm vMap e2)
baseToAtoms vMap (C.And phis) = concat $ map (baseToAtoms vMap) phis

----------------------------------------------
neg :: [(Rational, Var)] -> [(Rational, Var)]
----------------------------------------------
neg rs = map (\(r,v) -> (negate r, v)) rs

--------------------------------------------------
normExpToTerm :: VarMap -> C.Exp -> [(Rational, Var)]
--------------------------------------------------
normExpToTerm vMap (C.Plus es)  = concat $ map (normExpToTerm vMap) es
normExpToTerm vMap (C.Minus es) = concat $ map (neg.(normExpToTerm vMap)) es
normExpToTerm vMap (C.Times [(C.Num n),(C.Var s)]) = [(fromIntegral n, fromJust $ Map.lookup s vMap)]
normExpToTerm vMap (C.Var s)  = [(fromIntegral 1,  fromJust $ Map.lookup s vMap)]
normExpToTerm vMap (C.Num n)  = [(fromIntegral n,  unitVar)]

-----------------------------
mkVarMap :: C.Base -> VarMap
-----------------------------
mkVarMap phi = evalState (mkVarMap_ Map.empty phi) 0

-----------------------------------------------
mkVarMap_ ::  VarMap -> C.Base -> State Int VarMap
-----------------------------------------------
mkVarMap_ vMap (C.Eq e1 e2) = do
   vMap1 <- mkVarMapExp vMap e1
   vMap2 <- mkVarMapExp vMap e1
   return $ Map.union vMap1 vMap2
mkVarMap_ vMap (C.Geq e1 e2) = do
    vMap1 <- mkVarMapExp vMap e1
    vMap2 <- mkVarMapExp vMap e1
    return $ Map.union vMap1 vMap2
mkVarMap_ vMap (C.Leq e1 e2) = do
    vMap1 <- mkVarMapExp vMap e1
    vMap2 <- mkVarMapExp vMap e1
    return $ Map.union vMap1 vMap2
mkVarMap_ vMap (C.Neg phi) = mkVarMap_ vMap phi
mkVarMap_ vMap (C.And phis) = Map.unions <$> mapM (mkVarMap_ vMap) phis
mkVarMap_ vMap (C.Or phis) = Map.unions <$> mapM (mkVarMap_ vMap) phis
mkVarMap_ vMap (C.Implies phi1 phi2) = do
    vMap1 <- mkVarMap_ vMap phi1
    vMap2 <- mkVarMap_ vMap phi2
    return $ Map.union vMap1 vMap2

------------------------------------------------
mkVarMapExp :: VarMap -> C.Exp -> State Int VarMap
------------------------------------------------
mkVarMapExp vMap (C.Var v) =
  case Map.member v vMap of
      True  -> return vMap
      False -> do
        n <- get
        put (n+1)
        return $ Map.insert v n vMap
mkVarMapExp vMap (C.Num n) = return vMap
mkVarMapExp vMap (C.Plus es) = Map.unions  <$> mapM (mkVarMapExp vMap) es
mkVarMapExp vMap (C.Minus es) = Map.unions <$> mapM (mkVarMapExp vMap) es
mkVarMapExp vMap (C.Times es) = Map.unions <$> mapM (mkVarMapExp vMap) es

-- project(x', x >=1, x'=x+1)
test = do
    putStrLn $ "varMap: " ++ (show vMap)
    putStrLn $ "dbg rationals: "
    mapM_ (putStrLn.showAtom) rats
    putStrLn $ "dbg projected on x': "
    mapM_ (putStrLn.showAtom) proj
  where
    phi = C.And [C.Eq (C.Var "x'") (C.Plus [C.Var "x",C.Num 1]), C.Geq (C.Var "x") (C.Num 1)]
    vMap = mkVarMap phi
    rats = baseToAtoms vMap phi
    proj = concat $ map fst (projectN (IntSet.fromList[fromJust $ Map.lookup "x" vMap ]) rats)
    -- -- interpolate [y>=z,x>=y] [z>=x+1]
    -- vars = IntSet.fromList[0,1] -- x=0, x'=1
    -- x_geq_0 = (fromTerms [((1::Rational),0)] ) .>=. (fromTerms [((1::Rational),unitVar)]) :: Atom Rational
    -- xp_eq_x_plus_1 = (fromTerms [((1::Rational),1)] ) .==. (fromTerms [((1::Rational),0),((1::Rational),unitVar)]) :: Atom Rational
    -- rats = [x_geq_0,xp_eq_x_plus_1]
    -- proj = concat $ map fst (projectN (IntSet.fromList[0]) rats)
    -- tst = (fromTerms [(1::Rational,0),(2::Rational,0)]) .>=. (fromTerms [((1::Rational),unitVar)])





--returns phi s.t., a => phi, phi => ¬b and vars(phi)⊆(vars(a) ∪ vars(b))
----------------------------------------------
--interpolate :: Cube -> Cube -> SolveM Cube
----------------------------------------------
--interpolate a b =

-- ----------------------------------------------------
-- interpolate_ :: Cube -> Cube -> SolveM (Maybe Cube)
-- ----------------------------------------------------
--
-- --3x-y>=2
-- -- [3x-y>=2,x-2y>=0, z>=3] -->
-- -- [[3,-1,0],[1,-2,0],[0,0,1], [2,0,3]]
-- -- vars: [x,y,z]
-- --------------------------------
-- eqtoRow :: Base -> Vars -> [Int]
-- --------------------------------
-- eqtoRow
--
--
-- -- [y>=z,x>=y] [z>=x+1]
-- --------------------------------
-- cubeToMatrix :: Cube -> Matrix
-- --------------------------------
-- cubeToMatrix c =
--   where
--     vars = Set.toList $ get_vars (And c)
--
--
--
-- --normalizing
--
-- --------------------------------
-- normalizeCube :: Cube -> Cube
-- --------------------------------
-- normalizeCube c = concat $ map normalizeEq c
--
-- ----------------------------
-- normalizeEq :: Base -> Cube
-- ----------------------------
-- normalizeEq (Eq e1 e2) = [Geq e1 e2, Geq e2 e1]
-- normalizeEq phi = [phi]
