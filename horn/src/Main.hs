module Main where
import qualified Horn.Bindings.Z3          as HZ3
import qualified Horn.Fixpoint.Fixpoint    as Fix
import qualified Horn.Interpolation.Farkas as Fark
import qualified Horn.Logic.Clauses        as HC
import           Z3.Monad

main :: IO ()
main = do
    --Fix.test
    Fark.test
