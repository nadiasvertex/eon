module FrozenDataColumnSpec where

import qualified Data.Vector.Unboxed as VU
import           Test.Hspec
--import           Test.Hspec.QuickCheck (prop)
--import           Test.QuickCheck hiding ((.&.))

import           Store.DataColumn
import           Store.FrozenDataColumn

spec :: Spec
spec = do
  describe "freeze" $ do
    it "freezes a non-empty data column" $
