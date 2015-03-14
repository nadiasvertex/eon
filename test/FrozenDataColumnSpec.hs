module FrozenDataColumnSpec where

import           Data.Array.Repa     (fromListUnboxed, ix1)
import           Test.Hspec
--import           Test.Hspec.QuickCheck (prop)
--import           Test.QuickCheck hiding ((.&.))

import           Store.DataColumn
import           Store.FrozenDataColumn


test_frozen_segment_1 :: FrozenSegment Int
test_frozen_segment_1 = FrozenSegment { column =  test_frozen_list_1 }
  where
    test_list_1 = [1,2,3,4,5] :: [Int]
    test_frozen_list_1 = fromListUnboxed (ix1 $ length test_list_1) test_list_1

spec :: Spec
spec = do
  describe "freeze" $ do
    it "freezes a non-empty data column" $
      (snd . freeze $ segmentFromList empty [(1,1), (2,2), (3,3), (4,4), (5,5)]) `shouldBe`
        test_frozen_segment_1
