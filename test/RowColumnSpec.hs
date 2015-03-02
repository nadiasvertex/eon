module RowColumnSpec where

import           Data.Map            as Map
import qualified Data.Vector.Unboxed as VU
import           Test.Hspec
--import           Test.Hspec.QuickCheck (prop)
--import           Test.QuickCheck hiding ((.&.))

import           Store.RowColumn

spec :: Spec
spec = do
  describe "append" $ do
    it "appends (10, NULL) to an empty row" $
      (append rc1 1 1 present1 (VU.fromList [10])) `shouldBe`
          RowColumn {rows = Map.fromList [
            (1,[Row {rid = 1, version = 1,
                    columns = VU.fromList [10],
                    present = VU.fromList [True,False]}])
          ]}
    it "appends (20, NULL) as a new version of an existing row" $
      (append rc2 1 2 present1 (VU.fromList [20])) `shouldBe`
          RowColumn {rows = Map.fromList [
            (1,[Row {rid = 1, version = 2,
                     columns = VU.fromList [20],
                     present = VU.fromList [True,False]},
                Row {rid = 1, version = 1,
                     columns = VU.fromList [10],
                     present = VU.fromList [True,False]}])
          ]}

  describe "update" $ do
    it "updates (20, NULL) to (20, 130)" $
      (Store.RowColumn.update rc3 1 2 3 [Just False, Just True] (VU.fromList [130])) `shouldBe`
        RowColumn {rows = Map.fromList [
          (1,[Row {rid = 1, version = 3,
                   columns = VU.fromList [130,20],
                   present = VU.fromList [True,True]},
              Row {rid = 1, version = 2,
                   columns = VU.fromList [20],
                   present = VU.fromList [True,False]},
              Row {rid = 1, version = 1,
                   columns = VU.fromList [10],
                   present = VU.fromList [True,False]}])
        ]}

    it "updates (20, NULL) to (30, NULL)" $
      (Store.RowColumn.update rc3 1 2 3 [Just True, Nothing] (VU.fromList [30])) `shouldBe`
        RowColumn {rows = Map.fromList [
          (1,[Row {rid = 1, version = 3,
                   columns = VU.fromList [30],
                   present = VU.fromList [True,False]},
              Row {rid = 1, version = 2,
                   columns = VU.fromList [20],
                   present = VU.fromList [True,False]},
              Row {rid = 1, version = 1,
                   columns = VU.fromList [10],
                   present = VU.fromList [True,False]}])
        ]}

    it "updates (20, NULL) to (NULL, 130)" $
      (Store.RowColumn.update rc3 1 2 3 [Nothing, Just True] (VU.fromList [130])) `shouldBe`
        RowColumn {rows = Map.fromList [
          (1,[Row {rid = 1, version = 3,
                   columns = VU.fromList [130],
                   present = VU.fromList [False,True]},
              Row {rid = 1, version = 2,
                   columns = VU.fromList [20],
                   present = VU.fromList [True,False]},
              Row {rid = 1, version = 1,
                   columns = VU.fromList [10],
                   present = VU.fromList [True,False]}])
        ]}

  describe "lookup" $ do
    it "finds row id 1 version 1" $
      Store.RowColumn.lookup rc4 1 1 `shouldBe`
        Just (Row {rid = 1, version = 1,
                   columns = VU.fromList [10],
                   present = VU.fromList [True,False]})

    it "finds row id 1 version 2" $
      Store.RowColumn.lookup rc4 1 2 `shouldBe`
        Just (Row {rid = 1, version = 2,
                   columns = VU.fromList [20],
                   present = VU.fromList [True,False]})

    it "finds row id 1 version 3" $
     Store.RowColumn.lookup rc4 1 3 `shouldBe`
       Just (Row {rid = 1, version = 3,
                  columns = VU.fromList [130, 20],
                  present = VU.fromList [True,True]})

    it "fails to find row id 1 version 4" $
     Store.RowColumn.lookup rc4 1 4 `shouldBe` Nothing

    it "fails to find row id 2 version 1" $
     Store.RowColumn.lookup rc4 2 1 `shouldBe` Nothing

  where
    present1 = (VU.fromList [True, False])

    rc1 = RowColumn{rows=Map.empty}
    rc2 = append rc1 1 1 (VU.fromList [True, False]) (VU.fromList [10])
    rc3 = append rc2 1 2 (VU.fromList [True, False]) (VU.fromList [20])
    rc4 = Store.RowColumn.update rc3 1 2 3 [Just False, Just True] (VU.fromList [130])
