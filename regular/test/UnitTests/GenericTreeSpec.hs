{-# LANGUAGE ScopedTypeVariables #-}
module UnitTests.GenericTreeSpec where
import           Data.Bifunctor          (first)
import qualified Data.Map                as M
import           GenericTree.Cata
import           GenericTree.Main
import           Generics.Memo.Cata.Main
import           Generics.Memo.Main
import           Helper.Arbitrary
import           Helper.Main
import           Test.Hspec
import           Test.QuickCheck


spec :: Spec
spec = describe "Generic Tree Unit Tests" $ do
  it "Generate Tree has size (2n + 1)" $ property $
    \(Positive n) -> treeSize (generateTree n) `shouldBe` 2 * n + 1
  it "Generic Cata Sum == Specific Cata Sum" $ property $
    \t -> fst (cataSum (merkle t)) `shouldBe` cataSumTree t
  it "Size Generic Cata Sum == Size Specific Cata Sum" $ property $
    \(t :: Tree Int) -> merkleTreeSize (merkle t) `shouldBe` treeSize t
