{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module UnitTests.GenericTreeSpec where
import qualified GenericTree.GenericCata  as G
import           GenericTree.Main
import qualified GenericTree.SpecificCata as S
import           Generics.Main
import           Test.Hspec
import           Test.QuickCheck

instance Arbitrary (TreeG Int) where
  arbitrary = do sized (return . generateTreeG)

instance Arbitrary (MerkleTree Int) where
  arbitrary = merkle <$> arbitrary

treeSize :: TreeG a -> Int
treeSize (In x) = case x of
  Inl k -> 1
  Inr (Pair (Pair (I l, _), I r)) -> 1 + lx + rx
    where
      lx = treeSize l
      rx = treeSize r

spec :: Spec
spec = describe "Generic Tree Unit Tests" $ do
  it "Generate Tree has size (2n + 1)" $ property $
    \(Positive n) -> treeSize (generateTreeG n) `shouldBe` 2 * n + 1
  it "Generic Cata Sum equals Specific Cata Sum" $ property $
    \t -> G.cataSum empty t `shouldBe` S.cataSum t
  it "Generic Cata Map Result equals Generic Cata Trie Result" $ property $
    \t -> fst (G.cataSum empty t) `shouldBe` fst (G.cataSumTrie empty t)
