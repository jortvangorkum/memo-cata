{-# LANGUAGE ScopedTypeVariables #-}
module UnitTests.GenericTreeSpec where
import           Arbitrary
import           Data.Bifunctor           (first)
import qualified Data.Map                 as M
import qualified Data.Trie                as T
import qualified GenericTree.GenericCata  as G
import           GenericTree.Main
import qualified GenericTree.SpecificCata as S
import           Generics.Main
import           Test.Hspec
import           Test.QuickCheck


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
  it "Generic Cata Sum == Specific Cata Sum" $ property $
    \t -> G.cataSum empty t `shouldBe` S.cataSum t
  it "Generic Cata Map Result == Generic Cata Trie Result" $ property $
    \t -> fst (G.cataSum empty t) `shouldBe` fst (G.cataSumTrie empty t)
  it "Generic Cata Map Intermediate Results == Generic Cata Trie Intermediate Results" $ property $
    \t -> M.toList (snd (G.cataSum empty t)) `shouldBe` T.toList (snd (G.cataSumTrie empty t))
  it "Cata Int == Generic Cata Sum" $ property $
    \t -> S.cataInt t `shouldBe` fst (G.cataSum empty (merkle t))
  it "Cata Merkle Int == Generic Cata Sum" $ property $
    \t -> S.cataMerkleInt t `shouldBe` fst (G.cataSum empty (merkle t))
  it "Specific Cata Sum == Specific Cata Sum Map" $ property $
    \t -> S.cataSum t `shouldBe` S.cataSumMap empty t
  it "Merkelized Tree == Merkle Tree" $ property $
    \(t :: TreeG Int, m :: MerkleTree Int) -> merkle t `shouldBe` m
