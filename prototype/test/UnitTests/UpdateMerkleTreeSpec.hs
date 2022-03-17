{-# LANGUAGE ScopedTypeVariables #-}
module UnitTests.UpdateMerkleTreeSpec where

import           Generics.Main
import qualified GenericTree.GenericCata as G
import           GenericTree.Main
import           GenericTree.Zipper
import           Test.Arbitrary
import           Test.Helper
import           Test.Hspec
import           Test.QuickCheck

mt :: MerkleTree Int
mt = merkle (In (Inl (K 69)))

spec :: Spec
spec = describe "Incremental Update MerkleTree" $ do
  it "Updated Tree with same value == Original Tree" $ property $
    \(t :: MerkleTree Int) -> update id [down] t `shouldBe` t
  it "Updated Tree with different value != Original Tree" $ property $
    \(t :: MerkleTree Int) -> update (const mt) [down] t `shouldNotBe` t
  it "Updated Tree hash with same value == Original Tree hash" $ property $
    \(t :: MerkleTree Int) -> getRootHash (update id [down] t) `shouldBe` getRootHash t
  it "Updated Tree hash with different value != Original Tree hash" $ property $
    \(t :: MerkleTree Int) -> getRootHash (update (const mt) [down] t) `shouldNotBe` getRootHash t
  it "Size Updated Tree with same value == Size Original Tree" $ property $
    \(t :: MerkleTree Int) -> merkleTreeSize (update id [down] t) `shouldBe` merkleTreeSize t
  it "Size Updated Tree with different value == Size Original Tree" $ property $
    \(t :: MerkleTree Int) -> merkleTreeSize (update (const mt) [down] t) `shouldBe` merkleTreeSize t
  it "Result Updated Tree with same value == Result Original Tree" $ property $
    \(t :: MerkleTree Int) -> G.cataSumTrie empty (update id [down] t) `shouldBe` G.cataSumTrie empty t
  it "Result Updated Tree with different value != Result Original Tree" $ property $
    \(t :: MerkleTree Int) -> G.cataSumTrie empty (update (const mt) [down] t) `shouldNotBe` G.cataSumTrie empty t
  it "With Trie - Result Updated Tree with same value == Result Original Tree" $ property $
    \(t :: MerkleTree Int) -> let (_, tr) = G.cataSumTrie empty t
                              in  G.cataSumTrie tr (update id [down] t) `shouldBe` G.cataSumTrie tr t
  it "With Trie - Result Updated Tree with different value != Result Original Tree" $ property $
    \(t :: MerkleTree Int) -> let (_, tr) = G.cataSumTrie empty t
                              in  G.cataSumTrie tr (update (const mt) [down] t) `shouldNotBe` G.cataSumTrie tr t
  it "With Trie - Result Updated Tree with different value == Result Original Tree + 69" $ property $
    \(t :: MerkleTree Int) -> let (_, tr) = G.cataSumTrie empty t
                              in  fst (G.cataSumTrie tr (update (const mt) [down] t)) `shouldBe` fst (G.cataSumTrie tr t) + 69
