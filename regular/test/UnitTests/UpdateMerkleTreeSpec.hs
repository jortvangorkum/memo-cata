{-# LANGUAGE ScopedTypeVariables #-}
module UnitTests.UpdateMerkleTreeSpec where

import           GenericTree.Cata
import           GenericTree.Main
import           Generics.Memo.Main
import           Generics.Memo.Zipper
import           Helper.Arbitrary
import           Helper.Main
import           Test.Hspec
import           Test.QuickCheck

mt :: MerklePF (Tree Int)
mt = merkle $ Leaf 69

spec :: Spec
spec = describe "Incremental Update MerkleTree" $ do
  it "Updated Tree with same value == Original Tree" $ property $
    \(t :: MerklePF (Tree Int)) -> update id [down] t `shouldBe` t
  it "Updated Tree with different value != Original Tree" $ property $
    \(t :: MerklePF (Tree Int)) -> update (const mt) [down] t `shouldNotBe` t
  it "Size Updated Tree with same value == Size Original Tree" $ property $
    \(t :: MerklePF (Tree Int)) -> merkleTreeSize (update id [down] t) `shouldBe` merkleTreeSize t
  it "Size Updated Tree with different value == Size Original Tree" $ property $
    \(t :: MerklePF (Tree Int)) -> merkleTreeSize (update (const mt) [down] t) `shouldBe` merkleTreeSize t
  -- it "Result Updated Tree with same value == Result Original Tree" $ property $
  --   \(t :: MerklePF (Tree Int)) -> G.cataSumTrie empty (update id [down] t) `shouldBe` G.cataSumTrie empty t
  -- it "Result Updated Tree with different value != Result Original Tree" $ property $
  --   \(t :: MerklePF (Tree Int)) -> G.cataSumTrie empty (update (const mt) [down] t) `shouldNotBe` G.cataSumTrie empty t
  -- it "With Trie - Result Updated Tree with same value == Result Original Tree" $ property $
  --   \(t :: MerklePF (Tree Int)) -> let (_, tr) = G.cataSumTrie empty t
  --                             in  G.cataSumTrie tr (update id [down] t) `shouldBe` G.cataSumTrie tr t
  -- it "With Trie - Result Updated Tree with different value != Result Original Tree" $ property $
  --   \(t :: MerklePF (Tree Int)) -> let (_, tr) = G.cataSumTrie empty t
  --                             in  G.cataSumTrie tr (update (const mt) [down] t) `shouldNotBe` G.cataSumTrie tr t
  -- it "With Trie - Result Updated Tree with different value == Result Original Tree + 69" $ property $
  --   \(t :: MerklePF (Tree Int)) -> let (_, tr) = G.cataSumTrie empty t
  --                             in  fst (G.cataSumTrie tr (update (const mt) [down] t)) `shouldBe` fst (G.cataSumTrie tr t) + 69
