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

-- iterUpdate :: Int -> MerklePF (Tree Int)

spec :: Spec
spec = describe "Incremental Update MerkleTree" $ do
  it "Updated Tree with same value == Original Tree" $ property $
    \(t :: MerklePF (Tree Int)) -> update id [bottom] t `shouldBe` t
  it "Updated Tree with different value != Original Tree" $ property $
    \(t :: MerklePF (Tree Int)) -> update (const mt) [bottom] t `shouldNotBe` t
  it "Size Updated Tree with same value == Size Original Tree" $ property $
    \(t :: MerklePF (Tree Int)) -> merkleTreeSize (update id [bottom] t) `shouldBe` merkleTreeSize t
  it "Size Updated Tree with different value == Size Original Tree" $ property $
    \(t :: MerklePF (Tree Int)) -> merkleTreeSize (update (const mt) [bottom] t) `shouldBe` merkleTreeSize t
  it "Result Updated Tree with same value == Result Original Tree" $ property $
    \(t :: MerklePF (Tree Int)) -> cataSum (update id [bottom] t) `shouldBe` cataSum t
  it "Result Updated Tree with different value != Result Original Tree" $ property $
    \(t :: MerklePF (Tree Int)) -> cataSum (update (const mt) [bottom] t) `shouldNotBe` cataSum t
  it "Multiple update iterations => merkle cata value == cata value" $ property $
    \(n :: Positive Int, t :: MerklePF (Tree Int), ds :: [Direction a]) -> undefined
