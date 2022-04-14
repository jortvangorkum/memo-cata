{-# LANGUAGE ScopedTypeVariables #-}
module UnitTests.UpdateMerkleTreeSpec where

import qualified Data.Map                   as M
import           GenericTree.Cata
import           GenericTree.Main
import           Generics.Data.Digest.CRC32
import           Generics.Memo.Main
import           Generics.Memo.Zipper
import           Helper.Arbitrary
import           Helper.Main
import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Monadic

mt :: MerklePF (Tree Int)
mt = merkle $ Leaf 69

spec :: Spec
spec = describe "Incremental Update MerkleTree" $ do
  it "Updated Tree with same value == Original Tree" $ property $
    \(t :: MerklePF (Tree Int)) -> update id [Bttm] t `shouldBe` t
  it "Updated Tree with different value != Original Tree" $ property $
    \(t :: MerklePF (Tree Int)) -> update (const mt) [Bttm] t `shouldNotBe` t
  it "Size Updated Tree with same value == Size Original Tree" $ property $
    \(t :: MerklePF (Tree Int)) -> merkleTreeSize (update id [Bttm] t) `shouldBe` merkleTreeSize t
  it "Size Updated Tree with different value == Size Original Tree" $ property $
    \(t :: MerklePF (Tree Int)) -> merkleTreeSize (update (const mt) [Bttm] t) `shouldBe` merkleTreeSize t
  it "Result Updated Tree with same value == Result Original Tree" $ property $
    \(t :: MerklePF (Tree Int)) -> cataSum (update id [Bttm] t) `shouldBe` cataSum t
  it "Result Updated Tree with different value != Result Original Tree" $ property $
    \(t :: MerklePF (Tree Int)) -> cataSum (update (const mt) [Bttm] t) `shouldNotBe` cataSum t
  it "Multiple update iterations => merkle cata value == cata value" $ property $
    \((Positive n) :: Positive Int) -> forAll (cataIter n) (\((x, _), t) -> x `shouldBe` cataInt t)
