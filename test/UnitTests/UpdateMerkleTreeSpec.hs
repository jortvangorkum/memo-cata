{-# LANGUAGE ScopedTypeVariables #-}
module UnitTests.UpdateMerkleTreeSpec where

import           Arbitrary
import           GenericTree.Main
import           Generics.Data.Digest.CRC32
import           Generics.Main
import           Test.Hspec
import           Test.QuickCheck
import           Zipper.MerkleTree

mt :: MerkleTree Int
mt = merkle (In (Inl (K 69)))

getRootHash :: MerkleTree Int -> Digest
getRootHash (In (Pair (_, K h))) = h

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
