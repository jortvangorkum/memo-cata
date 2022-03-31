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

iterUpdate :: Int -> Gen (MerklePF (Tree Int))
iterUpdate 0 = do (t :: MerklePF (Tree Int))  <- arbitrary
                  (rt :: MerklePF (Tree Int)) <- arbitrary
                  (ds :: Dirs)                <- arbitrary
                  return (update' (const rt) ds t)
iterUpdate n = do prevT <- iterUpdate (n - 1)
                  (rt :: MerklePF (Tree Int)) <- arbitrary
                  (ds :: Dirs)                <- arbitrary
                  return (update' (const rt) ds prevT)

iterCata :: Int -> Gen ((Int, M.Map Digest Int), MerklePF (Tree Int))
iterCata 0 = do (t :: MerklePF (Tree Int)) <- arbitrary
                (rt :: MerklePF (Tree Int)) <- arbitrary
                (ds :: Dirs)                <- arbitrary
                let t' = update' (const rt) ds t
                return (cataSum t', t')
iterCata n = do ((_, m), t) <- iterCata (n - 1)
                (rt :: MerklePF (Tree Int)) <- arbitrary
                (ds :: Dirs)                <- arbitrary
                let t' = update' (const rt) ds t
                return (cataSumMap m t', t')

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
    \((Positive n) :: Positive Int) -> forAll (iterCata n) (\((n, _), t) -> n `shouldBe` cataInt t)
