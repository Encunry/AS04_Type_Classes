{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where
import Test.Hspec ( hspec, describe, it, shouldBe )
import Data.Monoid
import Map.TreeMap
import Prelude hiding (lookup)

main :: IO ()
main = hspec $ describe "TreeMap" $ do

  describe "empty / toList" $ do
    it "toList empty == []" $
      toList (empty :: Map Char Int) `shouldBe` []
    it "toList (insert 'a' 12 empty) == [('a',12)]" $
      toList (insert 'a' 12 empty) `shouldBe` [('a', 12 :: Int)]

  describe "insert" $ do
    it "keeps entries sorted by key regardless of insertion order" $
      toList (insert 2 "b" (insert 3 "c" (insert 1 "a" empty)))
        `shouldBe` [(1,"a"),(2,"b"),(3,"c")]
    it "replaces the value of an existing key" $
      toList (insert 'a' (2 :: Int) (insert 'a' 1 empty))
        `shouldBe` [('a', 2)]
    it "keeps all other entries when replacing deep in the tree" $
      toList (insert 1 "neu" (insert 3 "c" (insert 2 "b" (insert 1 "a" empty))))
        `shouldBe` [(1,"neu"),(2,"b"),(3,"c")]
    it "keeps all other entries when inserting into a non-empty tree" $
      toList (insert 4 'd' (fromList [(2,'b'),(1,'a'),(3,'c')]))
        `shouldBe` [(1,'a'),(2,'b'),(3,'c'),(4,'d')]

  describe "lookup" $ do
    it "lookup 'a' (insert 'a' 12 empty) == Just 12" $
      lookup 'a' (insert 'a' 12 empty) `shouldBe` Just (12 :: Int)
    it "lookup 'b' (insert 'a' 12 empty) == Nothing" $
      lookup 'b' (insert 'a' (12 :: Int) empty) `shouldBe` Nothing
    it "lookup in empty map == Nothing" $
      lookup 'a' (empty :: Map Char Int) `shouldBe` Nothing
    it "finds every key in a larger tree" $ do
      let m = fromList [(k, show k) | k <- [5,2,8,1,9,3,7 :: Int]]
      map (`lookup` m) [1,2,3,5,7,8,9] `shouldBe` map (Just . show) [1,2,3,5,7,8,9 :: Int]
    it "lookup of a missing key in a larger tree == Nothing" $
      lookup 4 (fromList [(k, k) | k <- [5,2,8,1,9,3,7 :: Int]]) `shouldBe` Nothing

  describe "insertOrMerge" $ do
    it "merges values for an existing key" $
      lookup 'a' (insertOrMerge 'a' (Sum 2) (insert 'a' (Sum 1) empty))
        `shouldBe` Just (Sum (3 :: Int))
    it "inserts a new key like insert" $
      toList (insertOrMerge 'b' (Sum 2) (insert 'a' (Sum 1) empty))
        `shouldBe` [('a', Sum (1 :: Int)), ('b', Sum 2)]
    it "merges also in the left subtree" $
      lookup 'a' (insertOrMerge 'a' (Sum 10) (fromList [('b', Sum 1), ('a', Sum 1)]))
        `shouldBe` Just (Sum (11 :: Int))
    it "merges also in the right subtree" $
      lookup 'c' (insertOrMerge 'c' (Sum 10) (fromList [('b', Sum 1), ('c', Sum 1)]))
        `shouldBe` Just (Sum (11 :: Int))
    it "combines existing <> new (order matters for Strings)" $
      lookup 1 (insertOrMerge (1 :: Int) "neu" (insert 1 "alt" empty))
        `shouldBe` Just "altneu"

  describe "fromList" $ do
    it "fromList [] == empty" $
      toList (fromList [] :: Map Int Int) `shouldBe` []
    it "last duplicate wins" $
      toList (fromList [("a", 1 :: Int), ("a", 2)]) `shouldBe` [("a", 2)]
    it "sorts the entries" $
      toList (fromList [(3,'c'),(1,'a'),(2,'b')]) `shouldBe` [(1,'a'),(2,'b'),(3,'c')]
    it "handles several duplicates among other keys" $
      toList (fromList [(1,"a"),(2,"b"),(1,"c"),(2,"d"),(3,"e")])
        `shouldBe` [(1,"c"),(2,"d"),(3,"e")]

  describe "fromListMerge" $ do
    it "fromListMerge [] == empty" $
      toList (fromListMerge [] :: Map Int (Sum Int)) `shouldBe` []
    it "merges duplicate keys" $
      toList (fromListMerge [("a", Sum 1), ("b", Sum 5), ("a", Sum (2 :: Int))])
        `shouldBe` [("a", Sum 3), ("b", Sum 5)]
    it "merges many duplicates" $
      lookup 'x' (fromListMerge [('x', Sum i) | i <- [1..10 :: Int]])
        `shouldBe` Just (Sum 55)

  describe "Show" $ do
    it "show empty == \"\"" $
      show (empty :: Map Int Int) `shouldBe` ""
    it "single entry" $
      show (fromList [(1 :: Int, "a")]) `shouldBe` "1 -> \"a\""
    it "multiple entries, one per line, sorted, no trailing newline" $
      show (fromList [(2 :: Int, 'b'), (1, 'a')]) `shouldBe` "1 -> 'a'\n2 -> 'b'"

  describe "Semigroup" $ do
    it "combines disjoint maps" $
      toList (fromList [(1 :: Int, "a")] <> fromList [(2, "b")])
        `shouldBe` [(1,"a"),(2,"b")]
    it "combines values of equal keys with (<>), left value first" $
      toList (fromList [(1 :: Int, "a"), (2, "x")] <> fromList [(1, "b"), (3, "y")])
        `shouldBe` [(1,"ab"),(2,"x"),(3,"y")]
    it "is associative" $ do
      let a = fromList [(1 :: Int, "a"), (2, "b")]
          b = fromList [(2, "c"), (3, "d")]
          c = fromList [(1, "e"), (3, "f")]
      toList ((a <> b) <> c) `shouldBe` toList (a <> (b <> c))

  describe "Monoid" $ do
    let m = fromList [(1 :: Int, "a"), (2, "b")]
    it "mempty == empty" $
      toList (mempty :: Map Int String) `shouldBe` []
    it "mempty <> m == m" $
      toList (mempty <> m) `shouldBe` toList m
    it "m <> mempty == m" $
      toList (m <> mempty) `shouldBe` toList m
    it "mconcat combines a list of maps" $
      toList (mconcat [fromList [(1 :: Int, Sum (1 :: Int))], fromList [(1, Sum 2)], fromList [(2, Sum 5)]])
        `shouldBe` [(1, Sum 3), (2, Sum 5)]