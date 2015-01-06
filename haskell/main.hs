import Test.HUnit

data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)

-- convenience
leaf :: a -> Tree a
leaf x = Branch x Empty Empty

-- info
height :: Tree a -> Integer
height Empty = 0
height (Branch _ left right) = 1 + Prelude.max (height left) (height right)

size :: Tree a -> Integer
size Empty = 0
size (Branch _ left right) = 1 + size left + size right

-- Balanced Search Tree
type BalancedTree = Tree

-- construct and edit
-- create :: a -> BalancedTree a
-- create :: [a] -> BalancedTree a
-- insert :: a -> BalancedTree a -> BalancedTree a
-- remove :: a -> BalancedTree a -> BalancedTree a

create x = leaf x

-- browse
minTree :: Eq a => BalancedTree a -> Maybe a
maxTree :: Eq a => BalancedTree a -> Maybe a
search ::Ord a =>  BalancedTree a -> a -> Bool
select :: BalancedTree a-> Integer -> Maybe a
rank :: Ord a => BalancedTree a-> a -> Integer

search Empty _ = False 
search (Branch value left right) key
  | key == value = True
  | key < value = search left key
  | otherwise   = search right key

minTree Empty = Nothing 
minTree (Branch value left _)
  | left == Empty = Just value
  | otherwise     = minTree left

maxTree Empty = Nothing 
maxTree (Branch value _ right)
  | right == Empty = Just value
  | otherwise      = maxTree right 

rank Empty _ = error "No such entry"
rank (Branch value left right) key
  | value == key = size left
  | key < value = rank left key 
  | otherwise   = size left + 1 + rank right key

select tree@(Branch value left right) order
  | order > total    = Nothing 
  | order == current = Just value 
  | order > current  = select right (order - current)
  | otherwise        = select left order
    where
      total = size tree
      current = size left + 1
      
-- print
-- print :: BalancedTree a -> String

-- pred
-- succ

-- Test case and Sample Values

tree1, tree2, tree3 :: Tree Int
-- empty tree
tree1 = Empty
-- unit tree
tree2 = leaf 1
-- a random tree
tree3 = Branch 7 
                  (Branch 2 Empty 
                            (Branch 5 (leaf 3) 
                                      Empty))
                  (Branch 6 (Branch 0 (leaf 4)
                                      (leaf 1))
                            (Branch 8 Empty
                                      (leaf 9)))
-- balanced tree
-- very unbalanced tree


bst1, bst2, bst3, bst4 :: BalancedTree Int
-- empty BST
bst1 = Empty
-- unit BST
bst2 = leaf 2 
-- random BST
bst3 = Branch 8 (Branch 3 (Branch 2 (leaf 1) Empty) (leaf 5)) (leaf 13)
-- Mirror BST
bst4 = Branch 4 (Branch 2 (leaf 1) (leaf 3)) (Branch 6 (leaf 5) (leaf 7))

-- test cases
-- test height
testHeight1 = TestCase (assertEqual "for tree1" 0 (height tree1))
testHeight2 = TestCase (assertEqual "for tree2" 1 (height tree2))
testHeight3 = TestCase (assertEqual "for tree3" 4 (height tree3))
testHeight4 = TestCase (assertEqual "for bst4" 3 (height bst4))

-- test size
testSize1 = TestCase (assertEqual "for tree1" 0 (size tree1))
testSize2 = TestCase (assertEqual "for tree2" 1 (size tree2))
testSize3 = TestCase (assertEqual "for tree3" 10 (size tree3))
testSize4 = TestCase (assertEqual "for bst4" 7 (size bst4))

-- test minTree
testMin1 = TestCase (assertEqual "for bst1" Nothing (minTree bst1))
testMin2 = TestCase (assertEqual "for bst2" (Just 2) (minTree bst2))
testMin3 = TestCase (assertEqual "for bst3" (Just 1) (minTree bst3))
testMin4 = TestCase (assertEqual "for bst4" (Just 1) (minTree bst4))

-- test maxTree
testMax1 = TestCase (assertEqual "for bst1" Nothing (maxTree bst1))
testMax2 = TestCase (assertEqual "for bst2" (Just 2) (maxTree bst2))
testMax3 = TestCase (assertEqual "for bst3" (Just 13) (maxTree bst3))
testMax4 = TestCase (assertEqual "for bst4" (Just 7) (maxTree bst4))

-- test search
testSearch1 = TestCase (assertEqual "for bst1" False (search bst1 1))
testSearch2 = TestCase (assertEqual "for bst2" False (search bst2 1))
testSearch3 = TestCase (assertEqual "for bst2" True (search bst2 2))
testSearch4 = TestCase (assertEqual "for bst3" True (search bst3 2))
testSearch5 = TestCase (assertEqual "for bst3" True (search bst3 13))
testSearch6 = TestCase (assertEqual "for bst3" False (search bst3 9))

-- test select
-- test rank

-- To run the tests, type
--   runTestTT tests
testHeights = TestList [testHeight1, testHeight2, testHeight3, testHeight4]
testSizes = TestList [testSize1, testSize2, testSize3, testSize4]
testMaxs = TestList [testMax1, testMax2, testMax3, testMax4]
testMins = TestList [testMin1, testMin2, testMin3, testMin4]
testSearches = TestList [testSearch1, testSearch2, testSearch3, testSearch4, testSearch5, testSearch6] 

tests = TestList [testHeights, testSizes, testMins, testMaxs, testSearches]

