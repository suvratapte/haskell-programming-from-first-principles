-- Chapter 10 exercises
{-# LANGUAGE FlexibleInstances #-}

import Data.Char -- Required for the cipher exercise
import Data.List -- Required for the phone exercise

-- Dog Types

-- 1 - Type constructor
-- 2 - * -> *
-- 3 - *
-- 4 - Num a => Doggies a
-- 5 - Doggies Int
-- 6 - Doggies String
-- 7 - Both
-- 8 - doge -> DogueDeBordeaux doge
-- 9 - DogueDeBordeaux String

-- Vehicles
data Price = Price Integer deriving (Eq, Show)

data Manufacturer =
    Mini
  | Mazda
  | Tata
  deriving (Eq, Show)

data Airline =
    PapuAir
  | CatapultsR'Us
  | TakeYourChancesUnited
  deriving (Eq, Show)

data Vehicle =
    Car Manufacturer Price
  | Plane Airline
  deriving (Eq, Show)

myCar = Car Mini (Price 14000)
urCar = Car Mazda (Price 20000)
clownCar = Car Tata (Price 7000)
doge = Plane PapuAir

-- 1 - Vehicle

-- 2
isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _ = False

isPlane :: Vehicle -> Bool
isPlane (Plane _) = True
isPlane _ = False

areCars :: [Vehicle] -> [Bool]
areCars = map isCar

-- 3
getManu :: Vehicle -> Manufacturer
getManu (Car man _) = man
getManu _ = error "Not a car"

-- 4 - It will crash.

-- 5
data Size =
    S
  | M
  | L
  deriving (Eq, Show)

data Vehicle' =
    Car' Manufacturer Price
  | Plane' Airline Size
  deriving (Eq, Show)

-- Cardinality

-- 1 - 1
-- 2 - 3
-- 3 - 65536
-- 4
-- Cardinality of Int: 18446744073709551616
-- Cardinality of Integer: Infinity
-- 5 - 2^8 = 256

-- For example
data Example = MakeExample deriving Show

-- 1
-- Example. You can't query type of a type constructor. You can however, query
-- the kind.

-- 2
-- It will display the kind of the type cosntructor, the data constructors and
-- typeclass instances. Show is an typeclass instance for Example.

-- 3
data Example' = MakeExample' Int deriving Show
-- :type MakeExample' will now be (map (`elem` bs) as).

-- Logic Goats

-- 1
class TooMany a where
  tooMany :: a -> Bool

newtype IntStringPair =
  IntStringPair (Int, String)
  deriving Show

instance TooMany IntStringPair where
  tooMany (IntStringPair (int, str)) = int > 42 || length str > 10

-- Without a newtype
instance TooMany (Int, String) where
  tooMany (int, str) = int > 42 || length str > 10

-- 2
instance TooMany (Int, Int) where
  tooMany (a, b) = a + b > 2 * 43

-- 3
instance (Num a, TooMany a) => TooMany (a, a) where
  tooMany (x, y) = tooMany x || tooMany y

-- Pity the Bool

-- 1
-- Cardinality:
-- Big Bool | Small Bool
-- Big Bool + Small Bool
-- 2 + 2
-- 4

-- 2
-- Cardinality: 258 (256 + 2)
-- It will give out a warning and round off.

-- How Does Your Garden Grow

-- 1
data FlowerType =
    Gardenia
  | Daisy
  | Rose
  | Lilac
  deriving Show

type Gardener = String

data Garden =
  Garden Gardener FlowerType
  deriving Show

-- Normal form:
data Garden' =
    Gardenia' Gardener
  | Daisy' Gardener
  | Rose' Gardener
  | Lilac' Gardener
  deriving Show

-- Programmers

data OperatingSystem =
    GnuPlusLinux
  | OpenBSDPlusNevermindJustBSDStill
  | Mac
  | Windows
  deriving (Eq, Show)


data ProgLang =
    Haskell
  | Agda
  | Idris
  | PureScript
  deriving (Eq, Show)

data Programmer =
  Programmer { os :: OperatingSystem
             , lang :: ProgLang }
  deriving (Eq, Show)

allOperatingSystems :: [OperatingSystem]
allOperatingSystems = [ GnuPlusLinux
                      , OpenBSDPlusNevermindJustBSDStill
                      , Mac
                      , Windows]

allLanguages :: [ProgLang]
allLanguages = [ Haskell
               , Agda
               , Idris
               , PureScript]

allProgrammers :: [Programmer]
allProgrammers = [Programmer { os = os, lang = lang}
                 | os <- allOperatingSystems, lang <- allLanguages]

-- Exponentiation in what order?

data Quantum =
    Yes
  | No
  | Both
  deriving Show

convert1 :: Quantum -> Bool
convert1 Yes  = True
convert1 No   = True
convert1 Both = True

convert2 :: Quantum -> Bool
convert2 Yes  = True
convert2 No   = True
convert2 Both = False

convert3 :: Quantum -> Bool
convert3 Yes  = True
convert3 No   = False
convert3 Both = True

convert4 :: Quantum -> Bool
convert4 Yes  = False
convert4 No   = True
convert4 Both = True

convert5 :: Quantum -> Bool
convert5 Yes  = True
convert5 No   = False
convert5 Both = False

convert6 :: Quantum -> Bool
convert6 Yes  = False
convert6 No   = True
convert6 Both = False

convert7 :: Quantum -> Bool
convert7 Yes  = False
convert7 No   = False
convert7 Both = True

convert8 :: Quantum -> Bool
convert8 Yes  = False
convert8 No   = False
convert8 Both = False

-- The Quad

-- 1 - 4 + 4 = 8
-- 2 - 4 * 4 = 16
-- 3 - 4 ^ 4 = 256
-- 4 - 2 * 2 * 2 = 8
-- 5 - 2 ^ 2 ^ 2 = 16
-- 6 - 4 ^ 4 ^ 2 = 65536

-- Inserting into trees

data BinaryTree a =
    Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

insert' ::
  Ord a
  => a
  -> BinaryTree a
  -> BinaryTree a
insert' e Leaf = Node Leaf e Leaf
insert' e (Node l a r)
  | e == a = Node l a r
  | e < a  = Node (insert' e l) a r
  | e > a  = Node l a (insert' e r)

-- Write a map for BinaryTree

mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node l a r) = Node (mapTree f l) (f a) (mapTree f r)

testTree' :: BinaryTree Integer
testTree' = Node (Node Leaf 3 Leaf) 1 (Node Leaf 4 Leaf)
mapExpected = Node (Node Leaf 4 Leaf) 2 (Node Leaf 5 Leaf)

mapOkay = if mapTree (+1) testTree' == mapExpected then print "yup okay!" else error "test failed!"

-- Convert binary trees to lists.

preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node l a r) = [a] ++ preorder l ++ preorder r

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node l a r) = preorder l ++ [a] ++ preorder r

postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder (Node l a r) = preorder l ++ preorder r ++ [a]

testTree :: BinaryTree Integer
testTree = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)

testPreorder :: IO ()
testPreorder = if preorder testTree == [2, 1, 3] then putStrLn "Preorder fine!" else putStrLn "Bad news bears."

testInorder :: IO ()
testInorder = if inorder testTree == [1, 2, 3] then putStrLn "Inorder fine!" else putStrLn "Bad news bears."

testPostorder :: IO ()
testPostorder = if postorder testTree == [1, 3, 2] then putStrLn "Postorder fine!" else putStrLn "postorder failed check"

main :: IO ()
main = do
  testPreorder
  testInorder
  testPostorder

-- Write foldr for BinaryTree

foldTree ::
    (a -> b -> b)
  -> b
  -> BinaryTree a
  -> b
foldTree f i = foldr f i . inorder

-- Chapter exercises

-- 1 - a
-- 2 - c
-- 3 - b
-- 4 - c

-- Ciphers
cipherChar :: Int -> Char -> Char
cipherChar n c =
  let baseIndex = if isUpper c then ord 'A' else ord 'a'
      index = ord c - baseIndex
      newIndex = mod (index + n) 26
  in chr $ newIndex + baseIndex

vigenere :: String -> String -> String
vigenere key str = go str 0
  where
    go [] index = []
    go (' ':xs) index = ' ' : go xs index
    go (x:xs) index =
      let
        char = (key !! index)
        baseIndex = if isUpper char then ord 'A' else ord 'a'
        offset = ord char - baseIndex
      in
        cipherChar offset x : go xs (mod (index + 1) (length key))

-- As-patterns

-- 1
isSubseqOf :: (Eq a) => [a] -> [a] -> Bool
isSubseqOf [] [] = True
isSubseqOf [] bs = True
isSubseqOf _ [] = False
isSubseqOf (a : as) bs = isSubseqOf as (drop 1 (dropWhile (/= a) bs))

-- Using "."
isSubSeqOf' :: Eq a => [a] -> [a] -> Bool
isSubSeqOf' [] _ = True
isSubSeqOf' _ [] = False
isSubSeqOf' aWhole@(a:as) (b:bs)
  | a == b = isSubSeqOf' as bs
  | otherwise = isSubSeqOf' aWhole bs

-- 2
capitalizeWords :: String -> [(String, String)]
capitalizeWords = map (\word@(w:ws) -> (word, toUpper w : ws)) . words

-- Language exercises

-- 1
capitalizeWord :: String -> String
capitalizeWord [] = []
capitalizeWord (x:xs) = toUpper x : xs

-- 2

-- This function handles a sentence starting with a space or new line.
capitalizeSentence :: String -> String
capitalizeSentence (s@' ' : xs) = s : capitalizeWord xs
capitalizeSentence (s@'\n' : xs) = s : capitalizeWord xs
capitalizeSentence xs = capitalizeWord xs

splice :: (Eq a) => [a] -> a -> [[a]]
splice xs e = go xs []
  where go xs xss
          | null xs = xss
          | otherwise = go (drop 1 (dropWhile (/= e) xs))
                           (xss ++ [takeWhile (/= e) xs])

join :: a -> [[a]] -> [a]
join _ [] = []
join separator [x] = x
join separator (x:xs) = x ++ separator : join separator xs

capitalizeParagraph :: String -> String
capitalizeParagraph str =
  let s = splice str '.'
      sc = map capitalizeSentence s
  in
    join '.' sc

-- Phone exercise

-- 1
data KeyLayout =
  MkKeyLayout {
    key :: Char
  , chars :: [Char]
  }
  deriving (Show, Eq)

data DaPhone = MkDaPhone
  {
    keys :: [KeyLayout]
  }
  deriving (Show, Eq)

phone =  MkDaPhone [ MkKeyLayout '1' "1"
                   , MkKeyLayout '2' "abc2"
                   , MkKeyLayout '3' "def3"
                   , MkKeyLayout '4' "ghi4"
                   , MkKeyLayout '5' "jkl5"
                   , MkKeyLayout '6' "mno6"
                   , MkKeyLayout '7' "pqrs7"
                   , MkKeyLayout '8' "tuv8"
                   , MkKeyLayout '9' "wxyz9"
                   , MkKeyLayout '*' "*^"
                   , MkKeyLayout '0' "+ 0"
                   , MkKeyLayout '#' ".,#"]

-- 2

type Digit = Char

type Presses = Int

elemIndex' :: (Eq a) => a -> [a] -> Int
elemIndex' e xs = go xs 0
  where
    go list index
      | null list = -1
      | e == head list = index
      | otherwise = go (tail list) (index + 1)

reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
reverseTaps (MkDaPhone keys) ch =
  let
    upper = isUpper ch
    lowerCh = toLower ch
    (MkKeyLayout key chars) = head $ filter (\(MkKeyLayout key chars) -> lowerCh `elem` map toLower chars) keys
    index = lowerCh `elemIndex'` chars
  in
    if upper
    then [('*', 1), (key, index + 1)]
    else [(key, index + 1)]

convo :: [String]
convo =  [ "Wanna play 20 questions"
         , "Ya"
         , "U 1st haha"
         , "Lol ok. Have u ever tasted alcohol"
         , "Lol ya"
         , "Wow ur cool haha. Ur turn"
         , "Ok. Do u think I am pretty Lol"
         , "Lol ya"
         , "Just making sure rofl ur turn"]

cellPhonesDead :: DaPhone -> String -> [(Digit, Presses)]
cellPhonesDead phone = concatMap (reverseTaps phone)

convToTaps :: DaPhone -> [String] -> [[(Digit, Presses)]]
convToTaps phone = map $ cellPhonesDead phone

-- 3
fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps = sum . map snd

-- 4
mostPopularLetter :: String -> Char
mostPopularLetter = head . last . sortOn length . group . sort

cost :: DaPhone -> Char -> Presses
cost phone = fingerTaps . reverseTaps phone

-- 5

coolestLtr :: [String] -> Char
coolestLtr = mostPopularLetter . filter (/= '\n') . unwords

coolestWord :: [String] -> String
coolestWord = head . last . sortOn length . group . sort . words . unwords
