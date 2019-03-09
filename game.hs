-- CS300 Spring 2018 Exam 1 - 25 February 2018
--
-- Total time is 5 hours and maximum score is 25 points.
-- Parts *must* be done in order. Exact score per part will be assigned later. 
-- If a part is not working, you will *not* be graded for any later parts. 
-- No USBs, mobiles, or electronic devices with you. Printouts are okay.
-- Strictly no talking to anyone during the exam. 
--
-- We have given a main function that uses test cases near the end of file to 
-- tell you how many parts you have successfully done. Note that we will test 
-- it on additional inputs for grading so do not hardcode for the given test 
-- cases. You only need to fill in the code for each part. The error 
-- "Prelude.undefined" means that you have not yet written the code for the 
-- next part in order.-
--
-------------------------------------------------------------------------------
import Data.List (sort)
-- YOU ARE NOT ALLOWED TO DO ANY ADDITIONAL IMPORTS
--
-- You are a computer scientist hired by a game development company to improve 
-- the performance of their multiplayer Roll Playing Game (RPG). You realize 
-- that the game stores its state (the position of all players and objects in 
-- the "game world") in a 2-D character array, or equivalently, an array of 
-- "strings". Here is a sample game state:
-- 
--  Col 0  Col 1   Col 2  Col 3
--  ------------------------------------
-- [[' ',   ' ',   ' ',   ' '],        | (Row 0)
--  [' ',   'W',   'W',   ' '],        | (Row 1)
--  ['E',   ' ',   ' ',   ' '],        | (Row 2)
--  [' ',   ' ',   'P',   ' ']]        | (Row 3)
--
-- The ' ' characters signify empty spaces in the game. The player can walk 
-- through these. 'E' is the enemy sprite (a game object is called a "sprite").
-- 'W' are walls. Sprite positions can be marked using any symbol other than 
-- ' ' which, as mentioned earlier, is for empty spaces in the game world.
--
-- To begin your analysis, you decide to write a couple of helper functions 
-- which you can call every turn to analyze what the game world contains.

-- === Part 1 ===
-- Returns the number of rows in a world
-- FOR THIS PART, YOU CANNOT USE ANY BUILTIN FUNCTION
numRows :: [[a]] -> Int
numRows x = tot_rows x
tot_rows [] = 0
tot_rows [x]= 1
tot_rows (x:xs) = (1 + tot_rows xs)

-- === Part 2 ==
-- Given the value that represents an empty location, the number of rows and 
-- the number of columns (in this order), create a new game world
createEmptyWorld :: a -> Int -> Int -> [[a]]
createEmptyWorld x y z = newWorld x y z

newWorld em ro co = replicate ro (replicate co em)

-- === Part 3 ===
-- Returns True if the row contains *only* sprites (or empty spaces) with the
-- symbol given in argument and no other sprites.
-- As an example, in the sample game state, rowContainsOnly called with ' ' 
-- will return True on Row 0, but False on Rows 1 to 3.
rowContainsOnly :: Eq a => a -> [a] -> Bool
rowContainsOnly sp [] = True
rowContainsOnly sp ro = minimum((map (checkOnly sp)) ro)

checkOnly sp x = sp==x

-- === Part 4 ===
-- Returns True if entire world contains *only* sprites (or empty space) with 
-- the symbol given in argument.
worldContainsOnly :: Eq a => a -> [[a]] -> Bool
worldContainsOnly sp [] = True
worldContainsOnly sp wo = minimum((map (checkWorld sp)) wo)

checkWorld sp ro = (rowContainsOnly sp ro)

-- === Part 5 ===
-- Given a world, return a new world with the same dimensions but with only 
-- empty spaces where the value representing empty space is given as argument
clearWorld :: a -> [[a]] -> [[a]]
clearWorld _ [] = []
clearWorld sp x = map (clearRow sp) x


clearRow sp x = map (clearOne sp) x
clearOne sp x = sp

-- === Part 6 ===
-- Returns True if a given world contains *at least one* sprite with the symbol
-- given in first argument

worldContains :: Eq a => a -> [[a]] -> Bool
worldContains sp [] = False
worldContains sp wo =  maximum (map (rowContains sp) wo)

rowContains :: Eq a => a -> [a] ->Bool
rowContains sp ro = if (length (filter (==sp) ro) >0) then True else False

-- == Part 7 ==
-- After analysis, you realize that it is not the processing time on the game 
-- servers that is causing delay, it is the *network* which is the bottleneck. 
-- The company has a proprietary network which its users subscribe to. The 
-- network is very low latency but has very little bandwidth. The game sends 
-- its game state to the server after every turn, but the game state in its 
-- current format is large enough to cause a lot of delay. What is needed is to 
-- find ways to compress the game state. You figure out two ways to compress 
-- the game state, which you plan to use in two different cases.
--
-- First arpproach: The game state is composed almost entirely of empty spaces 
-- and the number of sprites is very small. For this sutation, you represented 
-- every sprite (excluding empty spaces) with a tuple containing its symbol, 
-- its row in the game world, and its column in the game world. The compressed 
-- game state should look like this: 
-- [(s1, r1, c1), (s2, r2, c2), ... ]
--
-- Your task is to take the symbol to represent empty spaces (e.g. ' '), row 
-- number, column number and the actual row (char array / string) in this order 
-- and return the compressed form of the row as an array of tuples.
easyCompressRow :: Eq a => a -> Int -> Int -> [a] -> [(a, Int, Int)]
easyCompressRow sp ro co li = roCom sp ro co li 0

roCom sp ro co [] _ = []
roCom sp ro co (li:lis) i | li == sp = [] ++ roCom sp ro co lis (i+1)
                          | otherwise = [(makeTuple li ro i)] ++ (roCom sp ro co lis (i+1))

makeTuple sp ro co = (sp,ro,co)
-- == Part 8 ==
-- Takes in the symbol representing empty spaces and a game world and returns 
-- its compressed version as an array of tuples
easyCompressWorld :: Eq a => a -> [[a]] -> [(a, Int, Int)]
easyCompressWorld sp wo = mapWorld sp wo 0

mapWorld sp [] _ = []
mapWorld sp (wo:wos) i = (easyCompressRow sp i 0 wo) ++ (mapWorld sp wos (i+1))

-- == Part 9 ==
-- Given a sprite symbol, the desired column, and a row of sprites (in that
-- order), return a new row with the new sprite inserted in the desired column
insertSpriteInRow :: a -> Int -> [a] -> [a]
insertSpriteInRow sp co [] = [sp]
insertSpriteInRow sp co li | (length li) <= co = li ++ [sp]
insertSpriteInRow sp co li = insertSp sp co li 0 (length li)


insertSp :: a -> Int -> [a] ->Int ->Int -> [a]
insertSP sp co [] _ _= []
insertSp sp co (li:lis) i to_co | i /= co = if(i == (to_co-1)) then [li] else [li] ++ insertSp sp co lis (i+1) to_co
                                | otherwise = if(i == (to_co-1)) then [sp] else [sp] ++ insertSp sp co lis (i+1) to_co

-- == Part 10 == 
-- Insert the given sprite and the given row and column in the given wrold (in 
-- that order) and return a new world with the desired sprite added

insertSpriteInWorld :: a -> Int -> Int -> [[a]] -> [[a]]
insertSpriteInWorld sp ro co [[]] = handleEmp sp ro co 0
insertSpriteInWorld sp ro co wo = insertSpWorld sp ro co wo 0

insertSpWorld sp ro co [] _  = []
insertSpWorld sp ro co (wo:wos) i | i == ro = [(insertSpriteInRow sp co wo)] ++ insertSpWorld sp ro co wos (i+1)
                                  | otherwise = [wo] ++ insertSpWorld sp ro co wos (i+1)

handleEmp :: a -> Int -> Int -> Int -> [[a]]
handleEmp sp ro co i | ro /= i = [[]] ++ handleEmp sp ro co (i+1)
                     | otherwise = [[sp]]





-- == Part 11 ==
-- Given the empty symbol, the rows and columns, and the compressed world, 
-- return the corresponding decompressed world
easyDecompressWorld :: Eq a => a -> Int -> Int -> [(a, Int, Int)] -> [[a]]
easyDecompressWorld em r c w = makeRows em r c w 0

makeRows em ro co (w) i | i < ro = [rowConstruct em i 0 co w] ++ makeRows em ro co w (i+1)
                         | otherwise = []

rowConstruct em rN i to_co tup | i < to_co = [(findInTuples em rN i tup to_co 0)] ++ (rowConstruct em rN (i+1) to_co tup)
                       | otherwise = []

findInTuples :: a -> Int -> Int -> [(a,Int,Int)] -> Int ->Int -> a
findInTuples em _ i [] to_co z = em
findInTuples em rN i (t:ts) to_co z = 
				                 let (a,b,c) = t
				                 in 
				                     if(b== rN && c == i) then a else (findInTuples em rN i ts to_co z)
				                 
-- == Part 12 ==
-- Second approach: When the game world is filled with sprites, the above 
-- compression performs even worse than the original implementation. So you 
-- decide to create something better for this case. You realize the game only 
-- has 3 sprites in addition to empty spaces. These are Walls (W), Enemies (E) 
-- and the Player (P). You also realize that the order of sprites in a row does 
-- not matter. What matters is that sprites in one row should remain in the 
-- same row when they reach the server, even if their order within that row 
-- changes. You decide to represent every sprite with a prime number and every 
-- row with a product of those primes. You make the following algorithm:
-- Begin with 1
-- For each ' ' in the row, multiply by 2.
-- For each W in the row, multiply by 3.
-- For each E in the row, multiply by 5.
-- For each P in the row, multiply by 7.
-- Return one integer for each row.
-- e.g [' ', ' ', 'W', 'P'] becomes 2 * 2 * 3 * 7 which is 84
-- It does not matter what you return for invalid inputs
charToPrime :: [Char] -> Int
charToPrime [] = 1
charToPrime x = changeChar x

changeChar :: [Char] -> Int
changeChar [] = 1
changeChar (l:ls) = if (l == ' ') then (2 * changeChar ls) else if (l == 'W') then (3 * changeChar ls) else if (l == 'E') then (5 * changeChar ls) else if (l == 'P') then (7 * changeChar ls)
	              else 1

-- == Part 13 ==
-- Since each sprite is represented by a prime, the product representing the 
-- row has a unique factorizing into the primes allowing us to figure out what
-- the original sprites in the row were. You can use integer modulus operator 
-- (mod) and integer division operator (div). For example "84 `mod` 2==0" which 
-- means 2 is a prime factor and hence there was a ' ' in the row. We can use 
-- div to find leftover product e.g. "84 `div` 2==42" which means 42 is the 
-- product representing the remaining sprites. It is again divisble by 2 which
-- means there was another ' ' in the row and so on. It does not matter what 
-- you return for invalid inputs.
primeToChar :: Int -> [Char]
primeToChar 0 = []
primeToChar x = changePrime x

changePrime x = if( (x `mod` 2) == 0) then ([' '] ++ changePrime (x `div` 2)) else 
	            if( (x `mod` 3) == 0) then (['W'] ++ changePrime (x `div` 3)) else 
	            if( (x `mod` 5) == 0) then (['E'] ++ changePrime (x `div` 5)) else 
	            if( (x `mod` 7) == 0) then (['P'] ++ changePrime (x `div` 7)) else
	            	[]

-- == Part 14 ==
-- The class below represents a type of sprite whose lists can be converted 
-- back and forth from integers. You have to make the Char type a member of the
-- Pr typeclass by implementing the required functions.
class Pr a where
    toPrime :: [a] -> Int
    fromPrime :: Int -> [a]

instance Pr Char where
	toPrime x = charToPrime x
	fromPrime x = primeToChar x

-- instance Eq Char where
-- 	toPrime x = charToPrime x
-- 	fromPrime x = primeToChar x

-- ============================
func :: [[Char]] ->[Int]
func x = betterCompressWorld x

func2 :: [Int] -> [[Char]]
func2 x = betterDecompressWorld x

-- == Part 15 ==
-- Returns the list of compressed integers representing the entire game world 
betterCompressWorld :: Pr a => [[a]] -> [Int]
betterCompressWorld x = betCompress x

betCompress [] = []
betCompress (x:xs) = [(toPrime x)] ++ betCompress xs

-- == Part 16 ==
-- Decompress the list of primes to the game world
betterDecompressWorld :: Pr a => [Int] -> [[a]]
betterDecompressWorld x = betDecompress x

betDecompress [] = []
betDecompress (x:xs) = [(fromPrime x)] ++ betDecompress xs



-- =========================== CLEAR UPTO THIS POINT ==================================
-- Part 17, 18 and 19 are prone to error
