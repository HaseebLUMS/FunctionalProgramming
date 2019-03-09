-- ---------------------------------------------------------------------
-- DNA Analysis 
-- CS300 Spring 2018
-- Due: 24 Feb 2018 @9pm

-- MUHAMMAD HASEEB 
-- 2020-10-0192
-- ALSO REMEMBERED AS 20100192
-- ------------------------------------Assignment 2------------------------------------
--
-- >>> YOU ARE NOT ALLOWED TO IMPORT ANY LIBRARY
-- Functions available without import are okay
-- Making new helper functions is okay
--
-- ---------------------------------------------------------------------
--
-- DNA can be thought of as a sequence of nucleotides. Each nucleotide is 
-- adenine, cytosine, guanine, or thymine. These are abbreviated as A, C, 
-- G, and T.
--
type DNA = [Char]
type RNA = [Char]
type Codon = [Char]
type AminoAcid = Maybe String

-- ------------------------------------------------------------------------
-- 				PART 1
-- ------------------------------------------------------------------------				

-- We want to calculate how alike are two DNA strands. We will try to 
-- align the DNA strands. An aligned nucleotide gets 3 points, a misaligned
-- gets 2 points, and inserting a gap in one of the strands gets 1 point. 
-- Since we are not sure how the next two characters should be aligned, we
-- try three approaches and pick the one that gets us the maximum score.
-- 1) Align or misalign the next nucleotide from both strands
-- 2) Align the next nucleotide from first strand with a gap in the second     
-- 3) Align the next nucleotide from second strand with a gap in the first    
-- In all three cases, we calculate score of leftover strands from recursive 
-- call and add the appropriate penalty.                                    

score :: DNA -> DNA -> Int


--simpleScore :: DNA -> DNA -> Int

score [] _ = 0
score _ [] = 0
score (x:xs) (y:ys) | x == y = 3 + score xs ys
                          | otherwise = maximum [firstBlank (x:xs) (y:ys) , sencondBlank (x:xs) (y:ys) , noBlank (x:xs) (y:ys)]

--firstBlank [] []= 0
firstBlank [] _ = 0
firstBlank _ [] = 0
firstBlank (x:xs) (y:ys) | x == y = 3 + score xs ys
                         | otherwise = 1 + score (x:xs) (ys)

--sencondBlank [] [] = 0
sencondBlank [] _ = 0
sencondBlank _ [] = 0
sencondBlank (x:xs) (y:ys) | x == y = 3 + score xs ys
                           | otherwise = 1 + score xs (y:ys)

--noBlank [] [] = 0
noBlank [] _ = 0
noBlank _ [] = 0
noBlank (x:xs) (y:ys) | x == y = 3 + score xs ys
                      | otherwise = 2 + score xs ys


-- -------------------------------------------------------------------------
--				PART 2
-- -------------------------------------------------------------------------
-- Write a function that takes a list of DNA strands and returns a DNA tree. 
-- For each DNA strand, make a separate node with zero score 
-- in the Int field. Then keep merging the trees. The merging policy is:
-- 	1) Merge two trees with highest score. Merging is done by making new
--	node with the smaller DNA (min), and the two trees as subtrees of this
--	tree
--	2) Goto step 1 :)
--

data DNATree = Node DNA Int DNATree DNATree | Nil deriving (Ord, Show, Eq)

makeDNATree :: [DNA] -> DNATree
makeDNATree x = head $ mergeAll (nodeTrees x)

nodeTrees :: [DNA] -> [DNATree]
nodeTrees [] = [] 
nodeTrees (x:xs) = [Node x 0 Nil Nil] ++ nodeTrees xs

revnodeTrees :: DNATree -> DNA
revnodeTrees (Node dna1 num1 leftchild1 rightchild1) = dna1

treeTuples :: [DNATree] -> [[DNATree]]
treeTuples [] = []
treeTuples [x]=[]
treeTuples (x:xs) = [[x,a] | a <- xs ] ++ treeTuples xs




-- mergeAll will receive list of lists of DNATREES and take maxScList and merge then do it recursively until one tree remains in incoming list



mergeAll :: [DNATree] -> [DNATree]
mergeAll [] = []
mergeAll [x] = [x]
--mergeAll x = iterate maxScList x !! (length x)
mergeAll x = mergeAll (maxScList x)

--removeDoubles:: [[DNATree]] -> [[DNATree]]
--removeDoubles x = concat [filter (\q -> (a!!0 /= q !! 1) && (a!!1 /= q !! 0)) x | a<- x]

-- im sure that every list has two elements 
-- it will return back given list with highest score dnas merged
maxScList :: [DNATree] -> [DNATree]
maxScList [x] = [x]
maxScList x = maxScList' (treeTuples x) (mscore (treeTuples x)) x


rm :: [DNATree] -> DNATree -> DNATree -> [DNATree]
rm [] _ _ = []
rm [x] a b = if (x==a || x == b) then [] else [x]
rm (x:xs) a b = if (x == a || x == b) then rm xs a b else [x] ++ rm xs a b


-- main part of main business
maxScList' :: [[DNATree]] -> Int -> [DNATree] -> [DNATree]
maxScList' [] _ _= []
maxScList' (x:xs) n con | length x == 2 && ( score (revnodeTrees(x !! 0)) (revnodeTrees(x !! 1)) ) == n = [(merge (x!!0) (x!!1) )] ++ (rm con (x!!0) (x!!1)) 
                        | otherwise = maxScList' xs n con


-- mscore will take whole [[DNAtree]] and just return max score from score of all pairs
mscore :: [[DNATree]] -> Int
mscore x = maximum (map (\r -> score (revnodeTrees(r!!0)) (revnodeTrees(r!!1)) ) x)



-- merge two given tress
merge :: DNATree -> DNATree -> DNATree
merge (Node dna1 num1 leftchild1 rightchild1) (Node dna2 num2 leftchild2 rightchild2)  = 
	let tree1 = Node dna1 num1 leftchild1 rightchild1
	    tree2 = Node dna2 num2 leftchild2 rightchild2
	in  Node (min dna1 dna2) (score dna1 dna2) 
		(if num1 <= num2 then tree1 else tree2) (if num1 > num2 then tree1 else tree2 )


-- -------------------------------------------------------------------------
--				PART 3
-- -------------------------------------------------------------------------

-- Even you would have realized it is hard to debug and figure out the tree
-- in the form in which it currently is displayed. Lets try to neatly print 
-- the DNATree. Each internal node should show the 
-- match score while leaves should show the DNA strand. In case the DNA strand 
-- is more than 10 characters, show only the first seven followed by '...' 
-- The tree should show like this for an evolution tree of
-- ['AACCTTGG','ACTGCATG', 'ACTACACC', 'ATATTATA']
--
-- 20
-- +---ATATTATA
-- +---21
--     +---21
--     |   +---ACTGCATG
--     |   +---ACTACACC
--     +---AACCTTGG
--
-- Make helper functions as needed. It is a bit tricky to get it right. One
-- hint is to pass two extra string, one showing what to prepend to next 
-- level e.g. '+---' and another to prepend to level further deep e.g. '|   '


draw :: DNATree -> [Char]
draw x = showw x 0

showw :: DNATree -> Int -> String
showw (Node dna sc lc rc) x = if (lc == Nil && rc == Nil) 
							then ( dna ++ "\n" )
						   		else 
						   			if (lc /= Nil && rc /= Nil) 
						   			then ( (show sc) ++ "\n"
						   				++(concat $ replicate x "| ") ++ "+---" ++showw (lc) (x+1) 
						   				 ++(concat $ replicate x "| ") ++ "+---" ++ showw (rc) (x+1) )
						   				else " "
					   					 -- if (lc == Nil && rc /= Nil) 
					   					 -- then ( '| +---' ++ (givedna lc) ++ '\n' ++ '| +---' ++ (givesc rc) ++ '\n' ++ '| | +---' ++showw (givelc rc) ++ '| +---' ++ showw (giverc rc) )
					   						-- else 
					   						-- 	if (lc /= Nil && rc == Nil) 
					   						-- 	then ( '| +---' ++ (givesc lc) ++ '\n' ++ '| +---' ++ showw (givelc lc) ++ '| +---' ++ showw (giverc lc) ++ '| +---' ++ givedna rc )
					   						-- 		else ' '



givedna :: DNATree -> DNA
givedna (Node dna sc lc rc) = dna

givesc :: DNATree -> String
givesc (Node dna sc lc rc) = show sc

giverc :: DNATree -> DNATree
giverc (Node dna sc lc rc) = rc

givelc :: DNATree -> DNATree
givelc (Node dna sc lc rc) = lc


-- ---------------------------------------------------------------------------
--				PART 4
-- ---------------------------------------------------------------------------
--
--
-- Our score function is inefficient due to repeated calls for the same 
-- suffixes. Lets make a dictionary to remember previous results. First you
-- will consider the dictionary as a list of tuples and write a lookup
-- function. Return Nothing if the element is not found. Also write the 
-- insert function. You can assume that the key is not already there.
type Dict a b = [(a,b)]

lookupDict :: (Eq a) => a -> Dict a b -> Maybe b
lookupDict a [] = Nothing
lookupDict a (d:ds) | (fst d) == a = Just (snd d)
                    | otherwise = lookupDict a ds 


insertDict :: (Eq a) => a -> b -> (Dict a b)-> (Dict a b)
insertDict a b x = x ++ [(a,b)]

-- We will improve the score function to also return the alignment along
-- with the score. The aligned DNA strands will have gaps inserted. You
-- can represent a gap with '-'. You will need multiple let expressions 
-- to destructure the tuples returned by recursive calls.

alignment :: String -> String -> ((String, String), Int)
alignment dna1 dna2 = score1 dna1 dna2



score1 :: String -> String -> ((String, String), Int)
score1 [] _ = (("",""),0)--(((),()),)
score1 _ [] = (("",""),0)--(((),()),)
score1 (x:xs) (y:ys)  =
                       let abc = [firstBlank1 (x:xs) (y:ys) , sencondBlank1 (x:xs) (y:ys) , noBlank1 (x:xs) (y:ys)]
                           mS = maximum (map sumOfSc abc)
                           dna1' = map st1 abc
                           dna2' = map st2 abc
                           ind   = tellInd mS (map sumOfSc abc) 0
                           in ((dna1' !! ind , dna2' !! ind), mS)


tellInd :: Int -> [Int] -> Int -> Int
tellInd a [] _ = -1
tellInd a (x:xs) z| (x==a) = z
                  | otherwise = tellInd a xs (z+1)


sumOfSc :: [((String,String),Int)] -> Int
sumOfSc [] = 0
sumOfSc (x:xs) = (snd x) + sumOfSc xs

st1 :: [((String,String),Int)] -> String
st1 [] = []
st1 (x:xs) = fst (fst x) ++ st1 xs

st2 :: [((String,String),Int)] -> String
st2 [] = []
st2 (x:xs) = snd (fst x) ++ st2 xs

--firstBlank [] []= 0
firstBlank1 :: String -> String -> [((String,String),Int)]
firstBlank1 [] _ = []
firstBlank1 _ [] = []
firstBlank1 (x:xs) (y:ys) | x == y = [(([x],[y]),3)] ++ [score1 xs ys]
                         | otherwise = [(("-",[y]),1)] ++ [score1 (x:xs) (ys)]

--sencondBlank [] [] = 0
sencondBlank1 :: String -> String -> [((String,String),Int)]
sencondBlank1 [] _ = []
sencondBlank1 _ [] = []
sencondBlank1 (x:xs) (y:ys) | x == y = [(([x],[y]),3)] ++ [score1 xs ys]
                           | otherwise = [(([x],"-"),1)] ++ [score1 xs (y:ys)]

--noBlank [] [] = 0
noBlank1 :: String -> String -> [((String,String),Int)]
noBlank1 [] _ = []
noBlank1 _ [] = []
noBlank1 (x:xs) (y:ys) | x == y = [(([x],[y]),3)] ++ [score1 xs ys]
                      | otherwise =[(([x],[y]),2)] ++ [score1 xs ys]
-- We will now pass a dictionary to remember previously calculated scores 
-- and return the updated dictionary along with the result. Use let 
-- expressions like the last part and pass the dictionary from each call
-- to the next. Also write logic to skip the entire calculation if the 
-- score is found in the dictionary. You need just one call to insert.
type ScoreDict = Dict (DNA,DNA) Int

scoreMemo :: (DNA,DNA) -> ScoreDict -> (ScoreDict,Int)
scoreMemo x dic = 
					let abc = (alignment (fst x) (snd x))
					in if ((lookupDict x dic) == Nothing)
						then ((insertDict x (snd abc) dic),(snd abc))
						else (dic, snd abc)

-- In this part, we will use an alternate representation for the 
-- dictionary and rewrite the scoreMemo function using this new format.
-- The dictionary will be just the lookup function so the dictionary 
-- can be invoked as a function to lookup an element. To insert an
-- element you return a new function that checks for the inserted
-- element and returns the old dictionary otherwise. You will have to
-- think a bit on how this will work. An empty dictionary in this 
-- format is (\_->Nothing)


type Dict2 a b = a->Maybe b 
insertDict2 :: (Eq a)  => a -> b -> (Dict2 a b)-> (Dict2 a b)
insertDict2 a b x = if( function (x a) == True) then (\q-> (if q == a then Just b else x q) ) else x 


function :: Maybe a -> Bool
function Nothing = True
function (Just _) = False


type ScoreDict2 = Dict2 (DNA,DNA) Int

scoreMemo2 :: (DNA,DNA) -> ScoreDict2 -> (ScoreDict2,Int)

scoreMemo2 x dic = 
					let abc = (alignment (fst x) (snd x))
					in ((insertDict2 x (snd abc) (dic)),snd abc)

-- ---------------------------------------------------------------------------
-- 				PART 5
-- ---------------------------------------------------------------------------

-- Now, we will try to find the mutationDistance between two DNA sequences.
-- You have to calculate the number of mutations it takes to convert one 
-- (start sequence) to (end sequence). You will also be given a bank of 
-- sequences. However, there are a couple of constraints, these are as follows:

-- 1) The DNA sequences are of length 8
-- 2) For a sequence to be a part of the mutation distance, it must contain 
-- 'all but one' of the neuclotide bases as its preceding sequence in the same 
-- order AND be present in the bank of valid sequences
-- 'AATTGGCC' -> 'AATTGGCA' is valid only if 'AATTGGCA' is present in the bank
-- 3) Assume that the bank will contain valid sequences and the start sequence
-- may or may not be a part of the bank.
-- 4) Return -1 if a mutation is not possible

	
-- mutationDistance 'AATTGGCC' 'TTTTGGCA' ['AATTGGAC', 'TTTTGGCA', 'AAATGGCC' ,'TATTGGCC', 'TTTTGGCC'] == 3
-- mutationDistance 'AAAAAAAA' 'AAAAAATT' ['AAAAAAAA', 'AAAAAAAT', 'AAAAAATT', 'AAAAATTT'] == 2


mutationDistance :: DNA -> DNA -> [DNA] -> Int
mutationDistance x y z = mainFunc x y z 0


mainFunc x y z i | x /= y = 
								if((z) == [] && i == 0) then -1
								else if (x /= y && (oneNBList x y (z)) == []) then -1
								else 
								1+ (mainFunc ((oneNBList x y (z))!!0) y (z) (i+1))
                    | otherwise = 0


oneNBList :: DNA -> DNA -> [DNA]-> [DNA]
oneNBList x z [] = []
oneNBList x z (y:ys) | diff x y z == 1 = [y] ++ oneNBList x z ys
				     | otherwise = oneNBList x z ys

diff _ _ [] = 0
diff [] _ _ = 0
diff _ [] _ = 0
diff (x:xs) (y:ys) (z:zs) | x /= y && y ==z = 1 + diff xs ys zs
                          | otherwise = if(x /= y) then 2 + diff xs ys zs else 0 + diff xs ys zs


-- ---------------------------------------------------------------------------
-- 				PART 6
-- ---------------------------------------------------------------------------
--
-- Now, we will write a function to transcribe DNA to RNA. 
-- The difference between DNA and RNA is of just one base i.e.
-- instead of Thymine it contains Uracil. (U)
--
transcribeDNA :: DNA -> RNA
transcribeDNA [] = []
transcribeDNA (x:xs) | x /= 'T' = [x] ++ transcribeDNA xs
					 | otherwise = ['U'] ++ transcribeDNA xs

-- Next, we will translate RNA into proteins. A codon is a group of 3 neuclotides 
-- and forms an aminoacid. A protein is made up of various amino acids bonded 
-- together. Translation starts at a START codon and ends at a STOP codon. The most
-- common start codon is AUG and the three STOP codons are UAA, UAG and UGA.
-- makeAminoAcid should return Nothing in case of a STOP codon.
-- Your translateRNA function should return a list of proteins present in the input
-- sequence. 
-- Please note that the return type of translateRNA is [String], you should convert
-- the abstract type into a concrete one.
-- You might wanna use the RNA codon table from 
-- https://www.news-medical.net/life-sciences/RNA-Codons-and-DNA-Codons.aspx
-- type Codon = [Char]
-- type AminoAcid = Maybe String
--
-- data RNA1 = A | U | C | G

-- data AminoAcid1
--     = Ala | Cys | Asp | Glu | Phe | Gly | His | Ile | Lys | Leu
--     | Met | Asn | Pro | Gln | Arg | Ser | Thr | Val | Trp | Tyr
--     | Stop

makeAminoAcid :: Codon -> AminoAcid
makeAminoAcid (x) = if ((decode (x)) /= "Stop") then Just (decode x) 
                   else Nothing

translateRNA :: RNA -> [String]  
translateRNA x = filter (/= "") (translateRNA' x)




translateRNA' x = [(translateRNA1 x),(translateRNA2 (drop 1 x)), (translateRNA3 (drop 2 x))]
translateRNA1 [] = []
translateRNA1 [x] = []
translateRNA1 (x:y:xs) | xs == [] = []
translateRNA1 (x:y:z:zs) | x == 'A' && y == 'U' && z == 'G' = decode "AUG" ++ " " ++trans zs
						| otherwise = translateRNA1 (zs)
translateRNA2 [] = []
translateRNA2 [x] = []
translateRNA2 (x:y:xs) | xs == [] = []
translateRNA2 (x:y:z:zs) | x == 'A' && y == 'U' && z == 'G' = decode "AUG" ++ " " ++trans zs
						| otherwise = translateRNA2 (zs)
translateRNA3 [] = []
translateRNA3 [x] = []
translateRNA3 (x:y:xs) | xs == [] = []
translateRNA3 (x:y:z:zs) | x == 'A' && y == 'U' && z == 'G' = decode "AUG" ++" " ++trans zs
						| otherwise = translateRNA3 (zs)


trans [] = []
trans x | makeAminoAcid(take 3 x) /= Nothing = 
											let Just abc = makeAminoAcid(take 3 x)
											in abc ++ " " ++ trans (drop 3 x) 
		| otherwise = " "

-- codon is [char]
--decode :: RNA1 -> RNA1 -> RNA1 -> AminoAcid1 
decode [] = []
decode [x] = []
decode (x:y:xs) | xs == [] = []
decode (x:y:z:zs)|  x=='U' && y=='U' && z =='U' = "Phe"
decode (x:y:z:zs)|  x=='U' && y=='U' && z =='C' = "Phe"
decode (x:y:z:zs)|  x=='U' && y=='U' && z =='A'= "Leu"
decode (x:y:z:zs)|  x=='U' && y=='U' && z =='G' ="Leu"
decode (x:y:z:zs)|  x=='U' && y=='C' = "Ser"
decode (x:y:z:zs)|  x=='U' && y=='A' && z =='U' = "Tyr"
decode (x:y:z:zs)|  x=='U' && y=='A' && z =='C' = "Tyr"
decode (x:y:z:zs)|  x=='U' && y=='A' && z =='A' = "Stop"
decode (x:y:z:zs)|  x=='U' && y=='A' && z =='G' = "Stop"
decode (x:y:z:zs)|  x=='U' && y=='G' && z =='U' = "Cys"
decode (x:y:z:zs)|  x=='U' && y=='G' && z =='C' = "Cys"
decode (x:y:z:zs)|  x=='U' && y=='G' && z =='A' = "Stop"
decode (x:y:z:zs)|  x=='U' && y=='G' && z =='G' = "Trp"
decode (x:y:z:zs)|  x=='C' && y=='U' = "Leu"
decode (x:y:z:zs)|  x=='C' && y=='C'  ="Pro"
decode (x:y:z:zs)|  x=='C' && y=='A' && z =='U' = "His"
decode (x:y:z:zs)|  x=='C' && y=='A' && z =='C' = "His"
decode (x:y:z:zs)|  x=='C' && y=='A' && z =='A' = "Gln"
decode (x:y:z:zs)|  x=='C' && y=='A' && z =='G' = "Gln"
decode (x:y:z:zs)|  x=='C' && y=='G' = "Arg"
decode (x:y:z:zs)|  x=='A' && y=='U' && z =='U' = "Ile"
decode (x:y:z:zs)|  x=='A' && y=='U' && z =='C' = "Ile"
decode (x:y:z:zs)|  x=='A' && y=='U' && z =='A'= "Ile"
decode (x:y:z:zs)|  x=='A' && y=='U' && z =='G' ="Met"
decode (x:y:z:zs)|  x=='A' && y=='C'  ="'Thr"
decode (x:y:z:zs)|  x=='A' && y=='A' && z =='U' = "Asn"
decode (x:y:z:zs)|  x=='A' && y=='A' && z =='C' = "Asn"
decode (x:y:z:zs)|  x=='A' && y=='A' && z =='A' = "Lys"
decode (x:y:z:zs)|  x=='A' && y=='A' && z =='G' = "Lys"
decode (x:y:z:zs)|  x=='A' && y=='G' && z =='U' = "Ser"
decode (x:y:z:zs)|  x=='A' && y=='G' && z =='C'= "Ser"
decode (x:y:z:zs)|  x=='A' && y=='G' && z =='A' ="Arg"
decode (x:y:z:zs)|  x=='A' && y=='G' && z =='G' ="Arg"
decode (x:y:z:zs)|  x=='G' && y=='U' = "Val"
decode (x:y:z:zs)|  x=='G' && y=='C' = "Ala"
decode (x:y:z:zs)|  x=='G' && y=='A' &&z =='U' = "Asp"
decode (x:y:z:zs)|  x=='G' && y=='A' && z =='C' = "Asp"
decode (x:y:z:zs)|  x=='G' && y=='A' && z =='A' = "Glu"
decode (x:y:z:zs)|  x=='G' && y=='A' && z == 'G'= "Glu"
decode (x:y:z:zs)|  x=='G' && y=='G' = "Gly"
				 | otherwise = ""