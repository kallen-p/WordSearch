-- Word Search
import System.Random
import Data.Foldable
import Data.Maybe

-- | Splits the input string into chunks of length 15 characters and joins them with a newline character.
addNewlineEvery15Chars :: String -> String
addNewlineEvery15Chars [] = [] -- base case: empty string
addNewlineEvery15Chars xs = take 20 xs ++ "\n" ++ addNewlineEvery15Chars (drop 20 xs)

--Takes a set of words and returns a word search with a minimum of 25 characters in the list of searchable words
generateSearch :: [String] -> IO ()

generateSearch [] = generateSearch (randomwords 25)
generateSearch ourwords =
 do
     result <- buildRows ourwords
     putStrLn (addNewlineEvery15Chars (show [fst j |i <- result, j <- i]))

--Takes a set of words and returns a set of rows
buildRows :: [String] -> IO [[(Char,Bool)]]

buildRows ourwords = do
  let numrows = 20
  randWS <- fillWS numrows numrows []
  filledWS <- placeWords ourwords randWS ourwords randWS 
  return filledWS

--Fills a nxn sized wordsearch with random letters 
fillWS :: Int -> Int -> [[(Char,Bool)]] -> IO [[(Char,Bool)]]

fillWS numrows numcols ws =
 if numrows == 0 
  then return ws
  else do
   let newRow =  [(randomLetter (numrows + (i*111)), True) | i <- [1..numcols]]
   fillWS (numrows - 1) numcols (newRow:ws) 
   
--Places the words in the wordsearch
placeWords :: [String] -> [[(Char,Bool)]] -> [String] -> [[(Char,Bool)]] -> IO [[(Char,Bool)]]

placeWords [] ws _ _ = return ws
placeWords ourwords ws ogwords ogws = do
  (isvalid, pos, ori) <- (checkPosValid (head ourwords) (randomInt (length(ourwords)*111), randomInt ((length(ourwords)+1)*11)) ws)
  if isvalid
    then placeWords (tail ourwords) (placeChars (head ourwords) ws pos ori) ogwords ogws
    else placeWords ogwords ogws ogwords ogws

--Checks if a word can be placed in a random orientation given a position, wordlength and a wordsearch.
checkPosValid :: String -> (Int,Int) -> [[(Char,Bool)]] -> IO (Bool, (Int, Int), (Int, Int))

checkPosValid ourword pos ws =
  let ori = randomOrientation  (fst $ randomR (1, 8) (mkStdGen ((length(ourword)+5)*111)))
      rowpos = fst pos
      colpos = snd pos
  in if snd (ws !! rowpos !! colpos)
       then do
         let positions = [((i * (fst ori)) + rowpos, (i * (snd ori)) + colpos) | i <- [1..length(ourword)]]
         if all (==True) [checkSafeRow ws rowpos colpos | (rowpos, colpos) <- positions]
           then if (all (==True) [snd(ws!!rowpos!!colpos) | (rowpos,colpos) <-positions]) || (ourword!!colpos == fst(ws!!rowpos!!colpos))
                 then return (True, pos, ori)
                 else return (False, pos, ori)
           else return (False, pos, ori)
         else return (False, pos, ori)

--Checks if the word will fit vertically and horizontally in the wordsearch
checkSafeRow :: [[(Char,Bool)]] -> Int -> Int -> Bool

checkSafeRow [] rowpos colpos = False
checkSafeRow (row:rows) rowpos colpos = 
    if rowpos == 0
      then checkSafeCol row colpos
      else (checkSafeCol row colpos) && checkSafeRow rows (rowpos - 1) colpos
  
--Checks if the word will fit horizontally in a row of the wordsearch
checkSafeCol :: [(Char,Bool)] -> Int -> Bool
checkSafeCol row colpos = if colpos >= 0 && colpos < length row
                    then True
                    else False

--Takes a string WordSearch Position and an orientation and replaces the letters in the WordSearch at that position with the letters of the string going the specified orientation
placeChars :: String -> [[(Char,Bool)]] -> (Int, Int) -> (Int, Int) -> [[(Char,Bool)]]

placeChars "" ws _ _ = ws
placeChars ourword ws pos ori = placeChars (tail ourword) (replaceRow ws (fst pos) (replaceEle (ws!!(fst pos)) (snd pos) (head ourword, False))) (fst pos + fst ori, snd pos + snd ori) ori

--Takes a list, a position, and an element and replaces the element at the position in the list with the provided element
replaceRow :: [[(Char,Bool)]] -> Int -> [(Char,Bool)] -> [[(Char,Bool)]]

replaceRow (h:t) n ele
 | n == 0 = ele:t
 | otherwise = h:(replaceRow t (n-1) ele)
 
replaceEle :: [(Char,Bool)] -> Int -> (Char,Bool) -> [(Char,Bool)]
replaceEle (h:t) n ele
 | n == 0 = ele:t
 | otherwise = h:(replaceEle t (n-1) ele)

--Randomly choses an orientation
randomOrientation :: Int -> (Int, Int)
randomOrientation n 
    | n == 1 = (1, 1)
    | n == 2 = (-1, 1)
    | n == 3 = (1, 0)
    | n == 4 = (-1, 0)
    | n == 5 = (0, 1)
    | n == 6 = (1, 0)
    | n == 7 = (-1, -1)
    | n == 8 = (0, -1)
    | otherwise =  (0,1)
  
--Generates a random letter from a to z
--randomLetter :: Char
randomLetter i = fst $ randomR ('a', 'z') (mkStdGen i)

--Generates a random number
--randomInt :: Random a => a
randomInt i= fst $ randomR (0, 14) (mkStdGen i)

--Takes in a number of characters that are needed as extra and returns a list of words whose length sum to that integer
randomwords :: Int -> [String]

randomwords 0 = []
randomwords 1 = []
randomwords 2 = []
randomwords n 
 | n `mod` 5 == 0 = pickfive (n `div` 5)
 | n `mod` 5 == 1 = (pickword 6): randomwords (n-6)
 | n `mod` 5 == 2 = (pickword 7): randomwords (n-7)
 | n `mod` 5 == 3 = (pickword 8): randomwords (n-8)
 | n `mod` 5 == 4 = (pickword 9): randomwords (n-9)

--Takes an integer from 6-9 and returns a random word of that length from a list of words
pickword:: Int -> String

pickword n 
 | n < 6 = []
 | n == 6 = "Strong"
 | n == 7 = "Freedom"
 | n == 8 = "Hospital"
 | n == 9 = "Brilliant"
 
--Takes an integer from 0-5 and returns that many 5 letter words in a list
pickfive :: Int -> [String]

pickfive n 
 | n > 5 = pickfive 5
 | otherwise = take n ["apple", "beach", "chess", "daisy", "event"]

-- Main function to get input and generate the word search
main :: IO ()
main = do
  putStrLn "Enter a list of words to include in the word search, separated by commas:"
  input <- getLine
  let ourwords = words (map (\c -> if c == ',' then ' ' else c) input)
  generateSearch ourwords 