-- Word Search
import System.Random
import Data.Foldable
import Data.Maybe

--Takes a set of words and generates a Word
generateSearch :: [String] -> IO ()

generateSearch ourwords =
 do
     result <- buildRows ourwords
     putStrLn (filter (`notElem` "\"[],") (addNewlines (show [fst j : "   "|i <- result, j <- i])))

-- Takes a string of characters as an input and splits them into new lines every 140 cahracters.
-- This formats the strings such that the wordsearch will print 20 alphabetic characters each line.
addNewlines :: String -> String
addNewlines [] = []
addNewlines xs  = take 140 xs ++ "\n" ++ addNewlines (drop 140 xs) 


--Takes a set of words and builds a wordsearch with those words placed randomly and random letter filling the rest 
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
   
--Takes a word and a wordseach twice and attempts to place the word in the wordsearch at a random position and orientation. 
--If it fails then it tries again with the original word and wordsearch until it succeeds.
placeWords :: [String] -> [[(Char,Bool)]] -> [String] -> [[(Char,Bool)]] -> IO [[(Char,Bool)]]

placeWords [] ws _ _ = return ws
placeWords ourwords ws ogwords ogws = do
  (isvalid, pos, ori) <- (checkPosValid (head ourwords) (randomInt (length ourwords*111), randomInt ((length (ourwords)+1)*11)) ws)
  if isvalid
    then placeWords (tail ourwords) (placeChars (head ourwords) ws pos ori) ogwords ogws
    else placeWords ogwords ogws ogwords ogws

-- Takes a word, wordsearch and random position and checks if the word can be placed in a random orientation in that position. 
-- Returns a tuple of a boolean if the word succeeded, and the position and orientation it checked. 
checkPosValid :: String -> (Int,Int) -> [[(Char,Bool)]] -> IO (Bool, (Int, Int), (Int, Int))

checkPosValid ourword pos ws =
  let ori = randomOrientation  (fst $ randomR (1, 8) (mkStdGen ((length ourword+5)*111)))
      rowpos = fst pos
      colpos = snd pos
  in if snd (ws !! rowpos !! colpos)
       then do
         let positions = [((i * fst ori) + rowpos, (i * snd ori) + colpos) | i <- [1..length ourword]]
         if all (==True) [checkSafeRow ws rowpos colpos | (rowpos, colpos) <- positions]
           then if (all (==True) [snd(ws!!rowpos!!colpos) | (rowpos,colpos) <-positions]) || (ourword!!colpos == fst(ws!!rowpos!!colpos))
                 then return (True, pos, ori)
                 else return (False, pos, ori)
           else return (False, pos, ori)
         else return (False, pos, ori)

--Takes a wordsearch and a row and column position and returns true if the position and checks if that position is a valid index in the wordsearch
checkSafeRow :: [[(Char,Bool)]] -> Int -> Int -> Bool

checkSafeRow [] rowpos colpos = False
checkSafeRow (row:rows) rowpos colpos = 
    if rowpos == 0
      then checkSafeCol row colpos
      else checkSafeCol row colpos && checkSafeRow rows (rowpos - 1) colpos
  
--Takes a row of the wordsearch and a column position and checks if it is a valid index in that row. 
checkSafeCol :: [(Char,Bool)] -> Int -> Bool
checkSafeCol row colpos = colpos >= 0 && colpos < length row

--Takes a string WordSearch Position and an orientation and replaces the letters in the WordSearch at that position with the letters of the string going the specified orientation
placeChars :: String -> [[(Char,Bool)]] -> (Int, Int) -> (Int, Int) -> [[(Char,Bool)]]

placeChars "" ws _ _ = ws
placeChars ourword ws pos ori = placeChars (tail ourword) (replaceRow ws (fst pos) (replaceEle (ws!!fst pos) (snd pos) (head ourword, False))) (fst pos + fst ori, snd pos + snd ori) ori

--Takes a list, a position, and an element and replaces the element at the position in the list with the provided element
replaceRow :: [[(Char,Bool)]] -> Int -> [(Char,Bool)] -> [[(Char,Bool)]]

replaceRow (h:t) n ele
 | n == 0 = ele:t
 | otherwise = h:replaceRow t (n-1) ele
 
-- Takes a row of the wordsearch, an index number as an integer, and a element and replaces the element at that index with the inputted element.
replaceEle :: [(Char,Bool)] -> Int -> (Char,Bool) -> [(Char,Bool)]

replaceEle (h:t) n ele
 | n == 0 = ele:t
 | otherwise = h:replaceEle t (n-1) ele

--Takes a randomly generated integer and returns a tuple representing an orientation based on that. 
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
  
--Takes an integer and generates a random letter from a to z based on that integer
randomLetter :: Int -> Char

randomLetter i = fst $ randomR ('a', 'z') (mkStdGen i)

--Generates a random number based on an inputted integer
randomInt :: (Random a, Num a) => Int -> a

randomInt i= fst $ randomR (0, 14) (mkStdGen i)

--Takes an integer from 0-5 and returns that many 5 letter words in a list
pickfive :: Int -> [String]

pickfive n 
 | n > 5 = pickfive 5
 | otherwise = take n ["apple beach chess daisy event"]

modifyList :: [String] -> [String]
modifyList xs
  | null xs   = pickfive 5
  | otherwise = xs

-- Main function to get input and generate the word search
main :: IO ()

main = do
  putStrLn "Enter a list of words to include in the word search, separated by commas:"
  input <- getLine
  let list = words input
  let modifiedList = concat (modifyList list)
  let ourwords = words (map (\c -> if c == ',' then ' ' else c) modifiedList)
  generateSearch ourwords
  print ourwords