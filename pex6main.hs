-- pex6.hs 
-- unKnot Haskell

-- name: Sabrina McGarvey

{- DOCUMENTATION: None
-}
checkType1 :: [(Char, Char)] -> Char -> Bool
checkType1 knot previous
  -- base case: reach the end of the knot
  | null knot = False
  -- checks the last letter with the next one, match indicates there is a type 1
  | ((fst(head knot)) == previous) = True
  -- recursive
  | otherwise =  checkType1 (tail knot) (fst(head knot))

-- Uses the logic of the check function but knows it will only have valid inputs so deletes it when it finds the type 1
undoType1 :: [(Char, Char)] -> [(Char, Char)] -> Char -> [(Char, Char)]
undoType1 knot newKnot previous
  | null knot = newKnot
  | ((fst(head knot)) == previous) = undoType1 (tail knot) (take ((length newKnot) - 1) newKnot) ' '
  | otherwise = undoType1 (tail knot) (newKnot ++ (take 1 knot)) (fst(head knot))
  
checkType1Wrap :: [(Char, Char)] -> [(Char, Char)] -> Bool
checkType1Wrap knot findEnd
  | (((length findEnd) == 1) && (fst(head findEnd) == fst(head knot))) = True
  | ((length findEnd) == 1) = False
  | otherwise = checkType1Wrap knot (tail findEnd)
  
undoType1Wrap :: [(Char, Char)] -> [(Char, Char)] -> [(Char, Char)]
undoType1Wrap knot minusEnd
  | ((length knot) == 1) = minusEnd
  | otherwise = undoType1Wrap (tail knot) (minusEnd ++ (take 1 knot))
  
-- Checks for the oo or uu matches
confirmPair :: (Char, Char) -> (Char, Char) -> (Char, Char) -> (Char, Char) -> Bool
confirmPair pairOne pairTwo checkOne checkTwo
  | (snd(checkOne) == snd(checkTwo)) && (snd(pairOne) == snd(pairTwo)) = True
  | otherwise = False
  
-- checks for letter matching on the two pairs
checkPattern :: (Char, Char) -> (Char, Char) -> (Char, Char) -> (Char, Char) -> Bool
checkPattern pairOne pairTwo checkOne checkTwo
  | (fst(pairOne) == fst(checkOne)) && (fst(pairTwo) == fst(checkTwo)) = True
  | (fst(pairOne) == fst(checkTwo)) && (fst(pairTwo) == fst(checkOne)) = True
  | otherwise = False

-- returns any matches
checkPairsHelp :: (Char, Char) -> (Char, Char) -> [(Char, Char)] -> [(Char, Char)] -> [(Char, Char)]
checkPairsHelp pairOne pairTwo knot notChecked
  -- if checking against the pair for the last comparison terminate
  | ((length notChecked == 1) && ((pairOne == (head notChecked)) || (pairTwo == (head notChecked)) || (pairOne == (head knot)) || (pairTwo == (head knot)))) = []
  -- wrap around to the beginning and check
  | ((length notChecked == 1) && (confirmPair pairOne pairTwo (head notChecked) (head knot)) && (checkPattern pairOne pairTwo (head notChecked) (head knot)) ) = [pairOne, pairTwo, (head notChecked), (head knot)]
  -- wrap around didn't work: terminate
  | (length notChecked == 1) = []
  -- check if checking against a pair term and move to next if so
  | ((pairOne == (head notChecked)) || (pairTwo == (head notChecked)) || (pairOne == (head (tail notChecked))) || (pairTwo == (head (tail notChecked)))) = checkPairsHelp pairOne pairTwo knot (tail notChecked)
  -- check set and if found return
  | ((confirmPair pairOne pairTwo (head notChecked) (head (tail notChecked))) && (checkPattern pairOne pairTwo (head notChecked) (head (tail notChecked)))) = [pairOne, pairTwo, (head notChecked), (head (tail notChecked))]
  -- recursive case
  | otherwise = checkPairsHelp pairOne pairTwo knot (tail notChecked)

-- checks the knot using checkPairsHelp
checkPairs :: (Char, Char) -> (Char, Char) -> [(Char, Char)] -> [(Char, Char)] -> Bool -> [(Char, Char)]
checkPairs pairOne pairTwo knot notChecked done
  -- base case: return the list
  | (False == (null (checkPairsHelp pairOne pairTwo knot knot))) = checkPairsHelp pairOne pairTwo knot knot
  -- if the first has been compared to last (done == true) return an empty list
  | (done == True) = []
  -- If the the notChecked is empty wrap to beginning
  | (null notChecked) = checkPairs pairTwo (head knot) knot notChecked True
  -- recursive case: this pair didn't work, move to next two values
  | (null (checkPairsHelp pairOne pairTwo knot knot)) = checkPairs pairTwo (head notChecked) knot (tail notChecked) False
  
-- see if there is a type 2 that can be undone
checkType2 :: [(Char, Char)] -> Bool
checkType2 knot 
  | null knot = False
  | null (getSet knot) = False
  | otherwise = True
  
-- get a list of the set if there is a type 2
getSet :: [(Char, Char)] -> [(Char, Char)]
getSet knot = checkPairs (head knot) (head (tail knot)) knot [] False
  
-- deletes out the 4 that are a part of type 2
undoType2Help :: [(Char, Char)] -> [(Char, Char)] -> (Char, Char) -> (Char, Char) -> (Char, Char) -> (Char, Char) -> [(Char, Char)]
undoType2Help knot newKnot pairOne pairTwo checkOne checkTwo
  | null knot = newKnot
  | ((head knot) == pairOne) = undoType2Help (tail knot) newKnot pairOne pairTwo checkOne checkTwo
  | ((head knot) == pairTwo) = undoType2Help (tail knot) newKnot pairOne pairTwo checkOne checkTwo
  | ((head knot) == checkOne) = undoType2Help (tail knot) newKnot pairOne pairTwo checkOne checkTwo
  | ((head knot) == checkTwo) = undoType2Help (tail knot) newKnot pairOne pairTwo checkOne checkTwo
  | otherwise = undoType2Help (tail knot) (newKnot ++ (take 1 knot)) pairOne pairTwo checkOne checkTwo
  
undoType2 :: [(Char, Char)] -> [(Char, Char)]
undoType2 knot = undoType2Help knot [] (head (getSet knot)) (head (drop 1 (getSet knot))) (head (drop 2 (getSet knot))) (head (drop 3 (getSet knot)))

unKnot :: [(Char, Char)] -> String
unKnot tripCode
  | null tripCode = "not a knot"
  -- if there is a move make it, then call unKnot on reduced tripCode
  | (checkType1 tripCode ' ') = unKnot (undoType1 tripCode [] ' ')
  | (checkType1Wrap tripCode tripCode) = unKnot (undoType1Wrap (tail tripCode) [])
  | (checkType2 tripCode) = unKnot (undoType2 tripCode)
  | otherwise = "tangle – resulting trip code: " ++ (show tripCode)

main :: IO ()
main = do
   let t01 = [('a','o'),('a','u')]
   print("   test case t01 - tripcode: " )
   print(t01)
   print("   result:" ++ unKnot t01)

