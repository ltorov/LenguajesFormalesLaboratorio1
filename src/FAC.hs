import System.IO --Para leer y escribir archivos
import Data.Typeable
import Data.List ( nub )
import Data.Set ( Set )
import Text.Read (readMaybe)
import qualified Data.Set as Set

-- | Moves (or transitions) data type.
data Move a = Move a Char a | Emove a a
           deriving (Eq, Ord, Show, Read)

-- | Finite automata data type.
data FA a  = FA { statesFA      :: Set a
                , movesFA       :: Set (Move a)
                , startStateFA  :: a
                , finalStatesFA :: Set a
                }
          deriving (Show, Read, Eq)

-- | 'alphabetFA' returns the alphabet of the finite automata, by
--   finding a list of the characters mentioned in the moves.
alphabetFA :: FA a -> [Char]
alphabetFA (FA _ moves _ _) = nub [ c | Move _ c _ <- Set.toList moves ]

--Auxiliar method to separate a string into an array
--Taken from: https://codereview.stackexchange.com/questions/6992/approach-to-string-split-by-character-in-haskell
groupBy :: String -> Char -> String -> [String]
groupBy ""     _ "" = []
groupBy ""     _ r  = [r]
groupBy (x:xs) c ""
    | x == c        = groupBy xs c ""
    | otherwise     = groupBy xs c [x]
groupBy (x:xs) c r
    | x == c        = r : groupBy xs c ""
    | otherwise     = groupBy xs c (r ++ [x])

-- AUXILIARY FUNCTIONS:

--Extract the last element of a list, which must be finite and non-empty. The function generates an error if the list is empty.
myLast :: [a] -> a
myLast [] = error "Empty list."
myLast [x] = x
myLast (x : xs) = myLast xs

--Removes the last element from a list.
pop :: [a] -> [a]
pop [] = error "Empty list."
pop xs = init xs

--String to list
stringToList :: String -> [String]
stringToList [] = []
stringToList a = groupBy a ',' []

--NON AUXILIARY METHODS
---Prove that it is a finite automata

--Check if there are states
emptyStates :: FA Int -> Bool
emptyStates a = statesFA a /= Set.empty

{- --Check if start state is empty
emptyStartState :: FA Int -> Bool
emptyStartState a = startStateFA a == "" -}

--Check if start state belongs to the states
startBelongs :: FA Int -> Bool
startBelongs a = Set.notMember (startStateFA a)  (statesFA a)

--Check if final states are a subset of the states
finalBelongs :: FA Int -> Bool
finalBelongs a = Set.isSubsetOf (finalStatesFA a) (statesFA a)

--Check if the states used for the transtition belong to the set of states
--Todavia no lo quiero hacer ups perdon

--Put together all the booleans and determine if it is a finite automata
isAutomata :: FA Int -> Bool
isAutomata a = emptyStates a && startBelongs a && finalBelongs a

---Prove if it is and e-NFA (epsilon)
isENFA :: FA Int -> Bool
isENFA a = Set.isSubsetOf (movesFA) ()


--Reads the input into a FA
main :: IO ()
main = do 
    input <- readFile "FA4.txt"
    let automaton = read input :: FA Int
    print(finalBelongs automaton)

    
     
    
   


   
  



