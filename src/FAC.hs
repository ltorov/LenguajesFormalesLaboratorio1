import System.IO --Para leer y escribir archivos
import Data.Typeable
import Data.List ( nub )
import Data.Set ( Set )
import Text.Read (readMaybe)
import qualified Data.Set as Set
import FATypes

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
myLast  [] = error "Empty list."
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
emptyStates a = faStates a /= Set.empty

{- --Check if start state is empty
emptyStartState :: FA Int -> Bool
emptyStartState a = startStateFA a == "" -}

--Check if start state belongs to the states
startBelongs :: FA Int -> Bool
startBelongs a = Set.member (faStartState a) (faStates a)

--Check if final states are a subset of the states
finalBelongs :: FA Int -> Bool
finalBelongs a = Set.isSubsetOf (faFinalStates a) (faStates a)

--Check if the states used for the transtition belong to the set of states
--Todavia no lo quiero hacer ups perdon
moveBelongs :: FA a -> Move a -> Bool
moveBelongs a (Move s1 c s2) = False
moveBelongs a (Emove s1 s2)  = Set.notMember (s1) (faStates a)

--Put together all the booleans and determine if it is a finite automata
isAutomata :: FA Int -> Bool
isAutomata a = emptyStates a && startBelongs a && finalBelongs a

--Check whether a move is of the type emove by pattern matching
isEmove :: Move a -> Bool
isEmove (Move s1 c s2) = False
isEmove (Emove s1 s2)  = True

---Prove if it is and e-NFA (epsilon)
isENFA :: [Move a] -> Bool
isENFA [] = False
isENFA [x] = isEmove x
isENFA (x:xs) = isEmove x || isENFA xs

--Reads the input into a FA
main :: IO ()
main = do 
    input <- readFile "FA2.txt"
    let automaton = read input :: FA Int
    --let movesList = Set.toList(faMoves automaton) --Para correr el isENFA
    --print(isENFA movesList)