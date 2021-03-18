import System.IO --Para leer y escribir archivos
import Data.Typeable
import Data.List ( nub )
import Data.Set ( Set )
import Text.Read (readMaybe)
import qualified Data.Set as Set
import FATypes

---FINITE AUTOMATA
--Check if the automata has any states
emptyStates :: FA Int -> Bool
emptyStates a = faStates a /= Set.empty

--Check if the automata's start state belongs to its states
startBelongs :: FA Int -> Bool
startBelongs a = Set.member (faStartState a) (faStates a)

--Check if the automata's final states are a subset of its states
finalBelongs :: FA Int -> Bool
finalBelongs a = Set.isSubsetOf (faFinalStates a) (faStates a)

{- --Check if the states used for the transition belong to the set of states
moveBelongs :: FA a -> Move a -> Bool
moveBelongs a (Move s1 c s2) = Set.notMember s1 (faStates a)
moveBelongs a (Emove s1 s2)  = Set.notMember s1 (faStates a) -}

--Put together all the functions above and determine if it is a finite automata
isAutomata :: FA Int -> Bool
isAutomata a = emptyStates a && startBelongs a && finalBelongs a

--FINITE AUTOMATA TYPE
---e-NFA
--Check whether a move is of the type emove by pattern matching
isEmove :: Move a -> Bool
isEmove (Move s1 c s2) = False
isEmove (Emove s1 s2)  = True

-- if it is an e-NFA (epsilon)
isENFA :: [Move a] -> Bool
isENFA []     = False
isENFA [x]    = isEmove x
isENFA (x:xs) = isEmove x || isENFA xs

---NFA
{- 
--Prove if it is an NFA 
isNFA :: FA Int ->Bool
isNFA a = (isENFA (Set.toList(faMoves a)) == False) && (isDFA a == False)

---DFA
--
--Compares if when two moves begin in the same state they have the same char
auxIsDFA :: Move a -> Move a -> Bool
auxIsDFA (Move s1 c1 s2) (Move s3 c2 s4) = ( s1 == s3 && c1 == c2 ) 

--Takes list and compares every pair of two moves
compareMoves :: [Move a] ->  Bool
compareMoves []        = True --si no hay movimientos, es DFA?
compareMoves [x]       = True 
compareMoves [x,y]     = auxIsDFA x y
compareMoves (x:y:xs)  = (auxIsDFA x y) && compareMoves (x:xs) && compareMoves (y:xs)

--Check if it is a DFA
isDFA :: FA Int -> Bool
isDFA a = compareMoves(Set.toList(faMoves a))
 -}

--Reads the input into a FA and takes the name of the file to be read as what the user inputs
main :: IO ()
main = do 
    line <- getLine
    input <- readFile line
    let automaton = read input :: FA Int
    print(isAutomata automaton)
    {- let movesList = Set.toList(faMoves automaton) --Para correr el isENFA
    print(isENFA movesList) -}