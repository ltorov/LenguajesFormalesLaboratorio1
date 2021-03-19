--Joint lab with Gregorio Pérez Bernal and Luisa Toro Villegas
-- GHCi, version 8.10.4 and Stack version: 2.5.1

import System.IO --Para leer y escribir archivos
import Data.Typeable
import Data.List ( nub )
import Data.Set ( Set )
import Text.Read (readMaybe)
import qualified Data.Set as Set
import FATypes

---FINITE AUTOMATA1 
--Check if the automata has any states
emptyStates :: FA Int -> Bool
emptyStates a = faStates a /= Set.empty

reasonEmptyStates :: Bool -> String -> String
reasonEmptyStates False str = "The automaton in "++ str ++ " is invalid because it has not states"

--Check if the automata's start state belongs to its states
startBelongs :: FA Int -> Bool
startBelongs a = Set.member (faStartState a) (faStates a)

reasonStartBelongs :: Bool -> String -> String
reasonStartBelongs False str = "The automaton in "++ str ++ " is invalid because the start state is not a valid state"

--Check if the automata's final states are a subset of its states
finalBelongs :: FA Int -> Bool
finalBelongs a = Set.isSubsetOf (faFinalStates a) (faStates a)

reasonFinalBelongs :: Bool -> String -> String
reasonFinalBelongs False str = "The automaton in "++ str ++ " is invalid because the final states are not a subset of the valid states"

--Check if the states used for the transition belong to the set of states
moveBelongsAux :: Ord a => FA a -> Move a -> Bool
moveBelongsAux a (Move s1 c s2) = Set.member s1 (faStates a)
moveBelongsAux a (Emove s1 s2)  = Set.member s1 (faStates a)

moveBelongsList :: Ord a => FA a -> [Move a] -> Bool
moveBelongsList a []     = False
moveBelongsList a [x]    = moveBelongsAux a x
moveBelongsList a (x:xs) = (moveBelongsList a [x]) && (moveBelongsList a xs)

moveBelongs :: Ord a => FA a -> Bool
moveBelongs a = moveBelongsList a (Set.toList(faMoves a))

reasonMoveBelongs :: Bool -> String -> String
reasonMoveBelongs False str = "The automaton in "++ str ++ " is invalid because some states used in the transition are not valid states"

--Put together all the functions above and determine if it is a finite automata

isAutomata :: FA Int -> Bool
isAutomata a = emptyStates a && startBelongs a && finalBelongs a && (moveBelongs a (Set.toList(faMoves a)))

invalidAutomata :: FA Int -> String -> Bool
invalidAutomata a str
    | (isAutomata a == False), (emptyStates a == True)  = reasonEmptyStates (emptyStates a == True) str
    | (isAutomata a == False), (startBelongs a == True) = reasonStartBelongs (startBelongs a == True) str
    | (isAutomata a == False), (finalBelongs a == True) = reasonFinalBelongs (finalBelongs a == True) str
    | (isAutomata a == False), (moveBelongs a == True)  = reasonMoveBelongs (moveBelongs a == True) str

--FINITE AUTOMATA TYPE
---e-NFA
--Check whether a move is of the type emove by pattern matching
isEmove :: Move a -> Bool
isEmove (Move s1 c s2) = False
isEmove (Emove s1 s2)  = True

-- if it is an e-NFA (epsilon)
isENFAAux :: [Move a] -> Bool
isENFAAux []     = False
isENFAAux [x]    = isEmove x
isENFAAux (x:xs) = isEmove x || isENFAAux xs

isENFA :: FA Int -> Bool
isENFA a = isENFAAux (Set.toList(faMoves a))

printENFA :: Bool -> String -> String
printENFA True str = "The automaton in " ++ str ++ " is an ENFA"

---NFA
--Prove if it is an NFA 
isNFA :: FA Int ->Bool
isNFA a = (isENFA a == False) && (isDFA a == False)

printNFA :: Bool -> String -> String
printNFA True str = "The automaton in " ++ str ++ " is an NFA"
---DFA
--
--Compares if when two moves begin in the same state they have the same char
auxIsDFA :: Eq a => Move a -> Move a -> Bool
auxIsDFA (Move s1 c1 s2) (Move s3 c2 s4) = (( s1 == s3) && (c1 == c2)) == False
auxIsDFA (Emove s1  s2) (Move s3 c2 s4)  = False
auxIsDFA (Move s1 c1 s2) (Emove s3 s4)   = False
auxIsDFA (Emove s1 s2) (Emove s3 s4)     = False

--Takes list and compares every pair of two moves
compareMoves :: Eq a => [Move a] ->  Bool
compareMoves []        = True --si no hay movimientos, es DFA?
compareMoves [x]       = True 
compareMoves [x,y]     = auxIsDFA x y
compareMoves (x:y:xs)  = compareMoves [x,y] && compareMoves (x:xs) && compareMoves (y:xs)

--Check if it is a DFA
isDFA :: FA Int -> Bool
isDFA a = compareMoves(Set.toList(faMoves a))

printDFA :: Bool -> String -> String
printDFA True str = "The automaton in " ++ str ++ " is an DFA"

--Classify
classifyAutomata :: FA Int -> String -> String
classifyAutomata a str
    | isNFA a == True  = printNFA (isNFA a) str
    | isDFA a == True  = printDFA (isDFA a) str
    | isENFA a == True = printENFA (isENFA a) str


--Reads the input into a FA and takes the name of the file to be read as what the user inputs
main :: IO ()
main = do 
    line <- getLine
    input <- readFile line
    let automata = read input :: FA Int
    print(classifyAutomata automata line)
    {- let movesList = Set.toList(faMoves automata) --Para correr el isENFA
    print(isENFA movesList) -}