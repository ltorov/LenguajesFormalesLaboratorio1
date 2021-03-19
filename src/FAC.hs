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

--Check if the automata's start state belongs to its states
startBelongs :: FA Int -> Bool
startBelongs a = Set.member (faStartState a) (faStates a)

--Check if the automata's final states are a subset of its states
finalBelongs :: FA Int -> Bool
finalBelongs a = Set.isSubsetOf (faFinalStates a) (faStates a)

--Check if the states used for the transition belong to the set of states
moveBelongsAux :: Ord a => FA a -> Move a -> Bool
moveBelongsAux a (Move s1 c s2) = Set.member s1 (faStates a)
moveBelongsAux a (Emove s1 s2)  = Set.member s1 (faStates a)

--Goes through a list of moves and checks if all of them use states that belong to the set of states.
moveBelongsList :: Ord a => FA a -> [Move a] -> Bool
moveBelongsList a []     = False
moveBelongsList a [x]    = moveBelongsAux a x
moveBelongsList a (x:xs) = (moveBelongsList a [x]) && (moveBelongsList a xs)

moveBelongs :: Ord a => FA a -> Bool
moveBelongs a = moveBelongsList a (Set.toList(faMoves a))

--Puts together all the functions above and determine if it is a finite automata
isAutomata :: FA Int -> Bool
isAutomata a = emptyStates a && startBelongs a && finalBelongs a && moveBelongs a

--Returns a string that either says which type of finite automata the automata is or gives the reason why its not a finite automata.
printAutomata :: FA Int -> String -> String
printAutomata a str
    | emptyStates a == False  = "The automaton in "++ str ++ " is invalid because it has not states"
    | startBelongs a == False =  "The automaton in "++ str ++ " is invalid because the start state is not a valid state"
    | finalBelongs a == False = "The automaton in "++ str ++ " is invalid because the final states are not a subset of the valid states"
    | moveBelongs a == False  =  "The automaton in "++ str ++ " is invalid because some states used in the transition are not valid states"
    | isAutomata a            = classifyAutomata a str

--FINITE AUTOMATA TYPE
---e-NFA
--Check whether a move is of the type emove by pattern matching
isEmove :: Move a -> Bool
isEmove (Move s1 c s2) = False
isEmove (Emove s1 s2)  = True

--Takes a list of moves and returns a boolean, it works recursively going through a list checking if each move is of the type emove.
isENFAAux :: [Move a] -> Bool
isENFAAux []     = False
isENFAAux [x]    = isEmove x
isENFAAux (x:xs) = isEmove x || isENFAAux xs

--Check if the given automata is an epsilon transition automata.
isENFA :: FA Int -> Bool
isENFA a = isENFAAux (Set.toList(faMoves a))

---NFA
--Prove if it is an NFA 
isNFA :: FA Int ->Bool
isNFA a = (isENFA a == False) && (isDFA a == False)

---DFA
--Compares if when two moves begin in the same state they have the same char
auxIsDFA :: Eq a => Move a -> Move a -> Bool
auxIsDFA (Move s1 c1 s2) (Move s3 c2 s4) = (( s1 == s3) && (c1 == c2)) == False
auxIsDFA (Emove s1  s2) (Move s3 c2 s4)  = False
auxIsDFA (Move s1 c1 s2) (Emove s3 s4)   = False
auxIsDFA (Emove s1 s2) (Emove s3 s4)     = False

--Takes list and compares every pair of two moves
compareMoves :: Eq a => [Move a] ->  Bool
compareMoves []        = False
compareMoves [x]       = True 
compareMoves [x,y]     = auxIsDFA x y
compareMoves (x:y:xs)  = compareMoves [x,y] && compareMoves (x:xs) && compareMoves (y:xs)

--Check if it is a DFA
isDFA :: FA Int -> Bool
isDFA a = compareMoves(Set.toList(faMoves a))

--Classify each finite automata into NFA, DFA and eNFA.
classifyAutomata :: FA Int -> String -> String
classifyAutomata a str
    | isNFA a == True  = "The automaton in " ++ str ++ " is an NFA"
    | isDFA a == True  = "The automaton in " ++ str ++ " is an DFA"
    | isENFA a == True = "The automaton in " ++ str ++ " is an ENFA"

--Reads the input into a FA and takes the name of the file to be read as what the user inputs
main :: IO ()
main = do 
    line <- getLine
    input <- readFile line
    let automata = read input :: FA Int
    print(printAutomata automata line)