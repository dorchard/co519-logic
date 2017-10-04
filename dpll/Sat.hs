-- A short implementation of DPLL in Haskell
module Sat where

import Control.Monad.Trans.Writer.Strict
import Data.Maybe (mapMaybe, fromMaybe)
import Data.Functor ((<$>))
import Data.List (intercalate)

-- CNF representation
type CNF         = [Disjunction]
type Disjunction = [Atom]
data Atom        = Positive String | Negative String deriving (Eq, Show)

-- Results are a list of satisfying assignments
type Results = WriterT [(String, Bool)] []

-- DPLL algorithm
sat :: CNF -> Results Bool
sat formula
 -- Satisfiable (formula reduced to empty set of disjuncts)
 | null formula     = return True

 -- Unsatisfiable (one conjunct is false)
 | any null formula = return False

 | otherwise = do
   -- Step 1: Remove any tautological disjunctions
   formula <- return $ removeTautologies formula
   -- Step 2: Perform unit propagation
   formula <- unitPropagation formula
   -- Step 3: Remove atoms that don't have their opposite in the formula
   formula <- pureLiteralElimination formula
   -- Step 4: Branch the algorithm: try two possible assignments for an atom
   formula <- caseSplitOnAnAtom formula
   sat formula

-- Actions on atoms
neg :: Atom -> Atom
neg (Positive x) = Negative x
neg (Negative x) = Positive x

toBool :: Atom -> Bool
toBool (Positive _) = True
toBool (Negative _) = False

var :: Atom -> String
var (Positive s) = s
var (Negative s) = s

-- *** DPLL step 1 - Replace any tautological clauses (contain x and not x)
removeTautologies :: CNF -> CNF
removeTautologies = filter (not . isTautology)
  where
   isTautology :: Disjunction -> Bool
   isTautology disjs = any (\a -> elem (neg a) disjs) disjs

-- *** DPLL step 2 - unit propagation
-- a clause with a single positive atom must be true, propagate this fact
-- a clause with a single negative atom must be false, propagate this fact
unitPropagation :: CNF -> Results CNF
unitPropagation xs = unitPropagation' [] xs
  where
   unitPropagation' left [] = return left
   unitPropagation' left ([a]:right) = do
     -- Singleton, remember its assignment
     (left', right') <- assignAndUpdate a (toBool a) left right
     -- Drop this singleton clause and apply unit propagation to the rest
     unitPropagation' left' right'
   -- Not a singleton
   unitPropagation' left (r:right) = unitPropagation' (r:left) right

-- *** DPLL step 3 - find any atoms that occur with only one polarity in the
-- the formula, set their assignment and update the formula
pureLiteralElimination :: CNF -> Results CNF
pureLiteralElimination xs = onConjuncts [] xs
  where
    onConjuncts left [] = return left
    onConjuncts left (r:right) = do
     (left', r', right') <- onDisjunction left r right
     case r' of
       Nothing -> onConjuncts left right
       Just r' -> onConjuncts (r':left) right
    onDisjunction :: CNF -> Disjunction -> CNF -> Results (CNF, Maybe Disjunction, CNF)
    onDisjunction left [] right = return (left, Just [], right)
    onDisjunction left (a:as) right
       | not (any (elem (neg a)) left) && not (any (elem (neg a)) right) = do
          (left', right') <- assignAndUpdate a (toBool a) left right
          return (left', Nothing, right')
       | otherwise = do
          (left', as', right') <- onDisjunction left as right
          case as' of
            Nothing  -> return (left', Nothing, right')
            Just as' -> return (left', Just $ a:as', right')

-- **** DPLL step 4 - Split the computation into two non-deterministic paths by
-- picking an atom and assign it to true and false
caseSplitOnAnAtom :: CNF -> Results CNF
caseSplitOnAnAtom ((a:as):rest) =
    amb (assignAndUpdate' a True rest) (assignAndUpdate' a False (as:rest))
  where
    assignAndUpdate' a val rest = uncurry (++) <$> assignAndUpdate a val [] rest
    amb :: Results a -> Results a -> Results a
    amb m n = WriterT $ runWriterT m ++ runWriterT n
caseSplitOnAnAtom cnf = return cnf

-- Helpers for assigning a true to an atom and updating a pair of CNF formula
-- accordingly
assignAndUpdate :: Atom -> Bool -> CNF -> CNF -> Results (CNF, CNF)
assignAndUpdate a val left right = do
    -- Set an assignment for a to match its polarity
    assign a val
    -- Update the rest of the disjuncts with this fact
    let left'  = mapMaybe (replaceAtom a val) left
    let right' = mapMaybe (replaceAtom a val) right
    return (left', right')
  where
    -- Update our assignment of variables
    assign :: Atom -> Bool -> Results ()
    assign (Positive s) b = tell [(s, b)]
    assign (Negative s) b = tell [(s, not b)]

-- Update a disjunction by replacing an atom with a boolean
-- This may trigger the deletion of the disjunction (i.e., makes it true)
-- which is represented by Nothing
replaceAtom :: Atom -> Bool -> Disjunction -> Maybe Disjunction
replaceAtom a _ [] = Just []
replaceAtom a new (a':as)
  -- satisfied a disjunct
  | var a == var a' && polarise a a' new = Nothing
  -- Matching but replace with false, so delete an atom
  | var a == var a' = replaceAtom a new as
  | otherwise       = replaceAtom a new as >>= (\as' -> return $ a' : as')
  where
   polarise (Positive _) (Negative _) b = not b
   polarise (Negative _) (Positive _) b = not b
   polarise _            _            b = b

-- Helper that tests the assignment (for confidence in the algorithm)
test :: CNF -> Results Bool -> Bool
test cnf results =
   all checkAssignment (runWriterT results)
  where
    checkAssignment (False, _) = True
    checkAssignment (True, assignment) =
      and . map or . map (map (replaceWithBool assignment)) $ cnf
    replaceWithBool assignment a =
       fromMaybe True (lookup (var a) assignment)

-- Helper that pretty prints the result of the SAT algorithm
pretty :: Results Bool -> IO ()
pretty results =
    if all fst results'
      then do
        putStrLn "Satisfiable."
        putStrLn $ "\n" ++ "Satisfying assignments: "
        mapM_ (putStrLn . prettyAssigns . snd) results'
      else putStrLn "Unsatisfiable."
  where
    results' = runWriterT results
    prettyAssigns assgns =
      "\t" ++ intercalate ", " (map (\(s, b) -> s ++ "=" ++ show b) assgns)