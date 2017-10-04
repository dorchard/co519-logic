module Sat where

import Control.Monad.Trans.Writer.Strict
import Data.Maybe (mapMaybe)
import Data.Functor ((<$>))
import Data.List (intercalate)

-- CNF representation
type CNF         = [Disjunction]
type Disjunction = [Atom]
data Atom = Positive String | Negative String deriving (Eq, Show)

-- Results are a list of satisfying assignments
type Results = WriterT [(String, Bool)] []

sat :: CNF -> Results Bool
sat formula
 -- Satisfiable (formula reduced to empty set of disjuncts)
 | null formula     = return True

 -- Unsatisfiable (one conjunct is false)
 | any null formula = return False

 | otherwise = do
   formula <- unitPropagation . removeTautologies $ formula
   formula <- pureLiteralElimination formula
   formula <- split formula
   sat formula

-- Actions on atoms, negative and conversion to bool
neg :: Atom -> Atom
neg (Positive x) = Negative x
neg (Negative x) = Positive x

toBool :: Atom -> Bool
toBool (Positive _) = True
toBool (Negative _) = False

var :: Atom -> String
var (Positive s) = s
var (Negative s) = s

-- Update our assignment of variables
assign :: Atom -> Bool -> Results ()
assign (Positive s) b = tell [(s, b)]
assign (Negative s) b = tell [(s, not b)]

split :: CNF -> Results CNF
split ((a:as):rest) = do
    amb (assignAndUpdate' a True rest) (assignAndUpdate' a False (as:rest))
  where
    assignAndUpdate' a val rest = uncurry (++) <$> assignAndUpdate a val [] rest
    amb :: Results a -> Results a -> Results a
    amb m n = WriterT $ runWriterT m ++ runWriterT n

split cnf = return cnf

assignAndUpdate :: Atom -> Bool -> CNF -> CNF -> Results (CNF, CNF)
assignAndUpdate a val left right = do
  -- Set an assignment for a to match its polarity
  assign a val
  -- Update the rest of the disjuncts with this fact
  let left'  = mapMaybe (replaceAtom a val) left
  let right' = mapMaybe (replaceAtom a val) right
  return (left', right')

-- DPLL step 1 - Replace any tautological disjunctions (contain x and not x)
removeTautologies :: CNF -> CNF
removeTautologies = filter (not . isTautology)
  where
   isTautology :: Disjunction -> Bool
   isTautology disjs = any (\a -> elem (neg a) disjs) disjs

-- DPLL step 2 - unit propagation
-- a disjunction with a single positive atom must be true, propagate this fact
-- a disjunction with a single negative atom must be false, propagate this fact

unitPropagation :: CNF -> Results CNF
unitPropagation xs = unitPropagation' [] xs
  where
   unitPropagation' left [] = return left
   unitPropagation' left ([a]:right) = do
     -- Singleton, remember its assignment
     (left', right') <- assignAndUpdate a (toBool a) left right
     -- Drop this singleton disjunct and apply unit propagation to the rest
     unitPropagation' left' right'
   -- Not a singleton
   unitPropagation' left (r:right) = unitPropagation' (r:left) right

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
  | otherwise      = replaceAtom a new as >>= (\as' -> return $ a' : as')
  where
   polarise (Positive _) (Negative _) b = not b
   polarise (Negative _) (Positive _) b = not b
   polarise _            _            b = b

{-
   (Positive s) True (Negative s) = False
   (Positive s) False (Negative s) = True
   (Positive s) True (Positive s) = True
   (Positive s) False (Positive s) = False

   (Negative s) True (Positive s) = False
   (Negative s) False (Positive s) = True
   (Negative s) True (Negative s) = True
   (Negative s) False (Negative s) = False -}

-- DPLL step 3 - find any atoms that occur with only one polarity in the
-- the formula, set their assignment and update the formula
pureLiteralElimination :: CNF -> Results CNF
pureLiteralElimination xs = dropPureLiteral [] xs

dropPureLiteral left [] = return left
dropPureLiteral left (r:right) = do
  (left', r', right') <- dropPureLiteral' left r right
  case r' of
    Nothing -> dropPureLiteral left right
    Just r' -> dropPureLiteral (r':left) right

dropPureLiteral' :: CNF -> Disjunction -> CNF -> Results (CNF, Maybe Disjunction, CNF)
dropPureLiteral' left [] right = return (left, Just [], right)
dropPureLiteral' left (a:as) right
   | not (any (elem (neg a)) left) && not (any (elem (neg a)) right) = do
    (left', right') <- assignAndUpdate a (toBool a) left right
    return (left', Nothing, right')

  | otherwise = do
    (left', as', right') <- dropPureLiteral' left as right
    case as' of
      Nothing -> return (left', Nothing, right')
      Just as' -> return (left', Just $ a:as', right')

-- Test the assignment (for confidence in the algorithm)
test :: CNF -> Results Bool -> Bool
test cnf results =
   all checkAssignment (runWriterT results)
  where
    checkAssignment (False, _) = True
    checkAssignment (True, assignment) =
      and . (map or) . map (map (replaceWithBool assignment)) $ cnf
    replaceWithBool assignment a =
       case lookup (var a) assignment of
         Nothing -> True -- can be anything
         Just b  -> b

pretty :: Results Bool -> IO ()
pretty results = do
    if (all fst results')
      then do
        putStrLn "Satisfiable."
        putStrLn $ "\n" ++ "Satisfying assignments: "
        mapM_ (putStrLn . prettyAssigns . snd) results'
      else putStrLn "Unsatisfiable."
  where
    results' = runWriterT results
    prettyAssigns assgns =
      "\t" ++ intercalate ", " (map (\(s, b) -> s ++ "=" ++ show b) assgns)