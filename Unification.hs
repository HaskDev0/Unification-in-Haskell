{-# LANGUAGE GADTs, DataKinds,
StandaloneDeriving #-}

import qualified GHC.TypeLits as GTL

import Data.Kind
import GHC.Types

--------------------------------------------
--Some ideas/experiments on using the lengthed lists to implement
--a compile time check for the structure of terms

data Z = Z
data S a = S a

data List n a where
    Nil  :: List Z a
    Cons :: a -> List m a -> List (S m) a

data WWTerm where
  WWVar :: String -> WWTerm
  WWFunc :: String -> m -> List m WWTerm -> WWTerm

instance Show WWTerm where
  show (WWVar name) = name
  show (WWFunc name arity inputs) = name ++ "(" ++ (show inputs) ++ ")"

instance Show a => Show (List n a) where
  show Nil = ""
  show (Cons x Nil) = show x
  show (Cons x xs) = show x ++ "," ++ show xs
--------------------------------------------

data Term where
  Var :: String -> Term
  Func :: String -> Integer -> [Term] -> Term
  deriving (Show, Eq, Read)

data SystemU where
  Halt :: SystemU
  UniSet :: [(Term, Term)] -> [(Term, Term)] -> SystemU
  deriving (Show, Eq)

class WW a where
  checkFunction :: a -> Bool

instance WW Term where
  checkFunction (Var _) = True
  checkFunction (Func _ ar args) = case ar == mylength args of
    True -> True
    otherwise -> False

mylength :: [a] -> Integer
mylength [] = 0
mylength (x:xs) = 1 + (mylength xs)

constructFuncTerm :: String -> [Term] -> Term
constructFuncTerm name inputs = Func name (mylength inputs) inputs

constructVarTerm :: String -> Term
constructVarTerm name = Var name

accessTerms :: Term -> [Term]
accessTerms (Func _ _ terms) = terms

accessArityFunc :: Term -> Integer
accessArityFunc (Func _ n _) = n

accessName :: Term -> String
accessName (Var name) = name
accessName (Func name _ _) = name

isFunc :: Term -> Bool
isFunc (Var _) = False
isFunc (Func _ _ _) = True

isVariable :: Term -> Bool
isVariable = not . isFunc

accessP :: SystemU -> [(Term, Term)]
accessP (UniSet a _) = a

accessS :: SystemU -> [(Term, Term)]
accessS (UniSet _ b) = b

decompositionTr :: (Term, Term) -> [(Term, Term)]
decompositionTr tuple@(fs, ft) = zip (accessTerms fs) (accessTerms ft)

decompCheck :: SystemU -> Bool
decompCheck system = a && b && c
  where
    leftP = fst $ head $ accessP system
    rightP = snd $ head $ accessP system
    a = isFunc leftP
    b = isFunc rightP
    c = (accessName leftP) == (accessName rightP)

symbolClashCheck :: SystemU -> Bool
symbolClashCheck system = a && b && c
  where
    leftP = fst $ head $ accessP system
    rightP = snd $ head $ accessP system
    a = isFunc leftP
    b = isFunc rightP
    c = (accessName leftP) /= (accessName rightP)

orientCheck :: SystemU -> Bool
orientCheck system = b && a
  where
    leftP = fst $ head $ accessP system
    rightP = snd $ head $ accessP system
    a = isFunc leftP
    b = isVariable rightP

changeTuple :: (a,b) -> (b,a)
changeTuple (x,y) = (y,x)

variablesOfTerm :: Term -> [Term]
variablesOfTerm v@(Var _) = [v]
variablesOfTerm f@(Func _ _ inputs) = mconcat (map variablesOfTerm inputs)

isVariableInListOfVars :: Term -> [Term] -> Bool
isVariableInListOfVars var [] = False
isVariableInListOfVars var (x:xs) = case var == x of
  True -> True
  otherwise -> isVariableInListOfVars var xs

occursCheck :: SystemU -> Bool
occursCheck system = a && b && c
  where
    leftP = fst $ head $ accessP system
    rightP = snd $ head $ accessP system
    a = isVariable leftP
    b = leftP /= rightP
    c = isVariableInListOfVars leftP (variablesOfTerm rightP)

variableElimCheck :: SystemU -> Bool
variableElimCheck system = a && b
  where
    leftP = fst $ head $ accessP system
    rightP = snd $ head $ accessP system
    a = isVariable leftP
    b = (not . (isVariableInListOfVars leftP)) (variablesOfTerm rightP)

substitute :: (Term, Term) -> Term -> Term
substitute subs v@(Var _) = case v == (fst subs) of
  True -> snd subs
  otherwise -> v
substitute subs f@(Func a b inputs) = Func a b (map (substitute subs) inputs)

substituteTuple :: (Term, Term) -> (Term, Term) -> (Term, Term)
substituteTuple subs terms = (substitute subs (fst terms), substitute subs (snd terms))

--Here we assume that a system is constructed correctly
inferenceSystemU :: SystemU -> SystemU
inferenceSystemU system
  | (fst $ fstFromP) == (snd $ fstFromP) =
    UniSet (tailFromP) (setS)
  | decompCheck system =
    UniSet (mappend (tailFromP) (decompositionTr $ fstFromP)) (setS)
  | symbolClashCheck system = Halt
  | orientCheck system = UniSet ((changeTuple $ fstFromP):(tailFromP)) (setS)
  | occursCheck system = Halt
  | variableElimCheck system = UniSet (map (substituteTuple fstFromP) (tailFromP)) ((fstFromP):(map (substituteTuple fstFromP) (setS)))
  where
    setP = accessP system
    setS = accessS system
    fstFromP = head setP
    tailFromP = tail setP

unification :: SystemU -> SystemU
unification Halt = Halt
unification uniset@(UniSet [] _) = uniset
unification system = unification (inferenceSystemU system)

termExample :: String
termExample = ": t ::= Var \"name\" | Func \"name\" (arity of the function) [t]"

main :: IO ()
main = do
  putStrLn "This is a preliminary version of a planned implementation"
  putStrLn ("Provide the first term to unify in the form" ++ termExample)
  fstTermStr <- getLine
  putStrLn "Provide the second term to unify"
  sndTermStr <- getLine
  let fstTerm = read fstTermStr :: Term
  let sndTerm = read sndTermStr :: Term
  let system = UniSet [(fstTerm, sndTerm)] []
  let answer = unification system
  print answer
