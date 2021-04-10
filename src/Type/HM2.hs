{-# LANGUAGE FlexibleInstances #-}

module HM2 where

import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Map.Merge.Lazy
import Debug.Trace
import AST.AST
import Control.Monad.State
import Data.Maybe
import Data.List

-- | A constraint could be that a type is a subtype of anothor type. Or that a type variable is
-- not linear. 
data Constraint = Subtype Type Type | NotLinear Type deriving (Eq, Ord)

instance Show Constraint where
    show (Subtype a b) = show a ++ " <: " ++ show b
    show (NotLinear a) = "NotLinear " ++ show a

-- a : Bit !Bit
-- b : Bit !Bit
-- c : Bit !Bit

-- [a = !Bit, b = Bit, c = !Bit, d = Bit]
-- [a = !Bit, b = !Bit, c = !Bit,  d = Bit]
-- [a = !Bit, b = !Bit, c = !Bit, d = !Bit]



-- | A type environment keeping track of all global function types and bound variable types.
type TypeEnv = Map.Map Named Type
-- | The TypeEnvronment consists of named things.
data Named = Bound Integer | FunName String deriving (Eq, Ord)

-- | Infer the type of the term together with some constraints.
infer :: Integer -> TypeEnv -> Term -> Infer (Type, Set.Set Constraint)
infer _ _ Unit              = return (TypeUnit, Set.empty)
infer _ _ Meas              = return (TypeDup (TypeQBit :=> TypeDup TypeBit), Set.empty)
infer _ _ New               = return (TypeDup (TypeBit :=> TypeQBit), Set.empty)
infer _ _ (Bit _)           = return (TypeDup TypeBit, Set.empty)
infer _ _ (Gate g)          = return (inferGate g, Set.empty)
infer _ env (Fun f)         = return (inferFun f env, Set.empty)
infer absl env (Idx i)      = return (inferBound absl env i, Set.empty)
infer absl env (IfEl c t f) = inferIfEl absl env c t f
infer absl env (Abs body)   = inferAbs absl env body
infer absl env (App f arg)  = inferApp absl env f arg

-- | Infer type of a Gate
inferGate :: Gate -> Type
inferGate g = TypeDup (argT :=> argT)
    where
        numQbits = case g of
            GFRDK -> 3
            GTOF  -> 3
            GSWP  -> 2
            GCNOT -> 2
            _     -> 1
        
        argT = foldr (:><) TypeQBit (replicate (numQbits-1) TypeQBit)

-- | Infer type of a function. Throw error if function not in scope.
inferFun :: String -> TypeEnv -> Type
inferFun fun env = case Map.lookup (FunName fun) env of
    Nothing    -> error $ "Fun " ++ fun ++ " not found in environment"
    Just type' -> type'

-- | Infer type of a bound variable. Should never end up throwing error.
inferBound :: Integer -> TypeEnv -> Integer -> Type
inferBound absl env i = case Map.lookup (Bound (absl-i-1)) env of
    Nothing    -> error $ "De Bruijn " ++ show i ++ " not found in environment"
    Just type' -> type'

inferIfEl ::  Integer -> TypeEnv -> Term -> Term -> Term -> Infer (Type, Set.Set Constraint)
inferIfEl absl env c t f = do
    retType <- freshTypeVar
    (cType, cConstrainst) <- infer absl env c 
    (tType, tConstraints) <- infer absl env t
    (fType, fConstraints) <- infer absl env f

    let cConstraint = Subtype cType TypeBit
        tConstraint = Subtype tType retType
        fConstraint = Subtype fType retType
        constraints = Set.unions  [
                Set.fromList [cConstraint, tConstraint, fConstraint],
                tConstraints, tConstraints, fConstraints]

    return (retType, constraints)

inferAbs :: Integer -> TypeEnv -> Term -> Infer (Type, Set.Set Constraint)
inferAbs absl env body = do
    argType <- freshTypeVar
    let env' = addToEnv argType absl env
    (bodyType, bodyConstraints) <- infer (absl+1) env' body
    let constraints = if headCount body > 1 then Set.insert (NotLinear argType) bodyConstraints else bodyConstraints
    return (argType :=> bodyType, constraints)

inferApp :: Integer -> TypeEnv -> Term -> Term -> Infer (Type, Set.Set Constraint)
inferApp absl env f arg = do
    (fType, fConstraints)     <- infer absl env f
    (argType, argConstraints) <- infer absl env arg

    fArg <- freshTypeVar
    fRet <- freshTypeVar
    let fConstraint = Subtype fType (fArg :=> fRet)
        argConstraint = Subtype argType fArg
        constraints = Set.unions [Set.fromList [fConstraint, argConstraint], fConstraints, argConstraints]

    return (fRet, constraints)


-- | Find how many times the head variable x is referenced in the term A. (\x.A)
headCount :: Term -> Integer
headCount = f 0
    where
        f :: Integer -> Term -> Integer
        f absl (Idx i)                = if i == absl then 1 else 0
        f absl (Abs e)                = f (absl+1) e
        f absl (App l r)              = f absl l + f absl r
        f absl (IfEl cond true false) = f absl cond + max (f absl true) (f absl false)
        f _ _                         = 0

-- | Add  a lambda variable type to the environment at an abstraction level.
addToEnv :: Type -> Integer -> TypeEnv -> TypeEnv
addToEnv type' absl = Map.insert (Bound absl) type'

freshTypeVar :: Infer Type
freshTypeVar = do
    freshCount <- get
    let type' = TypeVar (name freshCount)
    put (freshCount + 1)
    return type'

name :: Integer -> String
name i =  letters !! fromInteger i
    where
        letters = [1..] >>= flip replicateM ['a'..'z']

-- | Integer keeps track on what the next fresh variable should have for name.
type InferState = Integer

-- | Type inference needs some state.
type Infer a = State InferState a

inferExp :: String -> (Type, Set.Set Constraint)
inferExp prog = evalState (infer 0 Map.empty term) 0
    where [Func _ _ term] = run ("f : a f = " ++ prog)


solveExp :: String -> [Type]
solveExp prog = resolve type' constraints
    where
        (type', constraints) = inferExp prog


-- | Takes a type and a set of constraints and finds all possible types satisfying the constraints.
resolve :: Type -> Set.Set Constraint -> [Type]
resolve type' constraints = trace ("Solutions: " ++ show solutions) $ nub $ map (applySolution type') solutions
    where
        solutions = resolveConstraints (Set.toList constraints)

        applySolution :: Type -> Solution -> Type
        applySolution type' solution = apply solution type'-- appundefined
    
class Appliable a where
    apply :: Solution -> a -> a

instance Appliable Type where
    apply subst (a :=> b)   = apply subst a :=> apply subst b
    apply subst (TypeVar a) = Map.findWithDefault (TypeVar a) a subst
    apply subst a           = a

instance (Ord a, Appliable a) => Appliable (Set.Set a) where
    apply subst set = Set.map (apply subst) set

instance (Ord a, Appliable a) => Appliable ([] a) where
    apply subst set = map (apply subst) set

instance Appliable Constraint where
    apply subst (Subtype a b) = Subtype (apply subst a) (apply subst b)
    apply subst (NotLinear a) = NotLinear (apply subst a)

instance Appliable Solution where
    apply sol1 sol2 = Map.map (apply sol1) sol2 `Map.union` sol1

-- compose :: Subst -> Map.Map TVar Type -> Map.Map TVar Type
-- s2 `compose` s1 = Map.map (apply s2) s1 `Map.union` s2
type Solution = Map.Map String Type

nullSolution :: Solution
nullSolution = Map.empty

bind :: String -> Type -> Solution
bind = Map.singleton
-- NotLinear (TypeVar "a")
-- a<:!TypeBit
-- 
-- a blir till !TypeBit

-- Notlinear a, a = QBit -> errror 
-- Notlinear TypeVar a -> do
        -- t <- lookup a 
        --bind a TypeDup t

-- 1. Find type with SubtypeConstraints
-- 2. Check type satisfies NotLinear constraints.
-- 
-- !(Bit ⊸ QBit) <: a ⊸ d


-- !(a -o b) <: c -o d
-- c <: a
-- b <: d
-- Nonlinear (a -o d)


-- !a <: b
-- a = b eller !a = b  ?

-- a <: b
-- ---------
-- !a <: b

-- add constraint a<:b and a solution a=b

-- 
-- | Takes a type and a constraint and constructs possible substitutions satisfying the constraint.
resolveConstraint :: Constraint -> Either [Solution] [Constraint]
resolveConstraint (NotLinear a)                           = Left [nullSolution] -- Handle this after.
resolveConstraint (Subtype (TypeDup a) (TypeVar b))       = Left [bind b a, bind b (TypeDup a)]
resolveConstraint (Subtype a (TypeVar b))                 = Left [bind b a]
resolveConstraint (Subtype (TypeVar a) (TypeDup b))       = Left [bind a (TypeDup b)]
resolveConstraint (Subtype (TypeVar a) b)                 = Left [bind a b, bind a (TypeDup b)]
resolveConstraint (Subtype (TypeDup a) b)                 = undefined--Right [Subtype (TypeDup a) (TypeDup b)]
resolveConstraint (Subtype (a :=> b) (c :=> d))           = Right [Subtype c a, Subtype b d]
resolveConstraint (Subtype (a :>< b) (a':>< b'))          = Right [Subtype a a', Subtype b b']
resolveConstraint (Subtype t1 t2) | t1 <: t2, isConstType t1, isConstType t2 = Left [nullSolution]
resolveConstraint (Subtype t1 t2) | t2 <: t1, isConstType t1, isConstType t2 = Left []
resolveConstraint const   = error ("Can't resolve: " ++ show const)

resolveConstraints :: [Constraint] -> [Solution]
resolveConstraints [] = [nullSolution]-- undefined--foldr (composeAll . resolveConstraint) [nullSolution]
resolveConstraints (c:cs) = case resolveConstraint c of 
        Left sols -> apply (last sols) $ concatMap (\sol -> resolveConstraints (apply sol cs)) sols
        Right cs' -> resolveConstraints (cs ++ cs')

(<:) :: Type -> Type -> Bool
TypeDup  a     <: TypeDup  b   = TypeDup a <: b
TypeDup  a     <:          b   = a <: b
(a1 :>< a2)    <: (b1 :><  b2) = a1 <: b1 && a2 <: b2
(a' :=>  b)    <: (a :=>   b') = a  <: a' && b  <: b'
a              <:          b   = a == b

-- | Checks if a type is a constant type
isConstType :: Type -> Bool
isConstType TypeBit = True
isConstType TypeQBit = True
isConstType TypeUnit = True
isConstType (TypeDup t) = isConstType t
isConstType _type = False
-- a = Bit
-- b = c

-- a = !Bit

-- Solution
-- compose [solution] [solution] = [solution]
-- compose [sol1, sol2] [sol3] = [sol1 med sol3, sol2 med sol3] 
-- 

-- subst [{a}, {b}]
-- subst [{c}, {d}}]
-- subst [{ac}, {ad}, {bc}, {bd}]

-- s2 = {a=Bit} 
-- s1 = {a=b}
-- ===========
-- s = {a=Bit, b=Bit}

-- s2 = {a=Bit}   -- \x . if 1 then 0 else 0 -- Bit, !Bit 
-- s1 = {a=!Bit}  -- ´(\ x. if x then x else 0) 0 -- !Bit
-- ===========
-- s = Nothing ??

-- s2 = {a=(Bit, b)} 
-- s1 = {a=b}
-- ===========
-- s = ...

-- while true 
-- 1. Lös 1 constraint
-- 2. Uppdatera constraints med lösning.
-- 3. Se 1

-- | Compose two solutions into one
compose :: Solution -> Solution -> Maybe Solution
s2 `compose` s1 =  if any isNothing ts then Nothing else Just $ Map.fromList $ zip ss $ catMaybes ts
    where f _ a b = if a == b then Just a else Nothing
          preserve = mapMissing (\_ x -> Just x)
          (ss,ts) = unzip $ Map.toList $ merge preserve preserve (zipWithMatched f) s2 s1
          
-- | Composes two lists of solutions, to a new solution were we have a solution from both lists.
composeAll :: [Solution] -> [Solution] -> [Solution]
composeAll sols1 sols2 = catMaybes [compose sol1 sol2 | sol1 <- sols1, sol2 <- sols2 ]
          --[a = Bit, a = !Bit] [a = Bit]
          --[a = Bit, a = Bit] ///// [a=!Bit, a=Bit]
          --Just [a=Bit], Nothing
          -- a=Bit


comp :: Solution -> Constraint -> Solution
comp = undefined

-- rcompose :: Resolver -> Resolver -> Resolver
-- r2 `rcompose` r1 = Map.map (f r2) r1 `Map.union` r2
--     where
--         f :: Resolver -> ResolveAction -> ResolveAction
--         f r (Rename id) = case Map.lookup id r of
--             Just act -> act
--             Nothing  -> Rename id
--         f r action = action

-- !Bit <: b
-- a -o a <: b -o c

-- [a -o b <: c -o d]
-- [c <: a] comp [b <: d] --> 

-- 1. [b/Bit],                          [b/!Bit]
-- 2. ->  [b <: a och a <: c]
-- 3. apply [b/Bit] [b <: a och a <: c]
-- 3. [Bit <: a och a <: c]
-- 4. [a/Bit]
-- 5. apply [a/Bit] [a<:c]
-- 6. -> [Bit<:c]
-- 7. [c/Bit] solution 1

-- apply [b/!Bit] [b <: a och a <: c]
-- -> [!Bit <: a och a<: c]
-- [a/Bit] [a/!Bit]
-- apply [a/Bit] [a <: c]
-- [Bit <: c]
-- [c/Bit] solution 2

-- apply [a/!Bit] [a <: c]
-- [!Bit <: c]
-- [c/Bit] [c/!Bit] solution 3

-- Set.unions [c/Bit] [c/Bit] [c/Bit] [c/!Bit]
-- Resulting solution: [c/Bit] [c/!Bit]
-- Resulting solution: [] -> throwError 

-- [TypeVar "a" <: TypeVar "b"] 
-- keep the constraint.

-- [b/a]

-- [!a <: b] 
-- [b/a] [b/!a]


-- [a <: a]  [a<:!a]

-- Bit <: QBit -> []


-- [b = Bit, a = Bit, c = Bit]
-- [b = !Bit, a = Bit, c = Bit]
-- [b = !Bit, a = !Bit, c = Bit]
-- [b = !Bit, a = !Bit, c = !Bit]





-- [b = Bit, a = Bit, c = Bit]
-- [b = !Bit, a = Bit, c = Bit]
-- [b = !Bit, a = !Bit, c = Bit]
-- [b = !Bit, a = !Bit, c = !Bit]

