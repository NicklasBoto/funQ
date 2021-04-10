module HM2 where

import qualified Data.Set as Set
import qualified Data.Map as Map
import AST.AST
import Control.Monad.State

-- | A constraint could be that a type is a subtype of anothor type. Or that a type variable is
-- not linear. 
data Constraint = Subtype Type Type | NotLinear Type deriving (Eq, Ord, Show)

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
inferExp prog = do
    let [Func _ _ term] = run ("f : a f = " ++ prog)
    evalState (infer 0 Map.empty term) 0

solveExp :: String -> Maybe Type
solveExp prog = resolve type' constraints
    where
        (type', constraints) = inferExp prog

-- | Takes a type and a set of constraints and finds a possible type satisfying the constraints, 
--   or none if nothing was found.
resolve :: Type -> Set.Set Constraint -> Maybe Type
resolve type' constraints = case resolveAll type' constraints of
    []   -> Nothing 
    x:xs -> Just x

-- | Takes a type and a set of constraints and finds all possible types satisfying the constraints.
resolveAll :: Type -> Set.Set Constraint -> [Type]
resolveAll type' constraints 
    | Set.null simpleConstraints = [type']
    | otherwise = concatMap (uncurry resolveAll) paths
    where
        simpleConstraints = simplifyConstraints constraints
        (constraint, rest) = Set.deleteFindMin simpleConstraints
        substs = resolveConstraint type' constraint

        paths :: [(Type, Set.Set Constraint)]
        paths = map (\subst -> (apply subst type', apply subst rest)) substs
    
class Appliable a where
    apply :: Subst -> a -> a

instance Appliable Type where
    apply subst (a :=> b)   = apply subst a :=> apply subst b
    apply subst (TypeVar a) = Map.findWithDefault (TypeVar a) a subst
    apply subst a           = a

instance (Ord a, Appliable a) => Appliable (Set.Set a) where
    apply subst set = Set.map (apply subst) set

instance Appliable Constraint where
    apply subst (Subtype a b) = Subtype (apply subst a) (apply subst b)
    apply subst (NotLinear a) = NotLinear (apply subst a)

type Subst = Map.Map String Type

nullSubst :: Subst
nullSubst = Map.empty

bind :: String -> Type -> Subst
bind = Map.singleton

-- | Takes a type and a constraint and constructs possible substitutions satisfying the constraint.
resolveConstraint :: Type -> Constraint -> [Subst]
resolveConstraint type' (Subtype TypeBit (TypeVar a)) = [bind a TypeBit]
resolveConstraint type' (Subtype TypeBit TypeBit)     = [nullSubst] 
resolveConstraint type' (Subtype (a :=> b) (c :=> d))  = undefined

-- | Simplifies constraints.
-- Such that !a<:b simplified to a<:b
simplifyConstraints :: Set.Set Constraint -> Set.Set Constraint
simplifyConstraints = Set.concatMap f
    where
        f (Subtype (TypeDup a) b) = Subtype a b -- todo: add recursive cases
        f (Subtype (a :=> b) (c :=> d)) = undefined -- c <:a, b<:d
        f a = a
