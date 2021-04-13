{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}

module Type.HM where

import AST.AST
import Data.String
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.State
import Control.Monad.Except
import Parser.Print
import Data.List (nub, intercalate)
import Data.Bifunctor (first)
import Debug.Trace
import System.IO.Unsafe


-- | Name functions and bound variables are represented by Named.
data Named
    = NFun String
    | Bound Integer
    deriving (Eq, Ord, Show)

-- | A scheme is a polymorphic type. Has a list of bound type variables together with a monotype.
data Scheme = Forall [TVar] Type

-- | A type variable representing any type.
type TVar = String

instance Show Scheme where
    show (Forall [] t) = printTree (reverseType t)
    show (Forall vs t) = "∀ " ++ unwords (map show vs) ++ " . "
                      ++ printTree (reverseType t)

-- | Each variable in the TypeEnv has an associated type Scheme.
--   Could be a function in outer scope, or a bound variable.
type TypeEnv = Map.Map Named Scheme

-- | A substitution is a map from a type variable to the type it 
--  should be substituted with. It could be another type variable.
type Subst = Map.Map TVar Type

showMap :: (Show a, Show k) => Map.Map k a -> String
-- showMap = undefined
showMap = intercalate "," . map elem . Map.toList
    where elem (from, to) = show from ++ "/" ++ show to

instance {-# OVERLAPPING #-} Show Subst where
    show s = "[" ++ showMap s ++ "]"

-- | A ResolveAction contains three actions that can be made with a TypeFlax.
--   Remove gets rid of that TypeFlex.
--   Rename renames its id.
--   ToDup makes it a TypeDup.
data ResolveAction = Remove | Rename Int | ToDup deriving Show

-- {0/ToDup} ?{0}T => !T

-- | A Resolver is a map from ids referencing TypeFlexes to some action.
type Resolver = Map.Map Int ResolveAction

instance {-# OVERLAPPING #-} Show Resolver where
    show r = "{" ++ showMap r ++ "}"

-- | The substitution where nothing are substituted.
nullSubst :: Subst
nullSubst = Map.empty

-- | The resolver where no TypeFlex actions are resolved.
nullResolver :: Resolver
nullResolver = Map.empty

-- | The type environment where no variables or named functions exist.
emptyEnv :: TypeEnv
emptyEnv = Map.empty

-- | Type errors that can occur during type check.
data TypeError
    = NotInScopeError Named
    | InfiniteTypeError String Type
    | UnificationFailError Type Type
    | SubtypeFailError Type Type
    | ProductDuplicityError Type Type
    | LinearityError Term
    | TopLevelLinearFail String
    deriving Eq

instance Show TypeError where
    show (UnificationFailError (TypeDup (TypeVar _)) a) =
        "Expected duplicable type, got type '" ++ show a ++ "'"

    show (UnificationFailError e a) =
        "Couldn't match expected type '" ++ show e ++
        "' with actual type '" ++ show a ++ "'"

    show (SubtypeFailError l r) =
        "Type '" ++ show l ++ "' is not a subtype of type '" ++ show r ++ "'"

    show (NotInScopeError (Bound j)) =
        "The impossible happened, free deBruijn index"

    show (NotInScopeError (NFun fun)) =
        "Function not in scope: " ++ fun

    show (InfiniteTypeError v t) =
        "Occurs check: cannot construct the infinite type: " ++
        show v ++ " ~ " ++ show t

    show (ProductDuplicityError l r) =
        "Linear product operands must have equal duplicity: " ++ show l ++ " ⊗  " ++ show r

    show (TopLevelLinearFail f) = "Linear function " ++ show f ++ " was used multiple times."

-- | The monad type inferrement occurs in.
--   It could have an exception if there is a type error.
--   It contains some state needed for the infer algorithm.

--   It keeps track of counter for fresh variables.
--   It keeps track of 
--    It keeps track of a counter for what
--   the next fresh type variable should be.
type Infer = ExceptT TypeError (State InferState)

-- | The state kept in the Infer monad.
--   count keeps track of what the next fresh type variable is.
--   idCount keeps track of the next id
--   linenv keeps track of functions used, so linear functios are used only once.
--   env keeps track of bound variables and named function types.
data InferState
    = St { count   :: Int
         , idCount :: Int
         , linenv  :: Set.Set String
         , env     :: TypeEnv
         }

-- | The empty state is where type inferment starts in.
emptyState :: InferState
emptyState = St 0 0 Set.empty Map.empty

-- | Extend the type environment with a new name and its type scheme.
extend :: (Named, Scheme) -> Infer ()
extend (name, scheme) = do
    env <- gets env
    modify $ \st -> st{env = Map.insert name scheme env}

-- | Add a function name to used functions.
addLin :: String -> Infer ()
addLin name = do
    linenv <- gets linenv
    modify $ \st -> st{linenv = Set.insert name linenv}

-- | Apply final substitution, generalize the resulting type, and normalize the type variables.
closeOver :: (Subst, Resolver, Type) -> Scheme
closeOver (sub, res, ty) = normalize scheme
  where scheme = generalize emptyEnv (resply res sub ty)

-- | Normalizes a polymorphic type to simple type variable names.
normalize :: Scheme -> Scheme
normalize (Forall ts body) = Forall (fmap snd ord) (normtype body)
  where
    -- | Maps each free variable in the body to 
    ord = zip (nub $ fv body) letters

    -- fv a = Set.toList $ ftv a
    fv (TypeVar  a)   = [a]
    fv (TypeFlex _ a) = fv a
    fv (TypeDup  a)   = fv a
    fv (a :=> b)      = fv a ++ fv b
    fv (a :>< b)      = fv a ++ fv b
    fv _              = []

    -- | Finds free flex variables in a type.
    ffv :: Type -> [Int]
    ffv (TypeFlex id type') = id : ffv type'
    ffv (TypeDup  a)   = ffv a
    ffv (a :=> b)      = ffv a ++ ffv b
    ffv (a :>< b)      = ffv a ++ ffv b
    ffv _              = []

    -- normflex = resolve . Map.fromList $ zip ((nub . ffv) body) $ map Rename [0..]

    normtype (a :=> b) = normtype a :=> normtype b
    normtype (a :>< b) = normtype a :>< normtype b
    normtype (TypeFlex var a)  = TypeFlex var (normtype a)
    normtype (TypeDup a)   = TypeDup $ normtype a
    normtype (TypeVar a)   =
      case lookup a ord of
        Just x -> TypeVar x
        Nothing -> error "type variable not in signature"
    normtype a = a

-- | 
compose :: Subst -> Map.Map TVar Type -> Map.Map TVar Type
s2 `compose` s1 = Map.map (apply s2) s1 `Map.union` s2

-- | Resolver = Map String ResolveAction
-- | resolve (r2 `rcompose` r1) typ = resolve r2 (resolve r1 typ)
rcompose :: Resolver -> Resolver -> Resolver
r2 `rcompose` r1 = Map.map (f r2) r1 `Map.union` r2
    where
        f :: Resolver -> ResolveAction -> ResolveAction
        f r (Rename id) = case Map.lookup id r of
            Just act -> act
            Nothing  -> Rename id
        f r action = action

-- SResolver :: Type -> Type
-- r2 `srcompose` r1 = \type' -> r2 (r1 type')
-- sResolve :: Resolver -> SResolver
-- sResolve res = \type -> resolve res type

-- {b/Delete}   `rcompose` {a/Rename b} = {a/Delete, b/Delete} --?? ?{b}T -o ?{a}T
-- [b/Delete]   `rcompose` [a/Delete] = {a/Delete, b/Delete}
-- [a/Rename b] `rcompose` [a/ToDup] = fel, omöjlig, satan själv

-- [a/Action]   `rcompose` [b/Rename a] = [b/Action, a/Action]
-- [a/Action]   `rcompose` [b/ToDup]    = [b/ToDup,  a/Action]
-- [a/Action]   `rcompose` [b/Delete]   = [b/Delete, a/Action]

--rmap :: TVar -> ResolveAction -> ResolveAction -> ResolveAction
--rmap id1 Delete (Rename id2) = undefined


-- titta på alla variabler i t1
-- kolla om dom också finns i t2
-- 

-- r2 `rcompose` r1
-- {a/Delete} {a/Rename b} 
-- {a/Delete}


-- {a/Rename b} {a/Delete} = [a/Rename b, a/Delete]
-- {a/Rename b, a/Delete} = a/Delete





(∘) :: Subst -> Map.Map TVar Type -> Map.Map TVar Type
(∘) = compose

-- | Make a type duplicable
bang :: Type -> Type
bang = TypeDup

-- | Make type non-duplicable
debang :: Type -> Type
debang (TypeDup a) = a
debang          a  = a

-- | Converts a type variable to a type flex 
flex :: Type -> Infer Type
flex t = freshId >>= \v -> return $ TypeFlex v t

-- | Converts a type flex to a type variable
deflex :: Type -> Type
deflex (TypeFlex _ a) = deflex a
deflex             t  = t

-- | Transforms all flex variables to normal linear type variables.
deflexType :: Type -> Type
deflexType (a :=> b) = deflexType a :=> deflexType b
deflexType (a :>< b) = deflexType a :>< deflexType b
deflexType (TypeDup a) = TypeDup $ deflexType a
deflexType (TypeFlex _ a) = deflexType a
deflexType a = a

class Substitutable a where
    -- | Given a substitution and a type, if the type contains instances of type variables
    --   that also exist in the substitution map, it replaces those instances with the new
    --   ones in the map.
    apply :: Subst -> a -> a

    -- | Resolves some FlexType constraint.
    resolve :: Resolver -> a -> a

    -- | Find all free type variables.
    ftv   :: a -> Set.Set TVar

    {-# MINIMAL apply, resolve, ftv #-}

    resply :: Resolver -> Subst -> a -> a
    resply r s = resolve r . apply s

instance Substitutable Type where
    -- | TypeVariables in the type are substituted if they exist in the substitution.
    apply _ TypeBit = TypeBit
    apply _ TypeQBit = TypeQBit
    apply _ TypeUnit = TypeUnit
    apply s (TypeDup d) = TypeDup $ apply s d
    apply s (TypeFlex id t) = TypeFlex id (apply s t)
    apply s t@(TypeVar v) = Map.findWithDefault t v s
    apply s (t1 :=> t2) = apply s t1 :=> apply s t2
    apply s (t1 :>< t2) = apply s t1 :>< apply s t2

    resolve r (TypeFlex id t) = case Map.lookup id r of
        Nothing           -> TypeFlex id $ resolve r t
        Just Remove       -> resolve r t
        Just (Rename new) -> TypeFlex new $ resolve r t
        Just ToDup        -> TypeDup (resolve r t)
    resolve r (TypeDup t) = TypeDup (resolve r t)
    resolve r (a :=> b) = resolve r a :=> resolve r b
    resolve r (a :>< b) = resolve r a :>< resolve r b
    resolve _      rest = rest

    -- | The free type variables for a type are all type variables in the type,
    --   since no type variables are bound.
    ftv (TypeVar v)  = Set.singleton v
    ftv (TypeFlex _ v) = ftv v
    ftv (t1 :=> t2)  = Set.union (ftv t1) (ftv t2)
    ftv (t1 :>< t2)  = Set.union (ftv t1) (ftv t2)
    ftv _constant    = Set.empty

instance Substitutable Scheme where
    -- | The type inside the scheme are applied to the substitution,
    --   with the bound type variables removed from the substitution.
    apply s (Forall as t) = Forall as $ apply s' t
        where s' = foldr Map.delete s as

    resolve r (Forall as t) = Forall as $ resolve r t

    -- | All free type variables in a scheme is the type variables
    --   of the type, except for the ones that are bound by the Forall.
    ftv (Forall as t) = Set.difference (ftv t) (Set.fromList as)

instance Substitutable a => Substitutable [a] where
    apply = fmap . apply
    resolve = fmap . resolve
    ftv = foldr (Set.union . ftv) Set.empty

instance Substitutable TypeEnv where
    apply s env =  Map.map (apply s) env
    resolve r env = Map.map (resolve r) env
    ftv env = ftv $ Map.elems env

occursCheck ::  Substitutable a => TVar -> a -> Bool
occursCheck a t = a `Set.member` ftv t

-- | Given two types, creates a substitution that when applied to the types
--   would make them 
unify :: Type -> Type -> Infer (Subst, Resolver)
unify (TypeDup (l :=> r)) (l' :=> r') = do
    (s1, r1) <- unify l l'
    (s2, r2) <- unify (resply r1 s1 r) (resply r1 s1 r')
    return (s2 ∘ s1, r2 `rcompose` r1)
unify (l :=> r) (TypeDup (l' :=> r')) = do
    (s1, r1) <- unify l l'
    (s2, r2) <- unify (resply r1 s1 r) (resply r1 s1 r')
    return (s2 ∘ s1, r2 `rcompose` r1)
unify (l :=> r) (l' :=> r') = do 
    (s1, r1) <- unify l l' 
    (s2, r2) <- unify (resply r1 s1 r) (resply r1 s1 r')
    return (s2 ∘ s1, r2 `rcompose` r1)
unify (TypeDup (l :>< r)) (l' :>< r') = do
    (s1, r1) <- unify (TypeDup l) l'
    (s2, r2) <- unify (TypeDup r) r'
    return (compose s2 s1, rcompose r2 r1)
unify (l :>< r) (TypeDup (l' :>< r')) = do
    (s1, r1) <- unify l (TypeDup l')
    (s2, r2) <- unify r (TypeDup r')
    return (compose s2 s1, rcompose r2 r1)
-- unify t1@(l :>< r) t2@(TypeDup (l' :>< r')) = unify t2 t1
unify (TypeFlex id (l :>< r)) (l' :>< r') = do
    (s1, r1) <- unify (TypeFlex id l') l
    (s2, r2) <- unify (TypeFlex id r') r
    return (compose s2 s1, rcompose r2 r1)
unify l@(a' :>< b') r@(TypeFlex id (a :>< b)) = unify r l
unify (l :>< r) (l' :>< r') = do
    (s1, r1) <- unify l l'
    (s2, r2) <- unify (resply r1 s1 r) (resply r1 s1 r') -- note: why not apply.
    return (s2 ∘ s1, r2 `rcompose` r1)
unify (TypeFlex id1 t1) (TypeFlex id2 t2) = unifyWithAction t1 t2 (Rename id2) id1
unify (TypeFlex id t1) (TypeDup t2) = unifyWithAction t1 t2 ToDup id
unify (TypeDup t1) (TypeFlex id t2) = unifyWithAction t1 t2 ToDup id
unify t1 (TypeFlex id t2) = unifyWithAction t1 t2 Remove id
unify a@(TypeFlex id t1) t2 = unify t2 a -- reverse this case
unify (TypeVar a) t = bind a t   -- TypeDup a ~ ?(TypeFlex b -o TypeFlex c), unify a (b -o c)
unify t (TypeVar a) = bind a t
unify (TypeDup t1) (TypeDup t2) = unify t1 t2
unify t1 t2 | (t1 <: t2 || t2 <: t1) && isConstType t1 && isConstType t2 = return (nullSubst, nullResolver)
            | otherwise = throwError $ UnificationFailError t1 t2 

unifyWithAction :: Type -> Type -> ResolveAction -> Int -> Infer (Subst, Resolver)
unifyWithAction t1 t2 action id = do
    (s,r1) <- unify t1 t2
    let r2 = createAction id action
    return (s, r2 `rcompose` r1)

-- | Binds a type variable with another type and returns a substitution.
bind :: TVar -> Type -> Infer (Subst, Resolver)
bind a (TypeDup t) = return (Map.singleton a t, nullResolver)
bind a t | t == TypeVar a = return (nullSubst, nullResolver)
         | occursCheck a t = throwError $ InfiniteTypeError a t
         | otherwise       = return (Map.singleton a t, nullResolver)

createActionM :: Int -> ResolveAction -> Infer (Subst, Resolver)
createActionM var action = return (nullSubst, Map.singleton var action)

createAction :: Int -> ResolveAction -> Resolver
createAction = Map.singleton

-- | Checks if a type is a constant type
isConstType :: Type -> Bool
isConstType TypeBit = True
isConstType TypeQBit = True
isConstType TypeUnit = True
isConstType (TypeDup t) = isConstType t
isConstType _type = False

-- | Introduce a new type variable.
--   Also updates the internal type variable counter to avoid collisions.
fresh :: Infer Type
fresh = do
  s <- gets count
  modify $ \st ->  st{count = s+1}
  return $ TypeVar $ letters !! s

freshId :: Infer Int
freshId = do
  i <- gets idCount
  modify $ \st ->  st{idCount = i+1}
  return i

-- | Introduce a new flexible type variable.
freshFlex :: Infer Type
freshFlex = TypeFlex <$> freshId <*> fresh

-- | Returns a list of strings used for fresh type variables.
letters :: [String]
letters = [1..] >>= flip replicateM ['a'..'z']

-- | Renames all TVars in the scheme and applies it
--   to get a type with unique (and free) type variables.
instantiate :: Scheme -> Infer Type
instantiate (Forall as t) = do
    as' <- mapM (const fresh) as -- Each bound variable gets a new name.
    let s = Map.fromList $ zip as as' -- Map from old type variables to new names.
    return $ apply s t

-- | 
generalize :: TypeEnv -> Type -> Scheme
generalize env t  = Forall as t
    where as = Set.toList $ ftv t `Set.difference` ftv env


-- epr : !(T -o (QBit >< QBit))
-- epr x = ...

-- q : QBit
-- q = H (new 0)

-- f : Bit
-- f = measure q

-- g : Bit
-- g = measure q

-- main : (Bit >< Bit)
-- main = (f,g)

-- (0,1) -- this becomes possible

-- f : Bit, g : Bit |- (f,g) : Bit >< Bit

-- q : QBit, f : Bit |- (f, measure q) : Bit >< Bit

-- If G1, !D, x:A |- M : B
-- and G2, !D |- V : A
-- then G1, G2, !D |- M[x:=V] : B



-- q = new 0 
-- f = (q,q)

-- environment instead of int? 
-- | Infers a substitution and a type from a Term
infer :: Integer -> Term -> Infer (Subst, Resolver, Type)
infer i (Idx j)      = (nullSubst, nullResolver,) <$> lookupEnv (Bound (i-j-1))
infer _ (Fun var)    = do
    linEnv <- gets linenv
    typ <- lookupEnv (NFun var)
    case typ of
        TypeDup _ -> return (nullSubst, nullResolver, typ)
        _notdup   -> if Set.member var linEnv
                        then throwError $ TopLevelLinearFail var
                        else addLin var >> return (nullSubst, nullResolver, typ)
infer i (Bit _)      = (nullSubst, nullResolver,) <$> flex TypeBit
infer _ (Gate gate)  = return (nullSubst, nullResolver, inferGate gate)
infer i (Tup l r)  = do
    tr $ "Infer tup l = " ++ show l ++ ", r = " ++ show r
    (ls, lr, lt) <- infer i l
    tr $ "l has type = " ++ show lt ++ ", with subst = " ++ show ls ++ " & resolver = " ++ show lr
    -- resply environment
    env <- gets env 
    modify (\st -> st{env=resply lr ls env})
    (rs, rr, rt) <- infer i r
    tr $ "r has type = " ++ show rt ++ ", with subst = " ++ show rs ++ " & resolver = " ++ show rr
    let rt' = resply lr ls rt 
    let lt' = resply rr rs lt
    tr $ "ls: " ++ show ls
    tr $ "rt: " ++ show rt
    tr $ "rs: " ++ show rs
    tr $ "rr: " ++ show rr 
    tr $ "rt': " ++ show rt'
    tr $ "lt': " ++ show lt'
    modify (\st -> st{env=resply rr rs env})
    (pr, pt) <- productExponential lt' rt'
    tr $ "pt: " ++ show pt
    return (ls ∘ rs, pr `rcompose` rr `rcompose` lr, pt)
infer i (App l r) = do
    tr $ "Infer appl l = " ++ show l ++ ", r = " ++ show r
    env <- gets env
    (s1,r1,t1) <- infer i l -- t1 = !(?{3}(Bit) ⊸ ?{4}(c)) ⊸ ?{1}(b) ⊗  ?{4}(c))
    modify (\st -> st{env=resply r1 s1 env})
    (s2,r2,t2) <- infer i r -- QBit 
    modify (\st -> st{env=resply r2 s2 env})
    tr $ "l: " ++ show t1 ++ ", r: " ++ show t2
    tv <- freshFlex
    id <- freshId 
    tr $ "tv: " ++ show tv
    tr $ "s1: " ++ show s1 ++ "r1: " ++ show r1
    tr $ "s2: " ++ show s2 ++ "r2: " ++ show r2
    tr $ "resply s1 r1" ++ show (resply r1 s1 t2)
    tr $ "unif left: " ++ show (resply r2 s2 t1)
    tr $ "unif right: " ++ show (t2 :=> tv)
    (s3,r3)    <- unify (resply r2 s2 t1) (TypeFlex id (t2 :=> tv))
    tr $ "s3: " ++ show s3 ++ "r3: " ++ show r3
   --- f: Bit -o QBit -o c >< c  b: Bit -o QBit -o c x c 
   -- t2 <: 
    tr $ "subtypecheck f: " ++ show (resply (r3 `rcompose` r2) (s3 `compose` s2) t1) ++ "b: " ++ show (resply r3 s3 t2)
    (s4,r4) <- subtypeCheck (resply (r3 `rcompose` r2) (s3 `compose` s2) t1) (resply r3 s3 t2) -- t2 <:  
    tr $ "s4: " ++ show s4 ++ "r4: " ++ show r4
    return (s4 ∘ s3 ∘ s2 ∘ s1, r4 `rcompose` r3 `rcompose` r2 `rcompose` r1, resply r3 s3 tv)
infer i (IfEl b l r) = do
    (s1,r1,t1) <- infer i b
    (s2,r2,t2) <- infer i l
    (s3,r3,t3) <- infer i r
    name <- freshId
    (s4,r4) <- unify (TypeFlex name TypeBit) t1
    (s5,r5) <- unify t2 t3
    return (s5 ∘ s4 ∘ s3 ∘ s2 ∘ s1, foldr1 rcompose [r1,r2,r3,r4,r5], resply (r5 `rcompose` r4) s5 t2)
infer i (Let eq inn) = do -- let (a, b) = eq in inn
    tv1 <- inferDuplicity (Abs inn) -- Create typevar for a
    tv2 <- inferDuplicity inn -- Create typevar for b
    (rtv, tv) <- productExponential tv1 tv2 -- Create productType tv with exponentials moved out
    (seq,req,teq) <- infer i eq -- Infer the type of eq
    --tr $ "teq is: " ++ show teq ++ ", tv is: " ++ show tv
    (sprod,rprod) <- unify teq tv -- teq and tv should be same
    --tr $ "sprod: " ++ show sprod ++ ", rprod" ++ show rprod
    -- Add the typevariables tv1 and tv2, with first rtv resolved, then rprod and sprod
    let tv1' = resply (rprod `rcompose` rtv) sprod tv1
    let tv2' = resply (rprod `rcompose` rtv) sprod tv2
    extend (Bound (i+1), Forall [] tv1')
    extend (Bound  i   , Forall [] tv2')
    (sinn,rinn,tinn) <- infer (i+2) inn -- Infer the type of inn
    let subst = sinn ∘ sprod ∘ seq -- all subsitutions
    let resolver = rinn `rcompose` rprod `rcompose` req -- rtv not needed, already used
    return (subst, resolver, tinn)
infer i (Abs body) = do
    tv <- inferDuplicity body
    tr $ show tv
    extend (Bound i, Forall [] tv)
    (s1,r1,t1) <- infer (i+1) body
    tr $ "t1 :" ++ show t1
    tr $ "tv new: " ++ show (resply r1 s1 tv)
    id <- freshId
    returnLinear <- not . all dupable <$> mapM (lookupEnv . NFun) (fv body)
    tr $ "resply tv" ++ show (resply r1 s1 tv)
    tr $ "resply tv" ++ show (resply r1 s1 tv :=> t1)
    if returnLinear 
        then return (s1, r1, resply r1 s1 tv :=> t1)
        else return (s1, r1, TypeFlex id (resply r1 s1 tv :=> t1))
infer _ New  = do
    flexBit <- flex TypeBit 
    return (nullSubst, nullResolver, TypeDup (flexBit  :=> TypeQBit)) -- !(Bit -o QBit) !(?Bit -o QBit)
infer _ Meas = return (nullSubst, nullResolver, TypeDup (TypeQBit :=> TypeDup TypeBit))
infer _ Unit = (nullSubst, nullResolver,) <$> flex TypeUnit

--                    !(Bit -o QBit)_new <: Bit -o QBit           !Bit_0 <: Bit
--                   -----------------------------------         ----------------
--   !Bit_0 <: Bit           D |- new : Bit -o QBit                D |- 0 : Bit
--  --------------          -----------------------------------------------------
--    G |- 0 : Bit                          D |- (new 0) : QBit        
-- ---------------------------------------------------------------------------
--                 G, D |- (0, new 0) : Bit >< QBit


fv :: Term -> [String]
fv (Fun v) = [v]
fv (Abs e) = fv e
fv (App l r) = fv l ++ fv r
fv (Tup t1 t2) = fv t1 ++ fv t2
fv (IfEl b t f) = fv b ++ fv t ++ fv f
fv (Let eq inn) = fv eq ++ fv inn 
fv _ = []


dupable :: Type -> Bool
dupable (TypeDup _) = True
dupable _ = False

-- (\\x.\\y.let (a,b) = (x,y) in a) (new 0) 0 -- QBit 
-- ? (?a -o ?b) -o   
-- \\x.(x,0) 
-- new : !(Bit -o QBit)

-- q : QBit
-- q = new 0

-- f : !(Bit -o (QBit >< Bit))
-- f = \\x . (q,x)


-- | Should move exponentials outside.
productExponential :: Type -> Type -> Infer (Resolver, Type)
productExponential (TypeDup       a) (TypeDup       b) = return (nullResolver, TypeDup (a :>< b))
productExponential (TypeFlex id1  a) (TypeFlex id2  b) | id1 == id2 = return (nullResolver, TypeFlex id1 (a :>< b))

-- productExponential (TypeFlex id   t) (TypeDup       b) = return (createAction id ToDup, TypeDup (t :>< b))
-- productExponential (TypeDup      t1) (TypeFlex id  t2) = return (createAction id ToDup, TypeDup (t1 :>< t2))
--productExponential (TypeFlex id1 t1) (TypeFlex id2 t2) = return (createAction id1 (Rename id2), TypeFlex id2 (t1 :>< t2))
-- productExponential (TypeFlex id  t1)               t2  = return (createAction id Remove, t1 :>< t2)
-- productExponential               t1  (TypeFlex id  t2) = return (createAction id Remove, t1 :>< t2)
-- productExponential               t1  (TypeDup      t2) = throwError $ ProductDuplicityError t1 (TypeDup t2)
-- productExponential (TypeDup      t1)               t2  = throwError $ ProductDuplicityError (TypeDup t1) t2
productExponential               t1                t2  = return (nullResolver, t1 :>< t2)

tr :: Monad m => String -> m ()
tr x = trace x (return ())

-- | Infers type of a Gate
inferGate :: Gate -> Type
inferGate g = TypeDup $ gateType $ case g of
    GFRDK -> 3
    GTOF  -> 3
    GSWP  -> 2
    GCNOT -> 2
    _     -> 1
    where
        gateType' n = foldr (:><) TypeQBit (replicate (n-1) TypeQBit)
        gateType  n = gateType' n :=> gateType' n

subtypeCheck :: Type -> Type -> Infer (Subst, Resolver)
subtypeCheck f b = case debang $ deflex f of
    (a :=> _) | b <: a    -> unify b a --- f: Bit -o b -o c >< c  b: Bit -o QBit -o d 
              | otherwise -> throwError $ SubtypeFailError b a
    t -> error $ "urk: subtype check " ++ show t

-- | Return whether a type is a subtype of another type.
(<:) :: Type -> Type -> Bool
TypeDup  a     <: TypeDup  b      = TypeDup a <: b
TypeDup  a     <: TypeFlex id b   = a <: b
(TypeDup (a :>< b)) <: (TypeDup a' :>< (TypeDup b')) = a  <: a' && b  <: b
TypeDup  a     <:          b      = a <: b
(TypeDup a :>< TypeDup b) <: (TypeDup (a' :>< b')) = a  <: a' && b  <: b'
(a1 :>< a2)    <: (TypeDup (b1 :><  b2)) = a1 <: TypeDup b1 && a2 <: TypeDup b2
(a1 :>< a2)    <: (b1 :><  b2)    = a1 <: b1 && a2 <: b2
(a' :=>  b)    <: (a :=>   b')    = a  <: a' && b  <: b'
TypeFlex id a  <: TypeDup  b      = a <: b 
TypeFlex id a  <:          b      = a <: b
a              <: TypeFlex id b   = a <: b
a              <: TypeVar  b      = True
a              <:          b      = a == b

-- ?a  !Bit -> a <: Bit 

-- | Given a term, 
--    if the head variable is used once or zero times returns TypeFlex id (TypeVar var).
--    if the head variable is used twice or more, returns TypeDup (TypeVar var).
inferDuplicity :: Term -> Infer Type
inferDuplicity e
    | headCount e <= 1 = freshFlex
    | otherwise        = bang <$> fresh

(!?) :: Term -> Infer Type
(!?) = inferDuplicity

-- | Looks up a free or bound variable from the environment
lookupEnv :: Named -> Infer Type
lookupEnv x = do
    env' <- gets env
    case Map.lookup x env' of
        Nothing -> throwError $ NotInScopeError x
        Just  s -> instantiate s

linearcheck :: Term -> Type -> Infer ()
linearcheck e = \case
    TypeDup t -> return ()
    _notdup   -> if headCount e <= 1
                    then return ()
                    else throwError $ LinearityError e

-- | The number of variables bound to the head
headCount :: Term -> Integer
headCount = cO 0
    where cO i = \case
            Idx   j    -> if i - j == 0 then 1 else 0
            Abs e      -> cO (i+1) e
            App l r    -> cO i l + cO i r
            IfEl b t f -> cO i b + max (cO i t) (cO i f)
            Tup l r    -> cO i l + cO i r
            Let _ e    -> cO (i+2) e
            -- \x . let (a,b) = x in M
            e -> 0


typecheckTerm :: Term -> Either TypeError Scheme
typecheckTerm e = runInfer (infer 0 e)

typecheckProgram :: [Function] -> [Either TypeError (String, Type)]
typecheckProgram fs = map (checkFunc state) fs
    where state = St 0 0 Set.empty (genEnv fs)

showTypes :: [Either TypeError (String, Type)] -> IO ()
showTypes = putStrLn . intercalate "\n\n" . map st
    where st (Left  err) = "*** Exception:\n" ++ show err
          st (Right (name, typ)) = name ++ " : " ++ show typ

runtc :: String -> [Either TypeError (String, Type)]
runtc prog = typecheckProgram (run prog)

runtcFile :: FilePath -> IO [Either TypeError (String, Type)]
runtcFile path = runtc <$> readFile path

-- | Run the inferement code. The result is a type scheme or an exception.
runInfer :: Infer (Subst, Resolver, Type) -> Either TypeError Scheme
runInfer = runInferWith emptyState

runUnify :: Type -> Type -> Either TypeError (Subst, Resolver)
runUnify a b = evalState (runExceptT (unify a b)) emptyState

runInferWith :: InferState -> Infer (Subst, Resolver, Type) -> Either TypeError Scheme
runInferWith inferState m = case evalState (runExceptT m) inferState of
        Left  err -> Left err
        Right res -> Right $ closeOver res

checkFunc :: InferState -> Function -> Either TypeError (String, Type)
checkFunc state (Func name qtype term) = do
    -- let is = St 0 Set.empty env 0
    Forall _ itype <- runInferWith state (infer 0 term)
    s <- evalState (runExceptT (subsumeSignature qtype itype)) emptyState
    return (name, s)

debangFunc :: Type -> Type
debangFunc (TypeDup (a :=> b)) = a :=> b
debangFunc a = a

-- | Gives the unified type of the type from the type signature
--   and the inferred type.  
subsumeSignature :: Type -> Type -> Infer Type
subsumeSignature typ inf = do
    (sub,res) <- unify typ inf -- Try to unify signature with inferred type
    tr $ "typ: " ++ show typ
    tr $ "inf: " ++ show inf
    tr $ "s: " ++ show sub
    tr $ "r: " ++ show res
    let t = resply res sub inf -- 
    tr $ "t: " ++ show t
    if t <: typ -- kolla att alla med samma flex id får samma typ 
        then return typ
        else throwError $ SubtypeFailError t typ

-- | Generate environment with function signatures 
genEnv :: [Function] -> TypeEnv
genEnv = Map.fromList . map f
    where f (Func n t _) = (NFun n, generalize emptyEnv t)

inferExp :: String -> Either TypeError Type
inferExp prog = do
    let [Func _ _ term] = run ("f : a f = " ++ prog)
    Forall _ type' <- runInfer (infer 0 term)
    return type'


instance IsString Type where
    fromString t = typ
        where [Func _ typ _] = run $ "m : " ++ t ++ " m = *"

-- tc fs = foldM (\(func, state) -> checkFunc state func) (St 0 Set.empty env 0) fs
--     where env = genEnv fs

tc :: [Function] -> Infer ()
tc = mapM_ f
    where f (Func _ qtype term) =
            do (s,r, itype) <- infer 0 term
               env <- gets env
               linenv <- gets linenv
               subsumeSignature qtype itype
               put $ St 0 0 linenv env

-- | Run typechecker on program
typecheck :: [Function] -> Either TypeError ()
typecheck funcs = void $ evalState (runExceptT (tc funcs)) state
    where
        state = St 0 0 Set.empty (genEnv funcs)

testTc :: String -> Either TypeError ()
testTc = typecheck . run