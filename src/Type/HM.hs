{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

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


-- | Representations of free and bound variables in lambda abstractions
data Named
    = NFun String -- Named function
    | Bound Integer
    deriving (Eq, Ord, Show)

-- | A scheme has a type, together with a list of (Forall) bound type variables.
data Scheme = Forall [TVar] Type

-- | A type variable (TVar) is either a normal linear type variable (LVar) "a", 
--   or the more general flexible type variable (FVar) "?a".
type TVar = String

instance Show Scheme where
    show (Forall [] t) = printTree (reverseType t)
    show (Forall vs t) = "∀ " ++ unwords (map show vs) ++ " . "
                      ++ printTree (reverseType t)

-- | Each variable in the TypeEnv has an associated type Scheme.
type TypeEnv = Map.Map Named Scheme

-- | A substitution is a map from a type variable to the type it 
--  should be substituted with. It could be another type variable.
type Subst = Map.Map TVar Type

showMap :: Show a => Map.Map String a -> String
showMap = intercalate "," . map elem . Map.toList
    where elem (from, to) = from ++ "/" ++ show to 

instance {-# OVERLAPPING #-} Show Subst where
    show s = "[" ++ showMap s ++ "]"

-- | A resolveaction contains three actions that can be made with a FlexType.
data ResolveAction = Remove | Rename String | ToDup deriving Show

type Resolver = Map.Map String ResolveAction

instance {-# OVERLAPPING #-} Show Resolver where
    show r = "{" ++ showMap r ++ "}"

-- | The null substitution are the substitution where nothing are substituted.
nullSubst :: Subst
nullSubst = Map.empty

-- | The resolver that does nothing
nullResolver :: Resolver
nullResolver = Map.empty

-- | The empty environment are the type environment where no variables exist.
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

    show (NotInScopeError (NFun v)) =
        "Function not in scope: " ++ v

    show (InfiniteTypeError v t) =
        "Occurs check: cannot construct the infinite type: " ++
        show v ++ " ~ " ++ show t

    show (ProductDuplicityError l r) =
        "Linear product operands must have equal duplicity: " ++ show l ++ " ⊗  " ++ show r

    show (TopLevelLinearFail f) = "Linear function " ++ show f ++ " was used multiple times."

-- | Counter for creating fresh type variables
type Counter = Int

-- | The monad type inferrement occurs, it could have an exception
--   if there is a type error. It keeps track of a counter for what
--   the next fresh type variable should be.
type Infer = ExceptT TypeError (State InferState)

data InferState 
    = St { count  :: Counter
         , linenv :: Set.Set String
         , env    :: TypeEnv
         }

emptyState :: InferState
emptyState = St 0 Set.empty Map.empty

-- | Extend the type environment with a new bound variable together with its type scheme.
extend :: (Named, Scheme) -> Infer ()
extend (n,s) = do
    env <- gets env
    modify $ \st -> st{env = Map.insert n s env}

extend' env (n,s) = Map.insert n s env

addLin :: String -> Infer ()
addLin name = do
    linenv <- gets linenv
    modify $ \st -> st{linenv = Set.insert name linenv}

-- | Apply final substitution and normalize the type variables 
closeOver :: (Subst, Resolver, Type) -> Scheme
closeOver (sub, res, ty) = normalize sc
  where sc = generalize emptyEnv (resply res sub ty)

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
r2 `rcompose` r1 =  r1 `Map.union` r2

rmap :: (TVar, ResolveAction) -> (TVar, ResolveAction) -> (TVar, ResolveAction)
rmap (t1, r1) (t2,r2) = undefined
-- titta på alla variabler i t1
-- kolla om dom också finns i t2

-- {a/Delete} {a/Rename}
-- {a/Delete} U (rmap {a/Delete} {a/Rename}) = {a/Delete} U {}

-- {a/Rename b} {a/Delete}
-- resolve {a/Rename b} U {b/Delete}

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
flex t = fresh' >>= \v -> return $ TypeFlex v t

-- | Converts a type flex to a type variable
deflex :: Type -> Type
deflex (TypeFlex _ a) = a
deflex           t  = t

-- myIf : Bit -> a -> b
-- condition:Bit, !Bit 

-- f : !Bit -o a
-- f = \x . if x then a else b : !Bit -o a

-- \x:!Bit. new x
-- HM{\x . new x} : ?Bit -o QBit
-- x till ?a
-- ?a -> Bit

-- / !(TypeVar a)
-- !Term Term ?Term
-- Sub ?Sub
-- [?a -> xxx]
-- [a/Bit] ?a --> ?Bit
-- [?a/Bit] ?a --> Bit 
-- [a/Bit] !a --> !Bit
-- [?a/Bit] !a --> !Bit -- ERR : ?a och a matchar inte
-- [a/!Bit] ?a --> 
--   [TVar a / !Bit] TypeFlex (TypeVar a) -> TypeFlex (!Bit)  -- !Bit     
--                                           TypeFlex (TypeDup e) = TypeDup e

--simplifyFlexes :: Type -> Type
--simplifyFlexes 





-- [id0/Bit] TypeFlex id0 Bit -> Bit
-- [id0/TypeFlex id1 Bit] TypeFlex id0 Bit ->TypeFlex id1 Bit
-- [id0/TypeDup Bit] TypeFlex id0 Bit -> TypeDup Bit 
-- [id0/Remove] TypeFlex id0 (TypeVar "a" >< TypeVar "b") -> (TypeVar "a" >< TypeVar "b")

-- TypeFlex id0 t1 -> t1
-- [id0/t1]  
-- TypeFlex id0 t2 -> t1

-- TypeFlex id0 t --> [id0/t]
-- t@()

-- \x.\y.(x,y)
-- (TypeFlex id1 tx, TypeFlex id2 ty)
-- (TypeFlex id1 tx, TypeFlex id1 ty)

-- TypeFlex "id0" t0 ~ TypeFlex "id1" t1
-- {id0/id1} [t0/t1]

-- TypeFlex id1 t1 ~ TypeFlex id1 t1


-- | Transforms all flex variables to normal linear type variables.
deflexType :: Type -> Type
deflexType (a :=> b) = deflexType a :=> deflexType b
deflexType (a :>< b) = deflexType a :>< deflexType b
deflexType (TypeDup a) = TypeDup $ deflexType a
deflexType a = deflex a

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
        Nothing           -> TypeFlex id t
        Just Remove       -> t
        Just (Rename new) -> TypeFlex new t
        Just ToDup        -> TypeDup t
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

-- ?a ~  t : [?a/t]
--  t ~ ?a : [?a/t]
-- ?a ~ !t : [?a/!t]
-- ?a ~ ?b : [a/b]

-- [a/b] TypeFlex (TypeVar a) --> TypeFlex (TypeVar b)

-- ?a ~ ?b : [?a/?b]

-- ?(a >< b) ~ ?(Bit >< QBit) = (a >< b) ~ (Bit >< QBit) : [a/Bit, b/QBit]
-- TypeFlex a ~ TypeFlex b =  unify a b 
-- TypeFlex e1 ~ e2 = ?????
-- TypeFlex e1 ~ (TypeVar "?a") = 
-- 
-- TypeFlex (TypeVar a) ~ TypeBit = TypeBit
-- TypeFlex (TypeVar a) ~ TypeDup TypeBit = TypeDup TypeBit  [?a/TypeDup TypeBit]
-- [FVar a -> TypeBit] TypeFlex (TypeVar a) -> TypeBit
-- TypeFlex (TypeDup TypeBit) = TypeDup TypeBit?
-- ?!Bit = !Bit
-- if ?Bit then a else a
--    FVar 
-- TypeFlex (TypeVar a) ~ TypeFlex (TypeVar b) = TypeFlex (TypeVar a (or b))
-- 


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
unify t1@(l :>< r) t2@(TypeDup (l' :>< r')) = unify t2 t1
unify (l :>< r) (l' :>< r') = do 
    (s1, r1) <- unify l l'
    (s2, r2) <- unify r r' -- note: why not apply.
    return (s2 ∘ s1, r2 `rcompose` r1)
unify (TypeVar a) t = bind a t
unify t (TypeVar a) = bind a t 
unify (TypeFlex id1 t1) (TypeFlex id2 t2) = unifyWithAction t1 t2 (Rename id2) id1
unify (TypeFlex id t1) (TypeDup t2) = unifyWithAction t1 t2 ToDup id 
unify (TypeDup t1) (TypeFlex id t2) = unifyWithAction t1 t2 ToDup id  
unify t1 (TypeFlex id t2) = unifyWithAction t1 t2 Remove id
unify a@(TypeFlex id t1) t2 = unify t2 a -- reverse this case
unify (TypeDup t1) (TypeDup t2) = unify t1 t2
unify t1 t2 | (t1 <: t2 || t2 <: t1) && isConstType t1 && isConstType t2 = return (nullSubst, nullResolver)            
            | otherwise =  throwError $ UnificationFailError t1 t2

unifyWithAction :: Type -> Type -> ResolveAction -> String -> Infer (Subst, Resolver)
unifyWithAction t1 t2 action id = do
    (s,r1) <- unify t1 t2
    let r2 = createAction id action
    return (s, r2 `rcompose` r1)

-- | Binds a type variable with another type and returns a substitution.
bind :: TVar -> Type -> Infer (Subst, Resolver)
bind a t | t == TypeVar a  = return (nullSubst, nullResolver)
         | occursCheck a t = throwError $ InfiniteTypeError a t
         | otherwise       = return (Map.singleton a t, nullResolver)

createActionM :: TVar -> ResolveAction -> Infer (Subst, Resolver)
createActionM var action = return (nullSubst, Map.singleton var action)

createAction :: TVar -> ResolveAction -> Resolver
createAction = Map.singleton

-- TypeFlex id0 t1 ~ TypeFlex id1 t2 -> [LVar id0/FlexAction Rename id1] ++ t1 ~ t2
-- TypeFlex id0 t1 ~ TypeVar "b"
-- id0 bindas till annat id
-- id0 instansiseras till !
-- id0 tas bort

-- \\x.if x then 0 else 1
-- : TypeFlex id0 (TypeVar "a") ~ TypeFlex id1 TypeBit  

-- apply [LVar "a"/TypeAction Bit] (TypeFlex "b" (TypeVar "a")) == (TypeFlex "b" Bit)
-- apply [LVar "b"/FlexAction Remove] (TypeFlex "b" t) == t
-- apply [LVar "a"/FlexAction Remove] (TypeFlex "a" (TypeVar "a")) == TypeVar "a"

-- | Checks if a type is a constant type
isConstType :: Type -> Bool
isConstType TypeBit = True
isConstType TypeQBit = True
isConstType TypeUnit = True
isConstType (TypeDup d) = isConstType d
isConstType _type = False

-- | Introduce a new type variable.
--   Also updates the internal type variable counter to avoid collisions.
fresh :: Infer Type
fresh = do
  s <- gets count
  modify $ \st ->  st{count = s+1}
  return $ TypeVar $ letters !! s

fresh' :: Infer String 
fresh' = do
  s <- gets count
  modify $ \st ->  st{count = s+1}
  return $ letters !! s

-- | Introduce a new flexible type variable.
freshFlex :: Infer Type
freshFlex = do
    s <- gets count 
    modify $ \st ->  st{count = s+2}
    return $ TypeFlex (letters !! s) $ TypeVar $ letters !! (s+1)

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

-- | Infers a substitution and a type from a Term
infer :: Integer -> Term -> Infer (Subst, Resolver, Type)
infer i (Idx j)      = lookupEnv (Bound (i-j-1)) 
infer i (Fun var)    = do
    linEnv <- gets linenv
    (s, res, typ) <- lookupEnv (NFun var)
    case typ of
        TypeDup _ -> return (s, res, typ)
        _notdup   -> if Set.member var linEnv
                        then throwError $ TopLevelLinearFail var
                        else addLin var >> return (s, res, typ)
infer i (Bit _)      = return (nullSubst, nullResolver, bang TypeBit)
infer i (Gate gate)  = return (nullSubst, nullResolver, inferGate gate)
infer i (Tup l r)  = do
    (ls, lr, lt) <- infer i l
    (rs, rr, rt) <- infer i r
    (pr, pt) <- productExponential lt rt
    return (ls ∘ rs, pr `rcompose` rr `rcompose` lr, pt)
infer i (App l r) = do
    tv <- freshFlex 
    env <- gets env
    (s1,r1,t1) <- infer i l 
    modify (\st -> st{env=apply s1 env}) -- todo, fix all apply
    (s2,r2,t2) <- infer i r
    (s3,r3)    <- unify (apply s2 t1) (t2 :=> tv)
    subtypeCheck (apply (s3 `compose` s2) t1) (apply s3 t2) -- t2 <: 
    return (s3 ∘ s2 ∘ s1, r3 `rcompose` r2 `rcompose` r1, resply r3 s3 tv)
infer i (IfEl b l r) = do
    (s1,r1,t1) <- infer i b
    (s2,r2,t2) <- infer i l
    (s3,r3,t3) <- infer i r
    name <- fresh'
    (s4,r4) <- unify (TypeFlex name TypeBit) t1
    (s5,r5) <- unify t2 t3
    return (s5 ∘ s4 ∘ s3 ∘ s2 ∘ s1, foldr1 rcompose [r1,r2,r3,r4,r5] ,apply s5 t2)
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
    extend (Bound i, Forall [] tv)
    (s1,r1,t1) <- infer (i+1) body
    return (s1, r1, resply r1 s1 tv :=> t1)
infer i New  = return (nullSubst, nullResolver, TypeDup (TypeBit  :=> TypeQBit))
infer i Meas = return (nullSubst, nullResolver, TypeDup (TypeQBit :=> TypeDup TypeBit))
infer i Unit = return (nullSubst, nullResolver, TypeDup TypeUnit)

-- | Should move exponentials outside.
productExponential :: Type -> Type -> Infer (Resolver, Type)
-- (!a, !b) -> !(a, b)
productExponential (TypeDup a) (TypeDup b) = return (nullResolver, TypeDup (a :>< b))
-- (?a, !b) -> !(a, b)
productExponential (TypeFlex id t) (TypeDup b) = return (createAction id ToDup, TypeDup (t :>< b))
-- (!a, ?b) -> !(a, b)
productExponential (TypeDup t1) (TypeFlex id t2) = return (createAction id ToDup, TypeDup (t1 :>< t2))
-- (?a, ?b) -> ?(a, b)
productExponential (TypeFlex id1 t1) (TypeFlex id2 t2) = return (createAction id1 (Rename id2), TypeFlex id2 (t1 :>< t2))
-- (?a, b) -> (a, b)
productExponential (TypeFlex id t1) t2 = return (createAction id Remove, t1 :>< t2)
-- (a, ?b) -> (a, b)
productExponential t1 (TypeFlex id t2) = return (createAction id Remove, t1 :>< t2)
-- (a, !b) -> fail
productExponential t1 (TypeDup t2) = throwError $ ProductDuplicityError t1 (TypeDup t2)
-- (!a, b) -> fail
productExponential (TypeDup t1) t2 = throwError $ ProductDuplicityError (TypeDup t1) t2
-- (a, b) -> (a, b)
productExponential t1 t2 = return (nullResolver, t1 :>< t2)



-- productExponential l r
--     | nexps l == nexps r = return $ iterate bang (debang l :>< debang r) !! nexps l
--     | otherwise = throwError $ ProductDuplicityError l r
--     where
--         nexps :: Type -> Int
--         nexps (TypeDup a) = 1 + nexps a
--         nexps a = 0

tr :: (Show a, Monad m) => a -> m ()
tr x = trace (show x) (return ())

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

subtypeCheck :: Type -> Type -> Infer ()
subtypeCheck f b = case debang $ deflex f of
    (a :=> _) | b <: a    -> return ()
              | otherwise -> throwError $ SubtypeFailError b a
    t -> error $ "urk: subtype check " ++ show t

-- | Return whether a type is a subtype of another type.
(<:) :: Type -> Type -> Bool
TypeDup  a     <: TypeDup  b      = TypeDup a <: b
TypeDup  a     <: TypeFlex id b   = a <: b 
TypeDup  a     <:          b      = a <: b
(a1 :>< a2)    <: (TypeDup (b1 :><  b2)) = a1 <: TypeDup b1 && a2 <: TypeDup b2
(a1 :>< a2)    <: (b1 :><  b2)    = a1 <: b1 && a2 <: b2
(a' :=>  b)    <: (a :=>   b')    = a  <: a' && b  <: b'
TypeFlex id a  <: TypeDup  b      = True 
TypeFlex id a  <:          b      = a <: b 
a              <: TypeFlex id b   = a <: b
a              <: TypeVar  b      = True
a              <:          b      = a == b

-- Given a function (or let) body and a bodytype, if the variable bound is used many times
--  it must be unlinear !t. If its used once or zero times it could have either
--  a linear or unlinear type, thus having flex.
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
lookupEnv :: Named -> Infer (Subst, Resolver, Type)
lookupEnv x = do
    env' <- gets env
    case Map.lookup x env' of
        Nothing -> throwError $ NotInScopeError x
        Just  s -> do t <- instantiate s
                      return (nullSubst, nullResolver, t)

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
    where state = St 0 Set.empty (genEnv fs)

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
    s <- evalState (runExceptT (equal qtype itype)) emptyState
    return (name, s)

debangFunc :: Type -> Type
debangFunc (TypeDup (a :=> b)) = a :=> b
debangFunc a = a

-- | Gives the unified type of the type from the type signature
--   and the inferred type.  
equal :: Type -> Type -> Infer Type
equal typ inf = do
    (sub,res) <- unify typ inf -- Try to unify signature with inferred type
    let t = resply res sub inf -- 
    if t <: debangFunc typ 
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
    -- return $ deflexType type'
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
               equal qtype itype
               put $ St 0 linenv env

-- | Run typechecker on program
typecheck :: [Function] -> Either TypeError ()
typecheck funcs = void $ evalState (runExceptT (tc funcs)) state
    where 
        state = St 0 Set.empty (genEnv funcs)

testTc :: String -> Either TypeError ()
testTc = typecheck . run