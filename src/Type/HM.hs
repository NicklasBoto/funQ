{-# LANGUAGE LambdaCase #-}

module Type.HM where

import AST.AST
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.State
import Control.Monad.Except
import Parser.Print
import Data.List (nub, intercalate)
import Debug.Trace

-- | Counter for creating fresh type variables
type Counter = Int

type TVar = String

-- | Representations of free and bound variables in lambda abstractions
data Named
    = Free String
    | Bound Integer
    deriving (Eq, Ord, Show)

-- | A scheme has a type, together with a list of (Forall) bound type variables.
data Scheme = Forall [FlexVar] Type

-- | A FlexVar could either by a normal type variable "a",
--   or the more general type variable "?a".
data FlexVar
    = TVar String
    | FVar String
    deriving (Eq, Ord)


instance Show FlexVar where
    show (TVar a) = a
    show (FVar a) = "?" ++ a

instance Show Scheme where
    show (Forall [] t) = printTree (reverseType t)
    show (Forall vs t) = "∀ " ++ unwords (map show vs) ++ " . "
                      ++ printTree (reverseType t)

-- | Each variable in the TypeEnv has an associated type Scheme.
newtype TypeEnv = TypeEnv (Map.Map Named Scheme) deriving Show

-- | A substitution is just a map from a type variable to the type it 
--  should be substituted with. It could be another type variable.
type Subst = Map.Map FlexVar Type

-- | The null substitution are the substitution where nothing are substituted.
nullSubst :: Subst
nullSubst = Map.empty

-- | The empty environment are the type environment where no variables exist.
emptyEnv :: TypeEnv
emptyEnv = TypeEnv Map.empty

-- | Type errors that can occur during type check
data TypeError
    = NotInScopeError Named
    | InfiniteTypeError TVar Type
    | UnificationFailError Type Type
    | SubtypeFailError Type Type
    | ProductDuplicityError Type Type
    | LinearityError Term
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

    show (NotInScopeError (Free v)) =
        "Variable not in scope: " ++ v

    show (InfiniteTypeError v t) =
        "Occurs check: cannot construct the infinite type: " ++
        show v ++ " ~ " ++ show t

    show (ProductDuplicityError l r) =
        "Linear product operands must have equal duplicity: " ++ show l ++ " ⊗  " ++ show r

type Infer = ExceptT TypeError (State Counter)

-- | Extend the type environment with a new bound variable together with its type scheme.
extend :: TypeEnv -> (Named, Scheme) -> TypeEnv
extend (TypeEnv env) (n,s) = TypeEnv $ Map.insert n s env

-- | Run the inferement code. The result is a type scheme or an exception.
runInfer :: Infer (Subst, Type) -> Either TypeError Scheme
runInfer m = case evalState (runExceptT m) 0 of
        Left  err -> Left err
        Right res -> Right $ closeOver res

-- | 
closeOver :: (Subst, Type) -> Scheme
closeOver (sub, ty) = normalize sc
  where sc = generalize emptyEnv (apply sub ty)

-- | 
normalize :: Scheme -> Scheme
normalize (Forall ts body) = Forall (fmap (TVar . snd) ord) (normtype body)
  where
    ord = zip (nub $ fv body) letters
    
    fv (TypeVar  a) = [a]
    fv (TypeFlex a) = [a]
    fv (a :=> b)    = fv a ++ fv b
    fv (a :>< b)    = fv a ++ fv b
    fv _            = []

    normtype (a :=> b) = normtype a :=> normtype b
    normtype (a :>< b) = normtype a :>< normtype b
    normtype (TypeVar a)   =
      case lookup a ord of
        Just x -> TypeVar x
        Nothing -> error "type variable not in signature"
    normtype (TypeFlex a) =
        case lookup a ord of
            Just x -> TypeFlex x
            Nothing -> error "type flexible not in signature"
    normtype a = a

-- | 
compose :: Subst -> Map.Map FlexVar Type -> Map.Map FlexVar Type
compose s1 s2 = Map.map (apply s1) s2 `Map.union` s1

(∘) :: Subst -> Map.Map FlexVar Type -> Map.Map FlexVar Type
(∘) = compose

class Bangable a where
    bang   :: a -> a
    debang :: a -> a

instance Bangable Type where
    bang = TypeDup
    debang (TypeDup a) = a
    debang          a  = a

instance Bangable Scheme where
    bang (Forall a t) = Forall a $ bang t
    debang (Forall a (TypeDup t)) = Forall a t

instance Bangable TypeEnv where
    bang (TypeEnv e) = TypeEnv $ Map.map bang e
    debang (TypeEnv e) = TypeEnv $ Map.map debang e

class Substitutable a where
    -- | Given a substitution and a type, if the type contains instances of type variables
    --   that also exist in the substitution map, it replaces those instances with the new
    --   ones in the map.
    apply :: Subst -> a -> a
    -- | Find all free type variables.
    ftv   :: a -> Set.Set FlexVar

instance Substitutable Type where
    -- | TypeVariables in the type are substituted if they exist in the substitution.
    apply _ TypeBit = TypeBit
    apply _ TypeQBit = TypeQBit
    apply _ TypeUnit = TypeUnit
    apply s (TypeDup d) = TypeDup $ apply s d
    apply s t@(TypeVar v) = Map.findWithDefault t (TVar v) s
    apply s t@(TypeFlex v) = Map.findWithDefault t (FVar v) s 
    apply s (t1 :=> t2) = apply s t1 :=> apply s t2
    apply s (t1 :>< t2) = apply s t1 :>< apply s t2

    -- | The free type variables for a type are all type variables in the type,
    --   since no type variables are bound.
    ftv (TypeVar v) = Set.singleton (TVar v)
    ftv (TypeFlex v) = Set.singleton (FVar v)
    ftv (t1 :=> t2) = Set.union (ftv t1) (ftv t2)
    ftv _constant   = Set.empty

instance Substitutable Scheme where
    -- | The type inside the scheme are applied to the substitution,
    --   with the bound type variables removed from the substitution.
    apply s (Forall as t) = Forall as $ apply s' t
        where s' = foldr Map.delete s as

    -- | All free type variables in a scheme is the type variables
    --   of the type, except for the ones that are bound by the Forall.
    ftv (Forall as t) = Set.difference (ftv t) (Set.fromList as)

instance Substitutable a => Substitutable [a] where
    apply = fmap . apply
    ftv = foldr (Set.union . ftv) Set.empty

instance Substitutable TypeEnv where
    apply s (TypeEnv env) = TypeEnv $ Map.map (apply s) env
    ftv (TypeEnv env) = ftv $ Map.elems env

occursCheck ::  Substitutable a => FlexVar -> a -> Bool
occursCheck a t = a `Set.member` ftv t

-- ?a ~  t : [?a/t]
--  t ~ ?a : [?a/t]
-- ?a ~ !t : [?a/!t]
-- ?a ~ ?b : [a/b]


-- | Tries to find common type for two input types
unify :: Type -> Type -> Infer Subst
unify (TypeDup (l :=> r)) (l' :=> r') = do
        s1 <- unify l l'
        s2 <- unify (apply s1 r) (apply s1 r')
        return (s2 ∘ s1)
unify (l :=> r) (l' :=> r') = do
        s1 <- unify l l'
        s2 <- unify (apply s1 r) (apply s1 r')
        return (s2 ∘ s1)
unify (TypeDup (l :>< r)) (l' :>< r') = do
    s1 <- unify (TypeDup l) l'
    s2 <- unify (TypeDup r) r'
    return (compose s2 s1)
unify (l :>< r) (TypeDup (l' :>< r')) = do
    s1 <- unify l (TypeDup l')
    s2 <- unify r (TypeDup r')
    return (compose s2 s1)
unify (l :>< r) (l' :>< r') = do
    s1 <- unify l l'
    s2 <- unify r r'
    return (compose s2 s1)
unify (TypeFlex a) (TypeFlex b) = bind (TVar a) (TypeVar b)
unify (TypeFlex a) (TypeDup  b) = bind (FVar a) (TypeDup b)
unify t (TypeFlex b) = bind (FVar b) t
unify (TypeFlex a) t = bind (FVar a) t
unify (TypeVar a) t =  bind (TVar a) t
unify t (TypeVar a) = bind (TVar a) t
unify (TypeDup a) (TypeDup b) = unify a b
unify t t' | t' <: t && isConstType t = return nullSubst
           | otherwise = throwError $ UnificationFailError t t'



-- !a ~ !Bit
-- [a/Bit]
-- [a/!Bit]
-- a Bit

-- Bit !~ !Bit


-- !(a >< a)

-- [a/!Bit] !a = !Bit

-- c ~ c : []
-- a ~ a : [] 

-- a notin ftv(t)
-- --------------
-- a ~ t : [a/t]

-- a notin ftv(t)
-- --------------
-- t ~ a : [a/t]

-- t1 ~ t1' : th1   [th1]t2 ~ [th1]t2' : th2
-- -----------------------------------------
--     t1t2 ~ t1't2' : th2 . th1


-- ?a ~  t : [?a/t]
--  t ~ ?a : [?a/t]
-- ?a ~ !t : [?a/!t]
-- ?a ~ ?b : [a/b]
-- 


replaceSig :: Type -- ^ Inferred type
           -> Type -- ^ Type signature
           -> Infer Type
replaceSig (il :=> ir) (tl :=> tr) = do
    n <- replaceSig il tl
    p <- replaceSig ir tr
    return (n :=> p)
replaceSig (il :>< ir) (tl :>< tr) = do
    n <- replaceSig il tl
    p <- replaceSig ir tr         
    return (n :>< p)
replaceSig a b | a <: b    = return b
               | otherwise = throwError $ SubtypeFailError a b
-- replaceSig t@(TypeVar a) b | occursCheck a b = throwError $ InfiniteTypeError a b
--                            | b <: t          = return b
--                            | otherwise       = throwError $ UnificationFailError b t

-- check identity substitution

--Binds a flexVar with a type and creates a substitution
bind :: FlexVar -> Type -> Infer Subst
bind a'@(TVar a) t | t == TypeVar a   = return nullSubst
                   | occursCheck a' t = throwError $ InfiniteTypeError a t
                   | otherwise        = return $ Map.singleton a' t
bind a'@(FVar a) t | t == TypeFlex a  = return nullSubst
                   | occursCheck a' t = throwError $ InfiniteTypeError a t
                   | otherwise        = return $ Map.singleton a' t

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
  s <- get
  put (s+1)
  return $ TypeVar $ letters !! s

freshFlex :: Infer Type
freshFlex = do
    s <- get
    put (s+1)
    return $ TypeFlex $ letters !! s

-- | Returns a list of strings used for fresh type variables.
letters :: [String]
letters = [1..] >>= flip replicateM ['a'..'z']

-- | Renames all FlexVars in the scheme and applies it
--   to get a type with unique (and free) type variables.
instantiate :: Scheme -> Infer Type
instantiate (Forall as t) = do
    as' <- mapM rename as -- Each bound variable gets a new name.
    let s = Map.fromList $ zip as as' -- Map from old type variables to new names.
    return $ apply s t

rename :: FlexVar -> Infer Type 
rename (FVar _) = freshFlex
rename (TVar _) = fresh

-- | 
generalize :: TypeEnv -> Type -> Scheme
generalize env t  = Forall as t
    where as = Set.toList $ ftv t `Set.difference` ftv env

-- | Infers a substitution and a type from a Term
infer :: Integer -> TypeEnv -> Term -> Infer (Subst, Type)
infer i env (Idx j)      = lookupEnv env (Bound (i-j-1))
infer i env (QVar var)   = lookupEnv env (Free var)
infer i env (Bit _)      = return (nullSubst, bang TypeBit)
infer i env (Gate gate)  = return (nullSubst, inferGate gate)
infer i env (Tup l r)  = do
    (ls, lt) <- infer i env l
    (rs, rt) <- infer i env r
    t <- productExponential lt rt
    return (ls ∘ rs, t)
infer i env (App l r) = do
    tv <- fresh
    (s1,t1) <- infer i env l
    (s2,t2) <- infer i (apply s1 env) r
    subtypeCheck t1 t2
    s3      <- unify (apply s2 t1) (t2 :=> tv)
    return (s3 ∘ s2 ∘ s1, apply s3 tv)
infer i env (IfEl b l r) = do
    (s1,t1) <- infer i env b
    (s2,t2) <- infer i env l
    (s3,t3) <- infer i env r
    s4 <- unify TypeBit t1
    s5 <- unify t2 t3
    return (s5 ∘ s4 ∘ s3 ∘ s2 ∘ s1, apply s5 t2)
-- infer i env (Let eq inn) = do -- let (a1, a0) = eq in inn
--     (s1, t1) <- infer i env eq -- 
--     tv1 <- inferDuplicity (Abs inn) <$> fresh -- Create a typevar for a1 and set its duplicity
--     tv2 <- inferDuplicity      inn  <$> fresh -- Create a typevar for a0 and set its duplicity
--     prods <- trace ("t1:" ++ show t1 ++ " | tv1:" ++ show tv1 ++ " | tv2: " ++ show tv2) $ unify t1 (tv1 :>< tv2) -- The type of eq must be unified with (tv1 :>< tv2).
--     let env' = trace ("prods is " ++ show prods) $ env `extend` (Bound (i+1), prods `apply` Forall [] tv2) -- Add the bound variable types to env.
--                    `extend` (Bound i, prods `apply` Forall [] tv1)
--     (s2,t2) <- trace ("env is " ++ show env') $ infer (i+2) env' inn -- Infer the type of inn, which is the final type
--     _ <- return $  apply prods t1
--     return (s1 ∘ s2, apply prods t2)

infer i env (Let eq inn) = do -- let (_:tv1, _:tv2) = eq:teq inn
    -- Create typevars for the types of the bound variables
    tv1 <- inferDuplicity (Abs inn) <$> fresh 
    tv2 <- inferDuplicity inn <$> fresh

    -- Infer the type of eq
    (seq, teq) <- infer i env eq 
    product <- unify teq (tv1 :>< tv2)

    -- Add the typevars to the environment to be used when inferring the type of inn
    let env' = env `extend` (Bound (i+1), product `apply` Forall [] tv1) 
                   `extend` (Bound  i   , product `apply` Forall [] tv2)

    -- Infer the type of inn
    (sinn, tinn) <- infer (i+2) env' inn

    -- The type of this expression is the type of inn
    return (seq ∘ product ∘ sinn, seq `compose` sinn `apply` tinn)
    

infer i env (Abs body) = do
    tv <- inferDuplicity body <$> fresh
    let env' = env `extend` (Bound i, Forall [] tv)
    (s1, t1) <- infer (i+1) env' body
    return (s1, apply s1 tv :=> t1)
infer i env New  = return (nullSubst, TypeBit  :=> TypeQBit)
infer i env Meas = return (nullSubst, TypeQBit :=> TypeDup TypeBit)
infer i env Unit = return (nullSubst, TypeDup TypeUnit)

productExponential :: Type -> Type -> Infer Type
productExponential l r
    | nexps l == nexps r = return $ iterate bang (debang l :>< debang r) !! nexps l
    | otherwise = throwError $ ProductDuplicityError l r
    where
        nexps :: Type -> Int
        nexps (TypeDup a) = 1 + nexps a
        nexps a = 0

tr :: (Show a, Monad m) => a -> m ()
tr x = trace (show x) (return ())

-- | Infers type of a Gate
inferGate :: Gate -> Type
inferGate g = bang . gateType $ case g of
    GFRDK -> 3
    GTOF  -> 3
    GSWP  -> 2
    GCNOT -> 2
    _     -> 1
    where
        gateType' n = foldr (:><) TypeQBit (replicate (n-1) TypeQBit)
        gateType  n = gateType' n :=> gateType' n

subtypeCheck :: Type -> Type -> Infer ()
subtypeCheck (a :=> _) b 
    | b <: a    = return ()
    | otherwise = throwError $ SubtypeFailError a b

-- | Return ẃhether a type is a subtype of another type.
(<:) :: Type -> Type -> Bool
TypeDup  a  <: TypeDup  b   = TypeDup a <: b
TypeDup  a  <: TypeFlex b   = True 
TypeDup  a  <:          b   = a  <: b
(a1 :>< a2) <: (TypeDup (b1 :><  b2)) = a1 <: TypeDup b1 && a2 <: TypeDup b2
(a1 :>< a2) <: (b1 :><  b2) = a1 <: b1 && a2 <: b2
(a' :=>  b) <: (a :=>   b') = a  <: a' && b  <: b'
TypeFlex a  <: TypeDup  b   = True 
TypeFlex a  <:          b   = True
a           <: TypeFlex b   = True
a           <: TypeVar  b   = True
a           <:          b   = a == b

(~@) :: Type -> Type -> Type
u ~@ TypeDup a = TypeDup $ u ~@ a
(u :=> v) ~@ (a :=> b) = (u ~@ a) :=> (v ~@ b)
(u :>< v) ~@ (a :>< b) = (u ~@ a) :>< (v ~@ b)
u ~@ a = u

elimExp :: Type -> Type
elimExp (TypeDup a) = elimExp a
elimExp (a :=> b) = elimExp a :=> elimExp b
elimExp (a :>< b) = elimExp a :>< elimExp b
elimExp a = a

flex :: Type -> Type
flex (TypeVar a) = TypeFlex a
flex          t  =          t

deflex :: Type -> Type
deflex (TypeFlex a) = TypeVar a
deflex           t  =         t

-- Given a function (or let) body and a bodytype, if the variable bound is used many times
--  it must be unlinear !t. If its used once or zero times it could have either
--  a linear or unlinear type, thus having flex.
inferDuplicity :: Term -> Type -> Type
inferDuplicity e t
    | headCount e <= 1 = flex t
    | otherwise        = bang t

(!?) :: Term -> Type -> Type
(!?) = inferDuplicity

-- | Looks up a free or bound variable from the environment
lookupEnv :: TypeEnv -> Named -> Infer (Subst, Type)
lookupEnv (TypeEnv env) x = do
    case Map.lookup x env of
        Nothing -> throwError $ NotInScopeError x
        Just  s -> do t <- instantiate s
                      return (nullSubst, t)

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

-- f : !Bit -o !(Bit >< Bit)
-- f y = (\x . (x,x)) y

-- HM{\y . (\x . (x,x) y)} = a => (a * a)
-- a => (a*a) ~ !Bit -o !(Bit >< Bit) : [a/Bit]
-- LC{f_e, f_t}

-- \y : !a . (\x : !a. (x,x)) y

typecheck :: Term -> Either TypeError Scheme
typecheck e = runInfer (infer 0 emptyEnv e)

typecheckProgram :: [Function] -> [Either TypeError Type]
typecheckProgram fs = map (checkFunc env) fs
    where env = genEnv fs

showTypes :: [Either TypeError Type] -> IO ()
showTypes = putStrLn . intercalate "\n\n" . map st
    where st (Left  err) = "*** Exception:\n" ++ show err
          st (Right typ) = show typ

runtc :: String -> [Either TypeError Type]
runtc prog = typecheckProgram (run prog)

runtcFile :: FilePath -> IO [Either TypeError Type]
runtcFile path = runtc <$> readFile path

checkFunc :: TypeEnv -> Function -> Either TypeError Type
checkFunc env (Func name qtype term) = do
    Forall _ itype <- runInfer (infer 0 env term)
    evalState (runExceptT (equal qtype itype)) 0

-- | Type, inferred type 
equal :: Type -> Type -> Infer Type
equal typ inf = do
    -- tr ("signature : " ++ show typ)
    -- tr ("inferred  : " ++ show inf)
    sub <- unify typ inf
    return $ apply sub typ

genEnv :: [Function] -> TypeEnv
genEnv = TypeEnv . Map.fromList . map f
    where f (Func n t _) = (Free n, generalize emptyEnv (elimExp t))

-- | Transforms all FlexVariables to normal linear TypeVariables.
deflexType :: Type -> Type
deflexType (a :=> b) = deflexType a :=> deflexType b
deflexType (a :>< b) = deflexType a :>< deflexType b
deflexType (TypeDup a) = TypeDup $ deflexType a
deflexType a = deflex a


inferExp :: String -> Either TypeError Type
inferExp prog = do
    let [Func _ _ term] = run ("f : a f = " ++ prog)
    Forall _ type' <- runInfer (infer 0 emptyEnv term)
    return $ deflexType type'

uni :: Type -> Type -> Infer Scheme
uni t1 t2 = do
    s <- unify t1 t2
    return $ generalize emptyEnv (apply s t1)

-- f : a
-- f = 0

-- f : Num a =>  a -> Int 
-- f x = 0
