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

-- | Type synonyme for type variables 
type TVar = String

-- | Counter for creating fresh type variables
type Counter = Int

-- | Representations of free and bound variables in lambda abstractions
data Named
    = Free String
    | Bound Integer
    deriving (Eq, Ord, Show)

data Scheme = Forall [TVar] Type

instance Show Scheme where
    show (Forall [] t) = printTree (reverseType t)
    show (Forall vs t) = "∀ " ++ unwords vs ++ " . "
                      ++ printTree (reverseType t)

newtype TypeEnv = TypeEnv (Map.Map Named Scheme)

type Subst = Map.Map TVar Type

nullSubst :: Subst
nullSubst = Map.empty

emptyEnv :: TypeEnv
emptyEnv = TypeEnv Map.empty

-- | Type errors that can occur during type check
data TypeError
    = NotInScopeError Named
    | InfiniteTypeError TVar Type
    | UnificationFailError Type Type
    | ProductDuplicityError Type Type
    | LinearityError Term

instance Show TypeError where
    show (UnificationFailError e a) =
        "Couldn't match expected type '" ++ show e ++
        "' with actual type '" ++ show a ++ "'"

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

-- | Extending the environment 
extend :: TypeEnv -> (Named, Scheme) -> TypeEnv
extend (TypeEnv env) (x,s) = TypeEnv $ Map.insert x s env

runInfer :: Infer (Subst, Type) -> Either TypeError Scheme
runInfer m = case evalState (runExceptT m) 0 of
        Left  err -> Left err
        Right res -> Right $ closeOver res

closeOver :: (Map.Map TVar Type, Type) -> Scheme
closeOver (sub, ty) = normalize sc
  where sc = generalize emptyEnv (apply sub ty)

normalize :: Scheme -> Scheme
normalize (Forall ts body) = Forall (fmap snd ord) (normtype body)
  where
    ord = zip (nub $ fv body) letters

    fv (TypeVar a) = [a]
    fv (a :=> b)   = fv a ++ fv b
    fv (a :>< b)   = fv a ++ fv b
    fv _           = []

    normtype (a :=> b) = normtype a :=> normtype b
    normtype (a :>< b) = normtype a :>< normtype b
    normtype (TypeVar a)   =
      case lookup a ord of
        Just x -> TypeVar x
        Nothing -> error "type variable not in signature"
    normtype a = a

compose :: Subst -> Map.Map TVar Type -> Map.Map TVar Type
compose s1 s2 = Map.map (apply s1) s2 `Map.union` s1

(∘) :: Subst -> Map.Map TVar Type -> Map.Map TVar Type
(∘) = compose

class Bangable a where
    bang   :: a -> a
    debang :: a -> a

instance Bangable Type where
    bang = TypeDup
    debang (TypeDup a) = a

instance Bangable Scheme where
    bang (Forall a t) = Forall a $ bang t
    debang (Forall a (TypeDup t)) = Forall a t

instance Bangable TypeEnv where
    bang (TypeEnv e) = TypeEnv $ Map.map bang e
    debang (TypeEnv e) = TypeEnv $ Map.map debang e

class Substitutable a where
    apply :: Subst -> a -> a
    ftv   :: a -> Set.Set TVar

instance Substitutable Type where
    apply _ TypeBit = TypeBit
    apply _ TypeQBit = TypeQBit
    apply _ TypeUnit = TypeUnit
    apply s (TypeDup d) = TypeDup $ apply s d
    apply s t@(TypeVar v) = Map.findWithDefault t v s
    apply s (t1 :=> t2) = apply s t1 :=> apply s t2
    apply s (t1 :>< t2) = apply s t1 :>< apply s t2

    ftv (TypeVar v) = Set.singleton v
    ftv (t1 :=> t2) = Set.union (ftv t1) (ftv t2)
    ftv _constant   = Set.empty

instance Substitutable Scheme where
    apply s (Forall as t) = Forall as $ apply s' t
        where s' = foldr Map.delete s as

    ftv (Forall as t) = Set.difference (ftv t) (Set.fromList as)

instance Substitutable a => Substitutable [a] where
    apply = fmap . apply
    ftv = foldr (Set.union . ftv) Set.empty

instance Substitutable TypeEnv where
    apply s (TypeEnv env) = TypeEnv $ Map.map (apply s) env
    ftv (TypeEnv env) = ftv $ Map.elems env

occursCheck ::  Substitutable a => TVar -> a -> Bool
occursCheck a t = a `Set.member` ftv t

-- | Tries to find common type for two input types
unify :: Type -> Type -> Infer Subst
unify (l :=> r) (l' :=> r') = do
        s1 <- unify l l'
        s2 <- unify (apply s1 r) (apply s1 r')
        return (s2 ∘ s1)
unify (l :>< r) (l' :>< r') = do
    s1 <- unify l l'
    s2 <- unify r r'
    return (compose s2 s1)
unify (TypeVar a) t = bind a t
unify t (TypeVar a) = bind a t
unify (TypeDup t) t' = unify t t'
unify t (TypeDup t') = unify t t'
unify t t' | t == t' && isConstType t = return nullSubst
           | otherwise = throwError $ UnificationFailError t t'

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
replaceSig a b | b <: a    = return b
               | otherwise = throwError $ UnificationFailError b a
-- replaceSig t@(TypeVar a) b | occursCheck a b = throwError $ InfiniteTypeError a b
--                            | b <: t          = return b
--                            | otherwise       = throwError $ UnificationFailError b t

-- check identity substitution

bind :: TVar -> Type -> Infer Subst
bind a t | t == TypeVar a  = return nullSubst
         | occursCheck a t = throwError $ InfiniteTypeError a t
         | otherwise       = return $ Map.singleton a t

-- | Checks if a type is a constant type
isConstType :: Type -> Bool
isConstType TypeBit = True
isConstType TypeQBit = True
isConstType TypeUnit = True
isConstType (TypeDup d) = isConstType d
isConstType _type = False

-- | Introduce a new type variable
fresh :: Infer Type
fresh = do
  s <- get
  put (s+1)
  return $ TypeVar $ letters !! s

-- | Returns a list of string used for fresh type variables
letters :: [String]
letters = [1..] >>= flip replicateM ['a'..'z']

instantiate :: Scheme -> Infer Type
instantiate (Forall as t) = do
    as' <- mapM (const fresh) as
    let s = Map.fromList $ zip as as'
    return $ apply s t

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
    -- t <- case (lt, rt) of
    --         (TypeDup lt', TypeDup rt') -> return $ TypeDup (lt' :>< rt')
    --         (TypeDup lt',         rt') ->
    --             throwError $ UnificationFailError (TypeDup rt') rt'
    --         (        lt', TypeDup rt') ->
    --             throwError $ UnificationFailError (TypeDup lt') lt'
    --         (        lt',         rt') -> return $ lt :>< rt
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
    s4 <- unify t1 TypeBit
    s5 <- unify t2 t3
    return (s5 ∘ s4 ∘ s3 ∘ s2 ∘ s1, apply s5 t2)
infer i env (Let eq inn) = do
    (s1, t1) <- infer i env eq
    tv1 <- fresh
    tv2 <- fresh
    prods <- unify t1 (tv1 :>< tv2)
    let env' = env `extend` (Bound (i+1), prods `apply` generalize emptyEnv tv2)
                   `extend` (Bound i, prods `apply` generalize emptyEnv tv1)
    (s2,t2) <- infer (i+2) env' inn
    return (s1 ∘ s2, t2)
infer i env (Abs body) = do
    tv <- fresh
    let tv' = inferDuplicity body tv
    let env' = env `extend` (Bound i, Forall [] tv')
    (s1, t1) <- infer (i+1) env' body
    return (s1, apply s1 tv' :=> t1)
infer i env New  = return (nullSubst, TypeBit  :=> TypeQBit)
infer i env Meas = return (nullSubst, TypeQBit :=> bang TypeBit)
infer i env Unit = return (nullSubst, TypeUnit)

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
inferGate g = gateType $ case g of
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
    | otherwise = throwError $ UnificationFailError a b

(<:) :: Type -> Type -> Bool
TypeDup  a  <: TypeDup b   = TypeDup a <: b
TypeDup  a  <:         b   = a  <: b
(a1 :>< a2) <: (b1 :>< b2) = a1 <: b1 && a2 <: b2
(a' :=>  b) <: (a :=>  b') = a  <: a' && b  <: b'
a           <: TypeVar b   = True
a           <:         b   = a == b

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

inferDuplicity :: Term -> Type -> Type
inferDuplicity e t
    | headCount e <= 1 =      t
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
            Let _ e    -> cO (i+2) e -- FIXME
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
    -- Forall _ qtype' <- runInfer (equal qtype itype)
    evalState (runExceptT (replaceSig itype qtype)) 0

equal :: Type -> Type -> Infer (Subst, Type)
equal typ inf = do
    sub <- unify inf typ
    return (sub, typ)

genEnv :: [Function] -> TypeEnv
genEnv = TypeEnv . Map.fromList . map f
    where f (Func n t _) = (Free n, generalize emptyEnv (elimExp t))

inferExp :: String -> Either TypeError Type
inferExp prog = do
    let [Func _ _ term] = run ("f : a f = " ++ prog)
    Forall _ type' <- runInfer (infer 0 emptyEnv term)
    return type'

uni :: Type -> Type -> Infer Scheme
uni t1 t2 = do
    s <- unify t1 t2
    return $ generalize emptyEnv (apply s t1)

-- f : a
-- f = 0

-- f : Num a =>  a -> Int 
-- f x = 0
