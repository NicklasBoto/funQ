module Type.HM where

import AST.AST
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.State
import Control.Monad.Except
import Parser.Print
import Data.List (nub, intercalate)

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
    fv _           = []

    normtype (a :=> b) = normtype a :=> normtype b
    normtype (TypeVar a)   =
      case lookup a ord of
        Just x -> TypeVar x
        Nothing -> error "type variable not in signature"
    normtype a = a

compose :: Subst -> Map.Map TVar Type -> Map.Map TVar Type
compose s1 s2 = Map.map (apply s1) s2 `Map.union` s1

class Substitutable a where
    apply :: Subst -> a -> a
    ftv   :: a -> Set.Set TVar

instance Substitutable Type where
    apply _ TypeBit = TypeBit
    apply _ TypeQBit = TypeQBit
    apply _ TypeUnit = TypeUnit
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
        return (s2 `compose` s1)
unify (l :>< r) (l' :>< r') = do
    s1 <- unify l l'
    s2 <- unify r r'
    return (compose s2 s1)
unify (TypeVar a) t = bind a t
unify t (TypeVar a) = bind a t
unify t t' | t == t' && isConstType t = return nullSubst
           | otherwise = throwError $ UnificationFailError t t'

-- check identity substitution

bind :: TVar -> Type -> Infer Subst
bind a t | t == TypeVar a  = return nullSubst
         | occursCheck a t = throwError $ InfiniteTypeError a t
         | otherwise       = return $ Map.singleton a t

-- | Checks if a type is a constant type
isConstType :: Type -> Bool
isConstType t | t == TypeBit  = True
              | t == TypeQBit = True
              | t == TypeUnit = True
              | otherwise     = False

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
infer i env (Bit _)      = return (nullSubst, TypeBit)
infer i env (Gate gate)  = return (nullSubst, inferGate gate)
infer i env (Tup terms)  = do
    s <- mapM (infer i env) terms
    let (ss, ts) = unzip s
    let ts' = foldr1 (:><) ts
    let ss' = foldr1 compose ss
    return (ss',ts')
infer i env (App l r)    = do
    tv <- fresh
    (s1,t1) <- infer i env l
    (s2,t2) <- infer i (apply s1 env) r
    s3      <- unify (apply s2 t1) (t2 :=> tv)
    return (s3 `compose` s2 `compose` s1, apply s3 tv)
infer i env (IfEl b l r) = do
    (s1,t1) <- infer i env b
    (s2,t2) <- infer i env l
    (s3,t3) <- infer i env r
    s4 <- unify t1 TypeBit
    s5 <- unify t2 t3
    return (s5 `compose` s4 `compose` s3 `compose` s2 `compose` s1, apply s5 t2)
infer i env (Let eq inn) = do
    (s1, t1) <- infer i env eq
    tv1 <- fresh
    tv2 <- fresh
    prods <- unify t1 (tv1 :>< tv2)
    let env' = env `extend` (Bound (i+1), prods `apply` Forall [] tv2)
                   `extend` (Bound i, prods `apply` Forall [] tv1)
    (s2,t2) <- infer (i+2) env' inn
    return (s1 `compose` s2, t2)
infer i env (Abs body) = do
    tv <- fresh
    let env' = env `extend` (Bound i, Forall [] tv)
    (s1, t1) <- infer (i+1) env' body
    return (s1, apply s1 tv :=> t1)
infer i env New          = return (nullSubst, TypeBit  :=> TypeQBit)
infer i env Meas         = return (nullSubst, TypeQBit :=> TypeBit)
infer i env Unit         = return (nullSubst, TypeUnit)

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

-- | Looks up a free or bound variable from the environment
lookupEnv :: TypeEnv -> Named -> Infer (Subst, Type)
lookupEnv (TypeEnv env) x = do
    case Map.lookup x env of
        Nothing -> throwError $ NotInScopeError x
        Just  s -> do t <- instantiate s
                      return (nullSubst, t)

typecheck :: Term -> Either TypeError Scheme
typecheck e = runInfer (infer 0 emptyEnv e)

typecheckProgram :: [Function] -> [Either TypeError Scheme]
typecheckProgram fs = map (checkFunc env) fs
    where env = genEnv fs

showTypes :: [Either TypeError Scheme] -> IO ()
showTypes = putStrLn . intercalate "\n\n" . map st
    where st (Left  err) = "*** Exception:\n" ++ show err
          st (Right sch) = show sch

runtc :: String -> [Either TypeError Scheme]
runtc prog = typecheckProgram (run prog)

runtcFile :: FilePath -> IO [Either TypeError Scheme]
runtcFile path = runtc <$> readFile path

checkFunc :: TypeEnv -> Function -> Either TypeError Scheme
checkFunc env (Func name typ term) = do
    sch <- runInfer (infer 0 env term)
    runInfer (equal typ sch)

equal :: Type -> Scheme -> Infer (Subst, Type)
equal typ sch@(Forall vs inf) = do
    sub <- unify inf typ
    return (sub, typ)

genEnv :: [Function] -> TypeEnv
genEnv = TypeEnv . Map.fromList . map f
    where f (Func n t _) = (Free n, generalize emptyEnv t)

-- f : a
-- f = 0

-- f : Num a =>  a -> Int 
-- f x = 0

