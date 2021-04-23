{-# LANGUAGE LambdaCase #-}

module Type.TypeChecker where
import AST.AST
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Map as M
import qualified Data.Set as S
import Data.String

runCheck :: Check a -> Either TypeError a
runCheck c = evalState (runReaderT (runExceptT c) M.empty) S.empty

-- | Typecheck a program.
--   Returns TypeError on failure an unit on success.
typecheck :: Program -> Either TypeError ()
typecheck program = evalState (runReaderT (runExceptT (typecheckP program)) top) lin
    where
        top = buildTopEnv program
        lin = S.empty

-- | Parse a typecheck a program encoded as a string.
tcStr :: String -> Either TypeError ()
tcStr = typecheck . run

inferExp :: String -> Either TypeError Type
inferExp s = runCheck $ infer p
    where [Func _ _ p] = run $ "f : a f = " ++ s

-- | All type errors that can occur.
data TypeError
    = NotFunction Type         -- ^ A type was expected to be a function but was not.
    | Mismatch Type Type       -- ^ Expected a type but found another type.
    | NotProduct Type          -- ^ A type was expected to be a product but was not.
    | NotLinearTop String      -- ^ A function that is linear used many times.
    | NotLinearTerm Term Type  -- ^ A term that breaks a linearity constraint
    | NoCommonSuper Type Type  -- ^ No common supertype was found
    | NotInScope String        -- ^ A function was not in scope.
    | DuplicateFunction String -- ^ A function was defined multiple times.
    deriving Eq

instance Show TypeError where
    show (NotFunction t) = 
        show t ++ " is not a function!"

    show (Mismatch e a) =
        "Couldn't match expected type '" ++ show e ++
        "' with actual type '" ++ show a ++ "'"

    show (NotProduct t) =
        "Not a factorizable type '" ++ show t ++ "'"

    show (NotLinearTop f) =
        "Linear function '" ++ f ++ "' is used more than once"
        
    show (NotLinearTerm e t) =
        "Expression breaks linearity constraint: " ++ show (Abs t e)

    show (NoCommonSuper a b) =
        "Could not find common type for type '" ++ show a ++ "' and type '" ++ show b ++ "'"

    show (NotInScope v) =
        "Variable not in scope: " ++ v

    show (DuplicateFunction f) =
        "Function '" ++ f ++ "' declared multiple times"

instance IsString Type where
    fromString t = t'
        where [Func _ t' _] = run $ "f : " ++ t ++ " f = *"

-- | Typecheck the program inside Check monad.
typecheckP :: Program -> Check ()
typecheckP program = do
    checkNamesUnique program
    mapM_ typecheckF program

-- Typechecks the function inside the check monad.
typecheckF :: Function -> Check ()
typecheckF (Func name type' term) = do
    t <- inferTerm [] term
    if t <: type'
        then return ()
        else throwError (Mismatch type' t)

type LinEnv = S.Set String
type TopEnv = M.Map String Type

-- | Builds map between top level names and types.
buildTopEnv :: Program -> TopEnv
buildTopEnv program = M.fromList (map addFunc program)
    where
        addFunc :: Function -> (String, Type)
        addFunc (Func name t _) = (name, t)

-- | Check that all top level names are unique.
checkNamesUnique :: Program -> Check ()
checkNamesUnique = foldM_ f S.empty
    where
        f :: S.Set String -> Function -> Check (S.Set String)
        f visited (Func n _ _)
            | S.member n visited = throwError (DuplicateFunction n)
            | otherwise          = return (S.insert n visited)


-- | Ability to throw type errors when type checking.
type Check a = ExceptT TypeError (ReaderT TopEnv (State LinEnv)) a

-- | Whether a type is a subtype of another type.
(<:) :: Type -> Type -> Bool
TypeDup a <: TypeDup b     = TypeDup a <: b       -- (!)
TypeDup a <: b             = a <: b               -- (D)
(a1 :>< a2) <: (b1 :>< b2) = a1 <: b1 && a2 <: b2 -- (><)
(a' :=> b) <: (a :=> b')   = a  <: a' && b  <: b' -- (-o)
a <: b                     = a == b               -- (ax)

-- | Count how many times the variable bound to the head is used.
headCount :: Term -> Integer
headCount = headCount' 0
    where
        headCount' :: Integer -> Term -> Integer
        headCount' absl term = case term of
            Idx i      -> if absl == i then 1 else 0
            Abs _ e    -> headCount' (absl+1) e
            App f arg  -> headCount' absl f + headCount' absl arg
            IfEl c t f -> headCount' absl c + max (headCount' absl t) (headCount' absl f)
            Tup l r    -> headCount' absl l + headCount' absl r
            Let eq inn -> headCount' absl eq + headCount' (absl+2) inn
            _          -> 0

checkLinear :: Term -> Type -> Check ()
checkLinear e = \case
    TypeDup t -> return ()
    t         -> if headCount e <= 1
                    then return ()
                    else throwError $ NotLinearTerm e t

infer :: Term -> Check Type
infer = inferTerm []

-- | Infer the type of a term.
inferTerm :: [Type] -> Term -> Check Type
inferTerm _ Unit      = return $ TypeDup TypeUnit
inferTerm _ (Bit _)   = return $ TypeDup TypeBit
inferTerm _ New       = return $ TypeDup (TypeBit :=> TypeQBit)
inferTerm _ Meas      = return $ TypeDup (TypeQBit :=> TypeDup TypeBit)
inferTerm _ (Gate g)  = return $ inferGate g
inferTerm ctx (Abs t e) = do
    top <- ask
    checkLinear e t
    et <- inferTerm (t:ctx) e
    if any (\idx -> isLinear (ctx !! fromIntegral idx)) (freeVars (Abs t e)) 
        then return (t :=> et)
        else return $ TypeDup (t :=> et)
inferTerm ctx (Let eq inn) = do
    teq <- inferTerm ctx eq
    let nBangs = numBangs teq
    case debangg teq of
        (a1 :>< a2) -> do
            let a1t = addBangs nBangs a1
            let a2t = addBangs nBangs a2
            checkLinear inn a2
            checkLinear (Abs a2 inn) a1
            inferTerm (a2t : a1t : ctx) inn
        _ -> throwError $ NotProduct teq
inferTerm ctx (App f arg) = do
    tf <- inferTerm ctx f
    argT <- inferTerm ctx arg
    case debangg tf of
        (fArg :=> fRet) | argT <: fArg -> return fRet
                        | otherwise -> throwError $ Mismatch fArg argT
        _ -> throwError $ NotFunction tf
inferTerm ctx (Tup l r) = do
    lt <- inferTerm ctx l
    rt <- inferTerm ctx r
    return $ shiftBang (lt :>< rt)
inferTerm ctx (Idx i) = return $ ctx !! fromIntegral i
inferTerm _ (Fun fun) = do
    top <- ask
    lin <- get
    case M.lookup fun top of
        Nothing -> throwError $ NotInScope fun
        Just t | isLinear t -> if S.member fun lin
                                then throwError $ NotLinearTop fun
                                else modify (S.insert fun) >> return t
               | otherwise -> return t
inferTerm ctx (IfEl c t f) = do
    tc <- inferTerm ctx c
    linc <- get

    tt <- inferTerm ctx t
    lint <- get
    put linc

    tf <- inferTerm ctx f
    linf <- get

    put (S.union lint linf)

    if tc <: TypeBit 
        then supremum tt tf
        else throwError $ Mismatch TypeBit tc

-- | Find the largest commont subtype (greatest lower bound).
--   Throws error if no common subtype exists.
infimum :: Type -> Type -> Check Type
infimum a b | a == b    = return a
infimum (TypeDup a) (TypeDup b) = TypeDup <$> infimum a b
infimum (TypeDup a) b = TypeDup <$> infimum a b
infimum a (TypeDup b) = TypeDup <$> infimum a b
infimum (a :>< b) (c :>< d) = (:><) <$> infimum  a c <*> infimum b d
infimum (a :=> b) (c :=> d) = (:=>) <$> supremum a c <*> infimum b d -- NOTE: contravariance of negative type
infimum a b = throwError (NoCommonSuper a b)

-- | Finds the smallest common supertype (least upper bound).
--   Throws error if no common supertype exists.
supremum :: Type -> Type -> Check Type
supremum a b | a == b = return a
supremum (TypeDup a) (TypeDup b) = supremum a b
supremum (TypeDup a) b = supremum a b
supremum a (TypeDup b) = supremum a b
supremum (a :>< b) (c :>< d) = (:><) <$> supremum a c <*> supremum b d
supremum (a :=> b) (c :=> d) = (:=>) <$> infimum  a c <*> supremum b d -- NOTE: contravariance of negative type
supremum a b = throwError (NoCommonSuper a b)

-- | Unwraps as many ! as possible from a type.
debangg :: Type -> Type
debangg (TypeDup a) = debangg a
debangg a           = a

-- | Moves as many common bangs as possible from inside a tuple to the outside.
shiftBang :: Type -> Type
shiftBang (TypeDup a :>< TypeDup b) = TypeDup (shiftBang (a :>< b))
shiftBang a = a

-- | Return how many ! a type is wrapped in.
numBangs :: Type -> Integer
numBangs (TypeDup a) = 1 + numBangs a
numBangs a = 0

-- | Wrap a type in some number of !.
addBangs :: Integer -> Type -> Type
addBangs 0 a = a
addBangs n a = addBangs (n-1) (TypeDup a)

-- | Whether a type is linear and not wrapped in !.
isLinear :: Type -> Bool
isLinear (TypeDup _) = False
isLinear _           = True

-- | Finds all free de bruijn variables in a term.
freeVars :: Term -> [Integer] -- todo: make sure it is not off by one and the result integers makes sense according to the callee.
freeVars = freeVars' 0
    where
        freeVars' :: Integer -> Term -> [Integer]
        freeVars' n (Tup l r)    = freeVars' n l ++ freeVars' n r
        freeVars' n (App f a)    = freeVars' n f ++ freeVars' n a
        freeVars' n (Let eq inn) = freeVars' n eq ++ freeVars' (n+2) inn
        freeVars' n (Abs _ e)    = freeVars' (n+2) e
        freeVars' n (Idx i)      = [n - i | i >= n]
        freeVars' _ _            = []

-- | Infer type of a gate.
inferGate :: Gate -> Type
inferGate g = TypeDup (arg :=> arg)
    where
        arg = foldr (:><) TypeQBit (replicate (n-1) TypeQBit)
        n = case g of
                GQFT   -> 1 -- temp
                GQFTI  -> 1 -- temp
                GQFT2  -> 2 -- temp
                GQFTI2 -> 2 -- temp
                GQFT3  -> 3 -- temp
                GQFTI3 -> 3 -- temp
                GQFT4  -> 4 -- temp
                GQFTI4 -> 4 -- temp
                GQFT5  -> 5 -- temp
                GQFTI5 -> 5 -- temp
                GFRDK  -> 3
                GTOF   -> 3
                GCCR   -> 3
                GCCR2  -> 3
                GCCR4  -> 3
                GCCR8  -> 3
                GSWP   -> 2
                GCNOT  -> 2
                GCR    -> 2
                GCRD   -> 2
                GCR2   -> 2
                GCR2D  -> 2
                GCR4   -> 2
                GCR4D  -> 2
                GCR8   -> 2
                GCR8D  -> 2
                _      -> 1
