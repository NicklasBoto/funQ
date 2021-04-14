module Type.TypeChecker where
import AST.AST
import Control.Monad.Except
import qualified Data.Map as M
import qualified Data.Set as S
import Data.String

-- | Typecheck a program.
--   Returns TypeError on failure an unit on success.
typecheck :: Program -> Either TypeError ()
typecheck program = runExcept (typecheckP program)

-- | Parse a typecheck a program encoded as a string.
tcStr :: String -> Either TypeError ()
tcStr = typecheck . run

-- | All type errors that can occur.
data TypeError
    = NotFunction Type         -- ^ A type was expected to be a function but was not.
    | Mismatch Type Type       -- ^ Expected a type but found another type.
    | NotProduct Type          -- ^ A type was expected to be a product but was not.
    | LinearNotAllowed String  -- ^ A function that should not be linear was linear.
    | NotLinear String         -- ^ A function that is linear used many times.
    | NoCommonSuper Type Type  -- ^ No common supertype was found
    | NotInScope String        -- ^ A function was not in scope.
    | DuplicateFunction String -- ^ A function was defined multiple times.
    deriving (Show, Eq)

instance IsString Type where
    fromString t = t'
        where [Func _ t' _] = run $ "f : " ++ t ++ " f = *"

-- | Typecheck the program inside Check monad.
typecheckP :: Program -> Check ()
typecheckP program = do
    checkNamesUnique program
    checkTopLevelUnlinear top
    mapM_ (typecheckF top) program
    where
        top = buildTopEnv program

-- Typechecks the function inside the check monad.
typecheckF :: TopEnv -> Function -> Check ()
typecheckF top (Func name type' term) = do
    t <- inferTerm [] top term
    if t <: type' then
        return ()
    else
        throwError (Mismatch type' t)

data Env
    = Env
    { linEnv :: S.Set String
    , topEnv :: M.Map String Type
    }

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


-- | Check that all top level names are unlinear.
checkTopLevelUnlinear :: TopEnv -> Check ()
checkTopLevelUnlinear env = mapM_ checkFuncUnlinear (M.toList env)
    where
        checkFuncUnlinear :: (String, Type) -> Check ()
        checkFuncUnlinear (name, TypeDup a) = return ()
        checkFuncUnlinear (name, a) = throwError $ LinearNotAllowed name


-- | Ability to throw type errors when type checking.
type Check a = ExceptT TypeError (Reader Env) a

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
            Idx i      -> if absl == i then 0 else 1
            Abs _ e    -> headCount' (absl+1) e
            App f arg  -> headCount' absl f + headCount' absl arg
            IfEl c t f -> headCount' absl c + max (headCount' absl t) (headCount' absl f)
            Tup l r    -> headCount' absl l + headCount' absl r
            Let eq inn -> headCount' absl eq + headCount' (absl+2) inn
            _          -> 0

-- | Infer the type of a term.
inferTerm :: [Type] -> Term -> Check Type
inferTerm _ _ _ Unit      = return $ TypeDup TypeUnit
inferTerm _ _ _ (Bit _)   = return $ TypeDup TypeBit
inferTerm _ _ _ New       = return $ TypeDup (TypeBit :=> TypeQBit)
inferTerm _ _ _ Meas      = return $ TypeDup (TypeQBit :=> TypeDup TypeBit)
inferTerm _ _ _ (Gate g)  = return $ inferGate g
inferTerm ctx top lenv (Abs t e) = do
    et <- inferTerm (t:ctx) top lenv e
    if any (\idx -> isLinear (ctx !! fromIntegral idx)) (freeVars (Abs t e)) then
        return (t :=> et)
    else
        return $ TypeDup (t :=> et)
inferTerm ctx top lenv (Let eq inn) = do
    teq <- inferTerm ctx top lenv eq
    let nBangs = numBangs teq
    case debangg teq of
        (a1 :>< a2) -> do
            let a1t = addBangs nBangs a1
            let a2t = addBangs nBangs a2
            inferTerm (a2t : a1t : ctx) top lenv inn
        _ -> throwError $ NotProduct teq
inferTerm ctx top lenv (App f arg) = do
    tf <- inferTerm ctx top lenv f
    argT <- inferTerm ctx top lenv arg

    -- tf must be a function and the passed argument must be a subtype of the function argument.
    case debangg tf of
        (fArg :=> fRet) | argT <: fArg -> return fRet
                        | otherwise -> throwError $ Mismatch fArg argT
        _ -> throwError $ NotFunction tf
inferTerm ctx top lenv (Tup l r) = do
    lt <- inferTerm ctx top lenv l
    rt <- inferTerm ctx top lenv r
    return $ shiftBang (lt :>< rt)
inferTerm ctx _ lenv (Idx i) = return $ ctx !! fromIntegral i
inferTerm _ top lenv (Fun fun) = case M.lookup fun top of
    Nothing -> throwError $ NotInScope fun
    Just t | isLinear t -> if Set.member fun linenv
                                then throwError $ NotLinear t
                                else 
inferTerm ctx top lenv (IfEl c t f) = do
    tc <- inferTerm ctx top lenv c
    tt <- inferTerm ctx top lenv t
    tf <- inferTerm ctx top lenv f
    if tc <: TypeBit 
        then sup tt tf
        else throwError $ Mismatch TypeBit tc


-- sup : Type -> Type -> Type
-- sup alpha beta = if alpha == beta alpha else fail
-- sup alpha !A = ! (sup alpha A)
-- sup alpha A = fail

-- sup !A !B = ! (sup A B)

-- sup (A x B) !C = sup !(A x B) !C
--                = ! (sup (A x B) C)
-- sup (A x B) (C x D) = (sup A C) x (sup B D)

-- sup (A -> B) (C -> D) = (inf A C) -> (sup B D)

-- inf : Type -> Type -> Type
-- ...
-- inf alpha !A = inf alpha A
-- inf (A x B) !C = inf (A x B) C

-- inf (A x B) (C x D) = (inf A C) x (inf B D)
-- inf (A -> B) (C -> D) = (sup A C) -> (inf B D)

-- sup A A = A
-- sup A B = sup B A
-- sup A (sup B C) = sup (sup A B) C
-- A <: B <-> sup A B = B
--        <-> inf A B = A

-- !A ^ !B = !(A ^ B)
-- !A ^  B = !(A ^ B)

-- | Find the largest commont subtype (greatest lower bound).
--   Throws error if no common subtype exists.
inf :: Type -> Type -> Check Type
inf a b | a == b    = return a
inf (TypeDup a) (TypeDup b) = TypeDup <$> inf a b
inf (TypeDup a) b = TypeDup <$> inf a b
inf a (TypeDup b) = TypeDup <$> inf a b
inf (a :>< b) (c :>< d) = (:><) <$> inf a c <*> inf b d
inf (a :=> b) (c :=> d) = (:=>) <$> sup a c <*> inf b d -- NOTE: contravariance of negative type
inf a b = throwError (NoCommonSuper a b)

-- | Finds the smallest common supertype (least upper bound).
--   Throws error if no common supertype exists.
sup :: Type -> Type -> Check Type
sup a b | a == b = return a
sup (TypeDup a) (TypeDup b) = sup a b
sup (TypeDup a) b = sup a b
sup a (TypeDup b) = sup a b
sup (a :>< b) (c :>< d) = (:><) <$> sup a c <*> sup b d
sup (a :=> b) (c :=> d) = (:=>) <$> inf a c <*> sup b d -- NOTE: contravariance of negative type
sup a b = throwError (NoCommonSuper a b)

-- sup !QBit QBit
-- ! (sup QBit QBit)
-- ! QBit

-- !QBit <: QBit
-- Expects QBit since it is the supertype.


-- sup (TypeVar a) (TypeVar b) = if a == b 
--             then return (TypeVar a)
--             else throwError $ NoCommonSuper (TypeVar a) (TypeVar b)




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
        n = case g of
            GFRDK -> 3
            GTOF  -> 3
            GSWP  -> 2
            GCNOT -> 2
            _     -> 1
        arg = foldr (:><) TypeQBit (replicate (n-1) TypeQBit)
