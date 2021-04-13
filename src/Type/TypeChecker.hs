module Type.TypeChecker where
import AST.AST
import Control.Monad.Except
import qualified Data.Map as M
import qualified Data.Set as S

-- | Typecheck a program.
--   Returns TypeError on failure an unit on success.
typecheck :: Program -> Either TypeError ()
typecheck program = runExcept (typecheckP program)

-- | Parse a typecheck a program encoded as a string.
tcStr :: String -> Either TypeError ()
tcStr = typecheck . run

-- | All type errors that can occur.
data TypeError = 
    NotFunction Type           -- A type was expected to be a function but was not.
    | Mismatch Type Type       -- Expected a type but found another type.
    | NotProduct Type          -- A type was expected to be a product but was not.
    | LinearNotAllowed String  -- A function that should not be linear was linear.
    | NotInScope String        -- A function was not in scope.
    | DuplicateFunction String -- A function was defined multiple times.
    deriving Show

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
        

-- | Check that all top level names are unlinear.
checkTopLevelUnlinear :: TopEnv -> Check ()
checkTopLevelUnlinear env = mapM_ checkFuncUnlinear (M.toList env)
    where
        checkFuncUnlinear :: (String, Type) -> Check ()
        checkFuncUnlinear (name, TypeDup a) = return ()
        checkFuncUnlinear (name, a) = throwError $ LinearNotAllowed name


-- | Ability to throw type errors when type checking.
type Check a = Except TypeError a

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
inferTerm :: [Type] -> TopEnv -> Term -> Check Type
inferTerm _ _ Unit      = return $ TypeDup TypeUnit
inferTerm _ _ (Bit _)   = return $ TypeDup TypeBit
inferTerm _ _ New       = return $ TypeDup (TypeBit :=> TypeQBit)
inferTerm _ _ Meas      = return $ TypeDup (TypeQBit :=> TypeDup TypeBit)
inferTerm _ _ (Gate g)  = return $ inferGate g
inferTerm ctx top (Abs t e) = do
    et <- inferTerm (t:ctx) top e
    if linears et then
        return (t :=> et)
    else
        return $ TypeDup (t :=> et)
inferTerm ctx top (Let eq inn) = do
    teq <- inferTerm ctx top eq 
    let nBangs = numBangs teq
    case debangg teq of
        (a1 :>< a2) -> do
            let a1t = addBangs nBangs a1
            let a2t = addBangs nBangs a2
            inferTerm (a2t : a1t : ctx) top inn
        _ -> throwError $ NotProduct teq
inferTerm ctx top (App f arg) = do
    tf <- inferTerm ctx top f
    argT <- inferTerm ctx top arg

    -- tf must be a function and the passed argument must be a subtype of the function argument.
    case debangg tf of
        (fArg :=> fRet) | argT <: fArg -> return fRet 
                        | otherwise -> throwError $ Mismatch fArg argT
        _ -> throwError $ NotFunction tf
inferTerm ctx top (Tup l r) = do
    lt <- inferTerm ctx top l
    rt <- inferTerm ctx top r
    return $ shiftBang (lt :>< rt)
inferTerm ctx _ (Idx i) = return $ ctx !! fromIntegral i
inferTerm _ top (Fun fun) = case M.lookup fun top of
    Nothing -> throwError $ NotInScope fun 
    Just t  -> return t
inferTerm ctx top (IfEl c t f) = do
    tc <- inferTerm ctx top c
    tt <- inferTerm ctx top t
    tf <- inferTerm ctx top f
    if tc <: TypeBit then
        if tt == tf then -- Should we require them to be equal? Or to have a common superclass.
            return tt
        else
            throwError $ Mismatch tt tf
    else
        throwError $ Mismatch TypeBit tc

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

-- | Check if there are any linear values in a type.
linears :: Type -> Bool
linears (TypeDup a) = False
-- linears (a :=> b)   = linears b
-- linears (a :>< b)   = linears a || linears b
linears a           = True

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
