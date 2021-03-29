{-# language LambdaCase #-}

module Type.STLC where

import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.List
import Control.Monad.Identity
import Data.Bifunctor
import Text.Parsec hiding ( State )
import Text.Parsec.String (Parser)
import Text.Parsec.Language (haskellStyle)
import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok
import qualified FunQ as Q

instance MonadFail Q.QM where
    fail = error

data Named
    = Bound Integer
    | Free String
    deriving (Show, Eq, Ord)

data Error
    = Mismatch Type Type
    | NotInScope Named
    | NotFunction Type
    | NotProduct Type
    | NotValueType
    | NotLinear Exp
    | MultipleDeclarations String
    | ParseError String
    | Fail String

instance Show Error where
    show (Mismatch e a) =
        "Couldn't match expected type '" ++ show e ++
        "' with actual type '" ++ show a ++ "'"

    show (NotInScope (Bound j)) =
        "The impossible happened, free deBruijn index"

    show (NotInScope (Free v)) =
        "Variable not in scope: " ++ v
        
    show (ParseError s) =
        "Parsec failed:\n" ++ s

    show (NotProduct t) =
        "Not a factorizable type '" ++ show t ++ "'"

    show (NotLinear e) =
        "Expression breaks linearity constraint: " ++ show e

    show (Fail s) =
        "Something went wrong\n" ++ s

    show (NotFunction t) = show t ++ " is not a function!"

data Type
    = Type :=> Type
    | Type :>< Type
    | TypeDup Type
    | TypeBit
    | TypeQBit
    | TypeUnit
    deriving Eq

instance Show Type where
    show (n :=> p) = show n ++ " ⊸ " ++ show p
    show (a :>< b) = show a ++ " ⊗  " ++ show b
    show (TypeDup t) = "!" ++ show t
    show TypeBit = "Bit"
    show TypeQBit = "QBit"
    show TypeUnit = "⊤"

infixr 1 :=>
infixr 2 :><

data Gate
    = H
    | X
    | Y
    | Z
    | S
    | T
    | I
    | CNOT
    | SWAP
    | FREDKIN
    | TOFFOLI
    deriving Show

data Exp
    = Idx Integer
    | QVar String
    | Bit Q.Bit
    | Gate Gate
    | Tup [Exp]
    | App Exp Exp
    | IfEl Exp Exp Exp
    | Let Exp Exp
    | Abs Type Exp
    | New
    | Meas
    | Unit

instance Show Exp where
    show (Abs t e) = "λ[" ++ show t ++ "]." ++ show e
    show (Idx x) = "{" ++ show x ++ "}"
    show (QVar s) = s
    show (App a b) = "(" ++ show a ++ " " ++ show b ++ ")"
    show (Bit b) = show b
    show New = "new"
    show Meas = "measure"
    show Unit = "*"
    show (Let e i) = "let " ++ show e ++ " in " ++ show i
    show (IfEl b t f) = "if " ++ show b ++ " then " ++ show t ++ " else " ++ show f
    show (Gate g) = show g
    show (Tup es) = show es

type SymT e m = ExceptT Error (ReaderT (M.Map Named e) m)

-- type Check = SymT Type Identity
type Check a = ExceptT Error (ReaderT (M.Map Named Type) (State (S.Set String))) a

inEnv :: Named -> Type -> Check a -> Check a
inEnv x t = local (M.insert x t)

lookupVar :: Named -> Check Type
lookupVar x = do
    env <- ask
    case M.lookup x env of
        Just e  -> return e
        Nothing -> throwError $ NotInScope x

type LinEnv = M.Map Named Int 

typecheck :: Exp -> Check Type
typecheck = tc 0 
    where
        gateType' n = foldr (:><) TypeQBit (replicate (n-1) TypeQBit)
        gateType  n = gateType' n :=> gateType' n
        
        funcType (TypeDup t@(n :=> p)) = t 
        funcType t = t

        tc i = \case
            Unit   -> return TypeUnit
            Bit _  -> return $ TypeDup TypeBit
            New    -> return $ TypeDup (TypeBit :=> TypeQBit)
            Meas   -> return $ TypeDup (TypeQBit :=> TypeDup TypeBit)
            Gate g -> return . TypeDup . gateType $ case g of
                            FREDKIN -> 3
                            TOFFOLI -> 3
                            SWAP    -> 2
                            CNOT    -> 2
                            _       -> 1

            Abs t e -> do
                checkLinear e t
                rhs <- inEnv (Bound i) t (tc (i+1) e) -- Skapa ny i counter mapp. 
                return (t :=> rhs)

            App e1 e2 -> do
                t1 <- tc i e1
                t2 <- tc i e2
                case funcType t1 of  -- typedup (n := p)
                    (n :=> p) | t2 <: n   -> return p
                              | otherwise -> throwError $ Mismatch n t2
                    ty -> throwError $ NotFunction ty

            Tup es -> foldr1 (:><) <$> mapM (tc i) es
            
            IfEl b t f -> do
                tt <- tc i t
                tf <- tc i f
                tb <- tc i b
                case tb of
                    TypeBit | tt == tf  -> return tt
                            | otherwise -> throwError $ Mismatch tt tf
                    _ -> throwError $ Mismatch TypeBit tb
            
            Let eq inn -> do
                teq <- tc i eq
                case teq of
                    (a1 :>< a2) -> do
                        checkLinear inn a2
                        checkLinear (Abs a2 inn) a1
                        inEnv (Bound (i+1)) a1 $ inEnv (Bound i) a2 (tc (i+2) inn)

                    _ -> throwError $ NotProduct teq

            Idx  j -> lookupVar (Bound (i-j-1)) -- +1 

            QVar x -> do
                tx <- lookupVar (Free x)
                case tx of 
                    (TypeDup t) -> return tx 
                    t           -> do 
                         linEnv <- get 
                         if S.member x linEnv
                             then throwError $ NotLinear (QVar x)
                             else modify (S.insert x) >> return tx 


-- f : !(Bit -o Bit)
-- f x = x

-- f : let 

-- f : A -o ((A >< B) -o A)
-- f a b = a
-- f = (\a : A. \b : A >< B -o A. a)

-- f : !(Bit -o QBit)
-- f = new

-- q : QBit
-- q = new 0

-- \x . x

-- Function Bit -o Bit (\x : Bit . x)

-- | Subtyping relation from QLambda 4.1
(<:) :: Type -> Type -> Bool
TypeDup  a  <: TypeDup b   = TypeDup a <: b
TypeDup  a  <:         b   = a  <: b
(a1 :>< a2) <: (b1 :>< b2) = a1 <: b1 && a2 <: b2
(a' :=>  b) <: (a :=>  b') = a  <: a' && b  <: b'
a           <:  b          = a == b

checkLinear :: Exp -> Type -> Check ()
checkLinear e = \case
    TypeDup t -> return ()
    _notdup   -> if headCount e <= 1
                    then return ()
                    else throwError $ NotLinear e

-- | The number of variables bound to the head
headCount :: Exp -> Integer
headCount = cO 0
    where cO i = \case
            Idx   j    -> if i - j == 0 then 1 else 0
            Abs _ e    -> cO (i+1) e
            App l r    -> cO i l + cO i r
            IfEl b t f -> cO i b + max (cO i t) (cO i f)
            Tup xs     -> sum $ map (cO i) xs
            Let e1 e2  -> cO i e1 + cO (i+2) e2
            -- \x . let (a,b) = x in M
            e -> 0

hc = Abs (TypeBit :>< TypeBit) (Let (Idx 0) (Tup [Idx 0, Idx 1])) -- \x : Bit . let (a,b) = x in (a,a)

-- \x : (Bit >< Bit) . let (a,b) = x in \.x+a


-- !(A >< B) ≃ (!A >< !B) 
-- !((A >< B >< C >< D)         
test1, test2, test3, test4, test5, test6, test7, test8, test9 :: (String, Exp)
test1 = ("test1",Abs TypeBit (App New (Bit 0)))
test2 = ("test2",Abs TypeBit (App (Abs TypeBit (Idx 1)) (Bit 0)))
test3 = ("test3",Tup [Bit 0, App New (Bit 1), Abs TypeBit (Idx 0)])
test4 = ("test4",App Meas (App (Gate H) (App New (Bit 0))))
test5 = ("test5",App (snd test2) (Bit 1))
test6 = ("test6",Abs TypeBit (Tup [Idx 0, Idx 0]))
test7 = ("test7",App (snd test6) (Bit 0))
test8 = ("test8",Let (Tup [Bit 0, Unit]) (Idx 1)) -- let (x,y) = (0,*) in y
test9 = ("test9",IfEl New (App New (Bit 0)) (Bit 1))
test10 = ("test10", Abs (TypeQBit :=> TypeBit) (App (Idx 0) (App New (Bit 0))))

runCheck :: Check a -> Either Error a
runCheck c = evalState (runReaderT (runExceptT c) M.empty) S.empty

check :: Exp -> IO ()
check e = case runCheck (typecheck e) of
    Left  err -> putStrLn $ "*** Exception:\n" ++ show err
    Right typ -> putStrLn $ "*** Successful:\n" ++ show typ

test :: (String, Exp) -> IO ()
test (name, exp) = case runCheck (typecheck exp) of
    Left  err -> putStrLn $ "*** Exception:\n" ++ show err
    Right typ -> putStrLn $ name ++ " : " ++ show typ ++ "\n"
                         ++ name ++ " = " ++ show exp

data Value
    = VBit Q.Bit
    | VQBit Q.QBit
    | VUnit
    | VTup [Value]
    | VFunc Type [Value] Exp

instance Show Value where
    show (VBit b)    = show b
    show (VTup bs)   = show bs
    show (VQBit q)   = show q
    show VUnit       = "*"
    show (VFunc t _ e) = case runCheck (typecheck (Abs t e)) of
        Right t' -> show t'
        Left err -> show err

type Eval = SymT Exp Q.QM Value

eval :: Exp -> Eval
eval = ev 0
    where
        ev :: Integer -> Exp -> Eval
        ev i = \case
            Abs _ e -> throwError NotValueType

evl :: [Value] -> Exp -> ExceptT Error Q.QM Value
evl vs = \case
    Bit b -> return $ VBit b

    Unit -> return VUnit

    Tup bs -> VTup <$> mapM (evl vs) bs

    Idx j -> return $ vs !! fromInteger j

    Abs t e -> return $ VFunc t vs e

    App New b -> do
        VBit b' <- evl vs b
        q <- lift $ Q.new b'
        return $ VQBit q

    App Meas q -> do
        VQBit q' <- evl vs q
        b <- lift $ Q.measure q' 
        return $ VBit b

    App (Gate g) q -> case g of
        H -> runGate Q.hadamard q vs
        X -> runGate Q.pauliX q vs
        
    App e1 e2 -> do
        VFunc _ e a <- evl vs e1
        v <- evl e e2
        evl (v : e) a

    IfEl bit l r -> do
        VBit b <- evl vs bit 
        evl vs $ if b == 1 then l else r
    
    Let eq inn -> do 
        VTup [x1, x2] <- evl vs eq 
        evl (x2 : x1 : vs) inn
    
    

    _ -> throwError $ Fail "not implemented"

testE :: Exp -> IO (Either Error Value)
testE s = Q.run $ runExceptT (evl [] s)

-- does not work for n-qubit gates
runGate :: (Q.QBit -> Q.QM Q.QBit) -> Exp -> [Value] -> ExceptT Error Q.QM Value
runGate g q vs = do
    VQBit q' <- evl vs q
    VQBit <$> lift (g q')

runEval :: String -> IO (Either Error Value)
runEval prog = case parseExpr prog of
    Right s -> Q.run $ runExceptT (evl [] s)
    Left  e -> return $ Left e

runE :: String -> Either Error (Exp, Type)
runE prog = do
    e <- parseExpr prog
    t <- runCheck (typecheck e)
    return (e,t)

data PExp
    = PAbs String Type PExp
    | PBit Q.Bit
    | PUnit
    | PNew
    | PMeas
    | PGate Gate
    | PApp PExp PExp
    | PTup PExp PExp
    | PVar String
    | PLet String String PExp PExp
    | PIfEl PExp PExp PExp

instance Show PExp where
    show (PAbs name type' term) = "λ" ++ name ++ " : " ++ show type' ++ " . " ++ show term
    show (PBit b)               = show b 
    show PUnit                  = "*" 
    show PNew                   = "new" 
    show PMeas                  = "meas" 
    show (PGate g)              = show g 
    show (PApp e1 e2)           = show e1 ++ " " ++ show e2 
    show (PTup e1 e2)           = "(" ++ show e1 ++ "," ++ show e2 ++ ")"
    show (PVar s)               = s
    show (PLet a b eq inn)      = "let (" ++ a ++ "," ++ b ++ ")" ++ " = " ++ show eq ++ " in " ++ show inn
    show (PIfEl b l r)          = "if " ++  show b ++ " then " ++ show l ++ " else " ++ show r

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser style
  where ops = [".","\\"]
        names = ["H", "X", "new", "measure"]
        style = haskellStyle {Tok.reservedOpNames = ops,
                              Tok.reservedNames = names,
                              Tok.commentLine = "#"}

reserved :: String -> Parser ()
reserved = Tok.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer

identifier :: Parser String
identifier = Tok.identifier lexer

parens :: Parser a -> Parser a
parens = Tok.parens lexer

contents :: Parser a -> Parser a
contents p = do
  Tok.whiteSpace lexer
  r <- p
  eof
  return r

natural :: Parser Integer
natural = Tok.natural lexer

variable :: Parser PExp
variable = PVar <$> identifier

lambda :: Parser PExp
lambda = do
  reservedOp "\\"
  s <- identifier
  reservedOp ":"
  t <- type'
  reservedOp "."
  e <- expr
  return (PAbs s t e)

bit :: Parser PExp
bit =  (reservedOp "0" >> return (PBit 0))
   <|> (reservedOp "1" >> return (PBit 1))

gate :: Parser PExp
gate =  (reservedOp "H" >> return (PGate H))
    <|> (reservedOp "X" >> return (PGate X))

op :: Parser PExp
op =  (reservedOp "new" >> return PNew)
  <|> (reservedOp "measure" >> return PMeas)
  <|> (reservedOp "*" >> return PUnit)

brackets = Tok.brackets lexer
comma = Tok.comma lexer
semi = Tok.semi lexer

----tup :: Parser PExp
--tup = Tup <$> brackets (sepBy1 expr comma)

tup :: Parser PExp
tup = do
    ts <- brackets (sepBy1 term comma)
    return $ foldr1 PTup ts

ifel :: Parser PExp
ifel = do
    reservedOp "if"
    b <- term
    reservedOp "then"
    t <- term
    reservedOp "else"
    f <- term
    return $ PIfEl b t f

plet :: Parser PExp
plet = do
    reservedOp "let"
    x1 <- identifier
    x2 <- identifier
    reservedOp "="
    eq <- term
    reservedOp "in"
    inn <- term
    return $ PLet x2 x1 eq inn

term :: Parser PExp
term =  parens expr
    <|> bit
    <|> gate
    <|> op
    <|> plet
    <|> tup
    <|> ifel
    <|> variable
    <|> lambda

expr :: Parser PExp
expr = do
  es <- many1 term
  return (foldl1 PApp es)

func :: Parser (String, PExp)
func = do
    name <- identifier
    reservedOp ":="
    term <- expr
    return (name, term)

tyatom :: Parser Type
tyatom = tylit <|> parens type'

tylit :: Parser Type
tylit =  (reservedOp "Bit" >> return TypeBit) 
     <|> (reservedOp "QBit" >> return TypeQBit)
     <|> (reservedOp "T" >> return TypeUnit)

type' :: Parser Type
type' = Ex.buildExpressionParser tyops tyatom
  where
    infixOp x f = Ex.Infix (reservedOp x >> return f)
    prefixOp x f = Ex.Prefix (reservedOp x >> return f)
    tyops = [
        [infixOp "-o" (:=>) Ex.AssocRight],
        [infixOp "><" (:><) Ex.AssocRight],
        [prefixOp "!" TypeDup            ]
      ]

parseExpr :: String -> Either Error Exp
parseExpr input = case parse (contents expr) "<stdin>" input of
    Left  err -> Left . ParseError . show $ err
    Right exp -> Right $ convertExp M.empty exp

parseProg :: String -> Either Error Program
parseProg input = case parse (contents (sepEndBy func semi)) "<stdin>" input of
    Left  err -> Left . ParseError . show $ err
    Right prg -> Right $ map (second (convertExp M.empty)) prg
    
convertExp :: M.Map String Integer -> PExp -> Exp
convertExp env (PAbs var typ term) = Abs typ $ convertExp env' term
    where env' = M.insert var 0 (M.map succ env)
convertExp env (PApp l r) = App (convertExp env l) (convertExp env r)
convertExp env PNew = New
convertExp env PMeas = Meas
convertExp env (PVar var) = case M.lookup var env of
    Just idx -> Idx idx
    Nothing  -> QVar var
convertExp env (PIfEl cond true false) =
    IfEl (convertExp env cond) (convertExp env true) (convertExp env false)
convertExp env (PLet x y eq inn) = Let (convertExp env eq) (convertExp env' inn)
    where env' = M.insert y 1 $ M.insert x 0 (M.map (succ . succ) env)
convertExp env (PTup e1 e2) = Tup $ map (convertExp env) [e1,e2]
convertExp env (PBit b) = Bit b
convertExp env (PGate g) = Gate g
convertExp env PUnit = Unit

type Program = [(String, Exp)]

runExp  :: String -> IO ()
runExp prg = case runE prg of
        Left  err   -> putStrLn $ "*** Exception:\n" ++ show err
        Right (e,t) -> do
            Right v <- runEval prg
            putStrLn $ "foo : " ++ show t ++ "\n"
                    ++ "foo = " ++ show e ++ "\n\n"
                    ++ "Result: " ++ show v

runFile :: FilePath -> IO ()
runFile path = run =<< readFile path

showTyped :: (Named, Type) -> String
showTyped (Free n,t) = n ++ " : " ++ show t

run :: String -> IO ()
run prog = do
    let parsed = case parseProg prog of
                    Left  e -> error $ show e
                    Right p -> p
    let ts = M.toList $ buildEnv parsed 
    let sts = intercalate "\n" (map showTyped ts)
    putStrLn sts 



buildEnv :: Program -> M.Map Named Type
buildEnv = foldl addToEnv M.empty 

addToEnv :: M.Map Named Type -> (String, Exp) -> M.Map Named Type
addToEnv env (name, term) = case evalState (runReaderT (runExceptT (typecheck term)) env) S.empty of
    Left err -> error $ show err
    Right  t -> M.insert (Free name) t env

run1, run2, run3, run4, run5 :: String
run1 = "\\x : Bit . new 0"
run2 = "(\\x: Bit . (\\y: Bit . x) 0) 1" -- Abs TypeBit (App (Abs TypeBit (Idx 1)) (Bit 0))
run3 = "\\ x : Bit . (\\ y : Bit . x) x"
run4 = "[b0, new b1, \\x: Bit . x]" -- Tup [Bit 0, App New (Bit 1), Abs TypeBit (Idx 0)]
run5 = "measure (H (new 0))" -- App Meas (App (Gate H) (App New (Bit 0)))
run6 = "(\\x: Bit . [x,x]) b0"

plus = "(\\x : Bit . (\\x : Bit . \\y : Bit . if x then (if y then 0 else 1) else (if y then 1 else 0)) x x) (measure (H (new 0)))"

-- duplicate = \QBit . [0,0]

-- duplicate   : QBit -o QBit >< QBit
-- duplicate q = (q,q)

reverseExp :: Integer -> Exp -> PExp 
reverseExp i (Idx j)       = PVar $ 'x' : show (i - j - 1)
reverseExp i (QVar s)      = PVar s
reverseExp i (Bit b)       = PBit b 
reverseExp i (Gate g)      = PGate g 
reverseExp i (Tup xs)      = foldr1 PTup $ map (reverseExp i) xs
reverseExp i (App e1 e2)   = PApp (reverseExp i e1) (reverseExp i e2) 
reverseExp i (IfEl c t f)  = PIfEl (reverseExp i c) (reverseExp i t) (reverseExp i f)  
reverseExp i (Let eq inn)  = PLet ('x' : show (i + 1)) ('x' : show i) (reverseExp i eq) (reverseExp (i + 2) inn)
reverseExp i (Abs t e)     = PAbs ('x' : show i) t (reverseExp (i+1) e)
reverseExp i New           = PNew
reverseExp i Meas          = PMeas 
reverseExp i Unit          = PUnit 


-- reverseImTerm :: Integer -> Term -> P.Term
-- reverseImTerm env (Idx idx)    = P.TVar $ P.Var $ 'x' : show (env - idx - 1)
-- reverseImTerm env (QVar s)     = P.TVar $ P.Var s
-- reverseImTerm env (Bit b)      = P.TBit b
-- reverseImTerm env (Gate g)     = P.TGate g
-- reverseImTerm env (Tup (x:xs)) = P.TTup $ P.Tuple (reverseImTerm env x) (map (reverseImTerm env) xs)
-- reverseImTerm env (App  t1 t2) = P.TApp (reverseImTerm env t1) (reverseImTerm env t2)
-- reverseImTerm env (IfEl c t e) = P.TIfEl (reverseImTerm env c) (reverseImTerm env t) (reverseImTerm env e)
-- reverseImTerm env (Let eq inn) = P.TLet (P.Var ('x' : show (env + 1))) (P.Var ('x' : show env)) (reverseImTerm env eq) (reverseImTerm (env + 2) inn)
-- reverseImTerm env (Abs  term)  = P.TLamb (P.Lambda "\\") (P.Var ('x' : show env)) (reverseImTerm (env+1) term)
-- reverseImTerm env New          = P.TVar (P.Var "new")
-- reverseImTerm env Meas         = P.TVar (P.Var "meas")
-- reverseImTerm env Unit         = P.TStar