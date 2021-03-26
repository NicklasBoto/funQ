{-# language LambdaCase #-}

module Type.STLC where

import qualified Data.Map as M
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.List
import Control.Monad.Identity
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

    show (Fail s) =
        "Something went wrong\n" ++ s

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
    show (TypeDup t) = "! " ++ show t
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
type Check a = ExceptT Error (ReaderT (M.Map Named Type) (State (M.Map Named Int))) a

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

        tc i = \case
            Unit   -> return TypeUnit
            Bit _  -> return $ TypeDup TypeBit
            New    -> return $ TypeBit :=> TypeQBit
            Meas   -> return $ TypeQBit :=> TypeBit
            Gate g -> return . gateType $ case g of
                            FREDKIN -> 3
                            TOFFOLI -> 3
                            SWAP    -> 2
                            CNOT    -> 2
                            _       -> 1

            Abs t e -> do
                -- linEnv <- get 
                -- Lägga till med count 0. 
                -- let linEnv' = M.insert linEnv (Bound i) 0
                checkLinear e t
                rhs <- inEnv (Bound i) t (tc (i+1) e) -- Skapa ny i counter mapp. 
                -- linEnv <- getITAgain
                -- let count = M.lookup linEnv (Bound i)

                return (t :=> rhs)

            App e1 e2 -> do
                t1 <- tc i e1
                t2 <- tc i e2
                case t1 of
                    (n :=> p) | n == t2   -> return p
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
            
            -- let (??) = eq in inn
            Let eq inn -> tc i eq >>= \case
                    (a1 :>< a2) -> inEnv (Bound (i+1)) a1 
                                 $ inEnv (Bound i) a2 (tc (i+2) inn)

                    teq -> throwError $ NotProduct teq

                --  G1 |- M : (A1 >< A2)   G2, x1 : A1, x2 : A2 |- N : a
                -- ------------------------------------------------------
                --         G1, G2 |- let (x1,x2) = M in N : A

            Idx  j -> lookupVar (Bound (i-j-1)) -- +1 
            QVar x -> lookupVar (Free  x)

checkLinear :: Exp -> Type -> Check ()
checkLinear e = \case
    TypeDup t -> return ()
    _notdup   -> if countOccurance e <= 1
                    then return ()
                    else throwError $ NotLinear e

countOccurance :: Exp -> Integer
countOccurance = cO 0
    where cO i = \case
            Idx   j -> if i - j == 0 then 1 else 0
            Abs _ e -> cO (i+1) e
            App l r -> cO i l + cO i r

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
runCheck c = evalState (runReaderT (runExceptT c) M.empty) M.empty

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
        v <- evl vs e2
        evl (v : vs) a

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

brackets = Tok.brackets lexer
comma = Tok.comma lexer

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
    reserved "["
    x1 <- identifier
    reserved ","
    x2 <- identifier
    reserved "]"
    reservedOp "="
    eq <- term
    reservedOp "in"
    inn <- term
    return $ PLet x1 x2 eq inn

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

tyatom :: Parser Type
tyatom = tylit <|> parens type'

tylit :: Parser Type
tylit =  (reservedOp "Bit" >> return TypeBit) 
     <|> (reservedOp "QBit" >> return TypeQBit)

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

run :: String -> IO ()
run prg = case runE prg of
        Left  err   -> putStrLn $ "*** Exception:\n" ++ show err
        Right (e,t) -> do
            Right v <- runEval prg
            putStrLn $ "foo" ++ " : " ++ show t ++ "\n"
                    ++ "foo" ++ " = " ++ show e ++ "\n\n"
                    ++ "Result: " ++ show v

runFile :: FilePath -> IO ()
runFile path = run =<< readFile path

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