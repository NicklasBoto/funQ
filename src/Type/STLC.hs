{-# language LambdaCase #-}

module Type.STLC where

import qualified Data.Map as M
import Control.Monad.Except
import Control.Monad.Reader
import Data.List
import Control.Monad.Identity
import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Language (haskellStyle)
import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok
import FunQ hiding ( run )

data Named
    = Bound Integer
    | Free String
    deriving (Show, Eq, Ord)

data Error
    = Mismatch Type Type
    | NotInScope Named
    | NotFunction Type
    | NotValueType
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

    show (Fail s) =
        "Something went wrong\n" ++ s

data Type
    = Type :=> Type
    | Type :>< Type
    | TypeBit
    | TypeQBit
    | TypeVoid
    deriving Eq

instance Show Type where
    show (n :=> p) = show n ++ " ⊸ " ++ show p
    show (a :>< b) = show a ++ " ⊗  " ++ show b
    show TypeBit = "Bit"
    show TypeQBit = "QBit"
    show TypeVoid = "⊤"

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
    = Abs Type Exp
    | Idx Integer
    | Var String
    | App Exp Exp
    | Let Exp Exp
    | IfEl Exp Exp Exp
    | New
    | Meas
    | Gate Gate
    | Bit Bit
    | Tup [Exp]

instance Show Exp where
    show (Abs t e) = "λ[" ++ show t ++ "]." ++ show e
    show (Idx x) = "{" ++ show x ++ "}"
    show (Var s) = s
    show (App a b) = "(" ++ show a ++ " " ++ show b ++ ")"
    show (Bit b) = show b
    show New = "new"
    show Meas = "measure"
    show (Gate g) = show g
    show (Tup es) = show es

type SymT e m = ExceptT Error (ReaderT (M.Map Named e) m)

type Check = SymT Type Identity

extend :: Named -> a -> M.Map Named a -> M.Map Named a
extend = M.insert

inEnv :: Monad m => Named -> e -> SymT e m a -> SymT e m a
inEnv x t = local (extend x t)

lookupVar :: Monad m => Named -> SymT e m e
lookupVar x = do
    env <- ask
    case M.lookup x env of
        Just e  -> return e
        Nothing -> throwError $ NotInScope x

typecheck :: Exp -> Check Type
typecheck = tc 0
    where
        gateType' n = foldr (:><) TypeQBit (replicate (n-1) TypeQBit)
        gateType  n = gateType' n :=> gateType' n

        tc i = \case
            Bit _  -> return TypeBit
            New    -> return (TypeBit :=> TypeQBit)
            Meas   -> return (TypeQBit :=> TypeBit)
            Gate g -> return . gateType $ case g of
                            FREDKIN -> 3
                            TOFFOLI -> 3
                            SWAP    -> 2
                            CNOT    -> 2
                            _       -> 1

            Abs t e -> do
                rhs <- inEnv (Bound i) t (tc (i+1) e)
                return (t :=> rhs)

            App e1 e2 -> do
                t1 <- tc i e1
                t2 <- tc i e2
                case t1 of
                    (n :=> p) | n == t2   -> return p
                              | otherwise -> throwError $ Mismatch n t2
                    ty -> throwError $ NotFunction ty

            Tup es -> foldr1 (:><) <$> mapM (tc i) es

            Idx j -> lookupVar (Bound j)
            Var x -> lookupVar (Free  x)

test1 = ("test1", Abs TypeBit (App New (Bit 0)))
test2 = ("test2",Abs TypeBit (App (Abs TypeBit (Idx 1)) (Bit 0)))
test3 = ("test3",Tup [Bit 0, App New (Bit 1), Abs TypeBit (Idx 0)])
test4 = ("test4",App Meas (App (Gate H) (App New (Bit 0))))
test5 = ("test5",App (snd test2) (Bit 1))
test6 = ("test6",Abs TypeBit (Tup [Idx 0, Idx 0]))
test7 = ("test7",App (snd test6) (Bit 0))

runCheck :: Check a -> Either Error a
runCheck c = runIdentity (runReaderT (runExceptT c) M.empty)

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
    = VBit Bit
    | VTup [Value]
    | VFunc [Value] Exp

instance Show Value where
    show (VBit b)  = show b
    show (VTup bs) = show bs
    show VFunc{}   = "<closure>"

type Eval = SymT Exp QM Value

eval :: Exp -> Eval
eval = ev 0
    where
        ev :: Integer -> Exp -> Eval
        ev i = \case
            Abs _ e -> throwError NotValueType

-- evl :: [Value] -> Exp -> ExceptT Error QM Value
evl :: [Value] -> Exp -> Except Error Value
evl vs = \case
    Bit b -> return $ VBit b

    Tup bs -> VTup <$> mapM (evl vs) bs

    Idx j -> return $ vs !! fromInteger j

    Abs _ e -> return $ VFunc vs e

    App e1 e2 -> evl vs e1 >>= \case
        VFunc e a -> do
            v <- evl vs e2
            evl (v : vs) a

    _ -> throwError $ Fail "not implemented"


runE :: String -> Either Error (Exp, Type, Value)
runE prog = do
    e <- parseExpr prog
    t <- runCheck (typecheck e)
    v <- runExcept (evl [] e)
    return (e,t,v)

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

variable :: Parser Exp
variable = Var <$> identifier

number :: Parser Exp
number = Idx <$> natural

lambda :: Parser Exp
lambda = do
  reservedOp "\\"
  t <- type'
  reservedOp "."
  e <- expr
  return (Abs t e)

bit :: Parser Exp
bit =  (reservedOp "b0" >> return (Bit 0))
   <|> (reservedOp "b1" >> return (Bit 1))

gate :: Parser Exp
gate =  (reservedOp "H" >> return (Gate H))
    <|> (reservedOp "X" >> return (Gate X))

op :: Parser Exp
op =  (reservedOp "new" >> return New)
  <|> (reservedOp "measure" >> return Meas)

term :: Parser Exp
term =  parens expr
    <|> number
    <|> bit
    <|> gate
    <|> op
    <|> variable
    <|> lambda

expr :: Parser Exp
expr = do
  es <- many1 term
  return (foldl1 App es)

tyatom :: Parser Type
tyatom = tylit <|> parens type'

tylit :: Parser Type
tylit =  (reservedOp "Bit" >> return TypeBit) 
     <|> (reservedOp "QBit" >> return TypeQBit)

type' :: Parser Type
type' = Ex.buildExpressionParser tyops tyatom
  where
    infixOp x f = Ex.Infix (reservedOp x >> return f)
    tyops = [
        [infixOp "-o" (:=>) Ex.AssocRight],
        [infixOp "><" (:><) Ex.AssocRight ]
      ]

parseExpr :: String -> Either Error Exp
parseExpr input = case parse (contents expr) "<stdin>" input of
    Left  err -> Left . ParseError . show $ err
    Right exp -> Right exp

run :: String -> IO ()
run prg = case runE prg of
        Left  err     -> putStrLn $ "*** Exception:\n" ++ show err
        Right (e,t,v) -> putStrLn $ "foo" ++ " : " ++ show t ++ "\n"
                               ++ "foo" ++ " = " ++ show e ++ "\n\n"
                               ++ "Result: " ++ show v

run1 = "\\Bit . new b0" -- Abs TypeBit (App New (Bit 0))
run2 = "\\Bit . (\\Bit . 1) b0" -- Abs TypeBit (App (Abs TypeBit (Idx 1)) (Bit 0))
run3 = "(b0, new b1, \\Bit . 0)" -- Tup [Bit 0, App New (Bit 1), Abs TypeBit (Idx 0)]
run4 = "measure (H (new b0))" -- App Meas (App (Gate H) (App New (Bit 0)))
run5 = "(\\Bit . (0,0)) b0"