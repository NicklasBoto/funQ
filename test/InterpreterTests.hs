module InterpreterTests( 
    runTests,
) where
import Control.Monad.Except (runExceptT, liftIO)
import Interpreter.Main

runTests :: IO ()
runTests = do
  -- runExceptT tar en exceptT-transformerad monad och kör den, tar ur den inre monaden ur exceptT
  -- och returnar Either left right
  res <- runExceptT (foldr runTest (pure 0) tests)
  case res of
    Left err -> putStrLn $ "Interpreter test failed with error: " ++ show err
    Right correct -> do
      putStrLn $ "Succesful tests for interpreter " ++ show correct ++ "/" ++ show (length tests)

runTest :: (String, String) -> Run Int -> Run Int
runTest (fileName, expectedValue) b = do
  liftIO . print $ "Testing file " ++ show fileName
  res <- run (testPath fileName)
  acc <- b
  if show res == expectedValue
    then do
      return $ acc + 1
    else do
      liftIO $ putStrLn $ "Got " ++ show res ++ " but expected " ++ show expectedValue
      return acc

testPath :: String -> String
testPath testName = "test/interpreter-test-suite/" ++ testName

-- TODO: tågmys
-- få stateT att fungera
-- gör alla tests i en .fq-fil
-- fixa bättre show-instans för tuples (nu visas nestade tuples)
-- runTestsDebug: printa mer saker
-- kolla över errors vi får när vi build:ar/kompilerar
-- 3-qbit-gates
-- fixa syntax highlighting
tests :: [(FilePath, String)]
tests =
  [ 
    ("cnot.fq", "0"),
    ("equals.fq", "1"),
    ("higher-order-function.fq","(0,1)"),
    ("id.fq", "1"),
    ("let-tup-q.fq", "0"),
    ("let-tup.fq", "0"),
    ("partial-app-cnot.fq", "1"),
    ("partial-app-comp.fq", "0"),
    ("partial-app-new.fq",  "1"),
    ("partial-app-meas.fq", "0"),
    ("pauliX.fq", "1"),
    ("pauliY.fq", "1"),
    ("pauliZ.fq", "0"),
    ("phase.fq", "0"),
    ("plus.fq", "0"),
    ("second-q.fq", "0"),
    ("second.fq", "1"),
    ("seventh.fq", "0"),
    ("swap.fq", "1"),
    ("swapTwice.fq", "1"),
    ("third.fq", "1"),
    ("teleport.fq", "1"),
    ("nested-let.fq", "0"),
    ("deutsch.fq", "1"),
    -- ("qpe.fq", "(0,0,1)")
    -- TODO ("toffoli.fq")
    -- TODO ("fredkin.fq",  "0"),
    ("qft1.fq",     "0"),
    ("tdagger.fq",  "1")
  ]

