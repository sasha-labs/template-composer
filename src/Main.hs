module Main where

import System.Directory
import System.FilePath ((</>))
import Data.Char
import Data.List (isPrefixOf)
import System.Environment (getArgs)
import System.Exit (exitFailure)

data OperatorType = Replace | Number | Place | Import
  deriving (Show,Eq)

data Node = Quote | LineBreak | Operator OperatorType | Variable String | Value String | Undecided String | EOF | Invalid | OpenParen | CloseParen | Comma
  deriving (Show,Eq)

class NodeValue a where
  nodeValue :: a -> String
instance NodeValue Node where
  nodeValue (Variable v) = v
  nodeValue (Value a) = a
  nodeValue (Undecided s) = s
  nodeValue _ = "INVALID"

data Expression = Apply Node Node Node [Expression] | Nested [Expression]
  deriving Show

escapees = ['\"']
operators = [":=", ":_", "<-", "<="]

word :: String -> [Expression]
word = express . combine . escape

escape :: String -> [Node]
escape [] = [EOF]
escape (c:cs)
  | c == '\\' && x `elem` escapees = Undecided [x] : escape xs
  | c `elem` escapees = (findEscChar c) : escape cs
  | c == '\n' = LineBreak : escape cs
  | c == '(' = OpenParen : escape cs
  | c == ')' = CloseParen : escape cs
  | c == ',' = Comma : escape cs
  | otherwise = Undecided [c] : escape cs
  where
    (x:xs) = cs

findEscChar :: Char -> Node
findEscChar _ = Quote

combine :: [Node] -> [Node]
combine [EOF] = [EOF]
combine (Quote : ns) = combineValue ns
combine (OpenParen : ns) = OpenParen : combine ns
combine (CloseParen : ns) = CloseParen : combine ns
combine (Comma : ns) = Comma : combine ns
combine (Undecided s1 : Undecided s2 : ns)
  | s1++s2 == "--" = skipToBreak ns
  | s1++s2 == ":=" = Operator Replace : combineAfterOp ns
  | s1++s2 == ":_" = Operator Number : combineAfterOp ns
  | s1++s2 == "<-" = Operator Place : combineAfterOp ns
  | s1++s2 == "<=" = Operator Import : combineAfterOp ns
combine (LineBreak : ns) = combine ns
combine (Undecided s : ns)
  | isSpace c = combine ns
  | otherwise = combineVariable (Variable s : ns)
  where (c:_) = s

skipToBreak :: [Node] -> [Node]
skipToBreak [EOF] = [EOF]
skipToBreak (LineBreak : ns) = combine ns
skipToBreak (_:ns) = skipToBreak ns

combineAfterOp :: [Node] -> [Node]
combineAfterOp [EOF] = [EOF]
combineAfterOp (Quote : ns) = combineValue ns
combineAfterOp (LineBreak : ns) = combineAfterOp ns
combineAfterOp (OpenParen : ns) = OpenParen : combine ns
combineAfterOp (CloseParen : ns) = CloseParen : combine ns
combineAfterOp (Comma : ns) = Comma : combine ns
combineAfterOp (Undecided s : ns)
  | isSpace c = combineAfterOp ns
  | otherwise = combineUnquoted (Value s : ns)
  where (c:_) = s

combineUnquoted :: [Node] -> [Node]
combineUnquoted [Value s, EOF] = [Value s, EOF]
combineUnquoted (Value s : LineBreak : ns) = Value s : combine (LineBreak : ns)
combineUnquoted (Value s : OpenParen : ns) = Value s : OpenParen : combine ns
combineUnquoted (Value s : CloseParen : ns) = Value s : CloseParen : combine ns
combineUnquoted (Value s : Comma : ns) = Value s : Comma : combine ns
combineUnquoted (Value s : Quote : ns) = Value s : combine (Quote : ns)
combineUnquoted (Value s1 : Undecided s2 : ns)
  | isSpace c = Value s1 : combine ns
  | otherwise = combineUnquoted (Value (s1++s2) : ns)
  where (c:_) = s2

combineValue :: [Node] -> [Node]
combineValue [EOF] = [Invalid, EOF]
combineValue [Value s, EOF] = [Invalid, EOF]
combineValue (Quote : ns) = Value "" : combine ns
combineValue (Undecided s : ns) = combineValue (Value s : ns)
combineValue (Value s : Quote : ns) = Value s : combine ns
combineValue (Value s : LineBreak : ns) = combineValue (Value (s++"\n") : ns)
combineValue (Value s1 : Undecided s2 : ns) = combineValue (Value (s1++s2) : ns)

combineVariable :: [Node] -> [Node]
combineVariable [Variable s, EOF] = [Invalid, EOF]
combineVariable (Variable s1 : Undecided s2 : Undecided s3 : ns)
  | s2++s3 `elem` operators = Variable s1 : combine (Undecided s2 : Undecided s3 : ns)
  | otherwise = combineVariable (Variable (s1++s2) : Undecided s3 : ns)
combineVariable (Variable s1 : Undecided s2 : ns) = combineVariable (Variable (s1++s2) : ns)
combineVariable (Variable s : _ : ns) = Invalid : combine ns

express :: [Node] -> [Expression]
express [EOF] = []
express (Variable v : Operator o : Value a : OpenParen : ns) =
  let (params, rest) = expressParams ns
  in Apply (Variable v) (Operator o) (Value a) params : express rest
express (Variable v : Operator o : Value a : ns) = Apply (Variable v) (Operator o) (Value a) [] : express ns
express (n:ns) = express ns

expressParams :: [Node] -> ([Expression], [Node])
expressParams (CloseParen : ns) = ([], ns)
expressParams (Comma : ns) = expressParams ns
expressParams (Variable v : Operator o : Value a : OpenParen : ns) =
  let (innerParams, rest1) = expressParams ns
      expr = Apply (Variable v) (Operator o) (Value a) innerParams
      (moreParams, rest2) = expressParams rest1
  in (expr : moreParams, rest2)
expressParams (Variable v : Operator o : Value a : ns) =
  let expr = Apply (Variable v) (Operator o) (Value a) []
      (moreParams, rest) = expressParams ns
  in (expr : moreParams, rest)
expressParams ns = ([], ns)

fileErrorString :: String -> String
fileErrorString s = "FILE " ++ s ++ " NOT FOUND"

importFile :: Expression -> IO (Expression)
importFile (Apply v o a params) = do
  if o == Operator Place then do
    putStrLn $ "  Placing file: " ++ nodeValue a
    fe <- doesFileExist (nodeValue a)
    processedParams <- performImports params
    if fe then do
      b <- readFile (nodeValue a)
      let result = performOps processedParams b
      return (Apply v (Operator Replace) (Value result) [])
    else do
      putStrLn $ "  WARNING: File not found: " ++ nodeValue a
      return (Apply v (Operator Replace) (Value (fileErrorString (nodeValue a))) [])
  else
    return (Apply v o a params)
importFile e = return e

importNest :: Expression -> IO (Expression)
importNest (Apply v o a params) = do
  if o == Operator Import then do
    cwd <- getCurrentDirectory
    let path = cwd </> nodeValue a
    putStrLn $ "  Importing: " ++ path
    fe <- doesFileExist path
    if fe then do
      b <- readFile path
      bwImports <- performImports (word b)
      return (Nested bwImports)
    else do
      putStrLn $ "  WARNING: File not found: " ++ path
      return (Apply v (Operator Replace) (Value (fileErrorString (nodeValue a))) [])
  else
    return (Apply v o a params)
importNest e = return e

performImports :: [Expression] -> IO ([Expression])
performImports e = do
  ewfiles <- mapM importFile e
  ewNests <- mapM importNest ewfiles
  return ewNests

replace :: String -> String -> String -> String
replace _ _ "" = ""
replace pattern word (c:cs)
  | pattern `isPrefixOf` (c:cs) = word ++ (replace pattern word (drop (length pattern) (c:cs)))
  | otherwise = c : replace pattern word cs

number :: String -> String -> String -> String
number = numberInt 1

numberInt :: Int -> String -> String -> String -> String
numberInt _ _ _ "" = ""
numberInt a pattern word (c:cs)
  | pattern `isPrefixOf` (c:cs) = (word++(show a)) ++ (numberInt (a+1) pattern word (drop (length pattern) (c:cs)))
  | otherwise = c : numberInt a pattern word cs

performOp :: Expression -> String -> String
performOp (Nested es) s = performOps es s
performOp (Apply v o a _) s
  | o == Operator Replace = replace (nodeValue v) (nodeValue a) s
  | o == Operator Number = number (nodeValue v) (nodeValue a) s
  | otherwise = s

performOps :: [Expression] -> String -> String
performOps [] s = s
performOps (e:es) s = performOps es (performOp e s)

parseArgs :: [String] -> (Maybe String, Maybe String, Maybe String)
parseArgs [] = (Nothing, Nothing, Nothing)
parseArgs (a:as)
  | "--source=" `isPrefixOf` a = let (m, _, t) = parseArgs as in (m, Just (drop 9 a), t)
  | "--target=" `isPrefixOf` a = let (m, s, _) = parseArgs as in (m, s, Just (drop 9 a))
  | otherwise                  = let (_, s, t) = parseArgs as in (Just a, s, t)

main = do
  args <- getArgs
  let (mTmc, mSource, mTarget) = parseArgs args
  case (mTmc, mSource, mTarget) of
    (Just tmc, Just source, Just target) -> do
      putStrLn $ "Reading instructions: " ++ tmc
      mFile <- readFile tmc
      putStrLn $ "Reading source: " ++ source
      tFile <- readFile source
      putStrLn "Processing imports..."
      exlist <- performImports (word mFile)
      putStrLn $ "Applying operations..."
      writeFile target (performOps exlist tFile)
      putStrLn $ "Written to: " ++ target
      putStrLn "Done."
    _ -> do
      putStrLn "Usage: tmc <file.tmc> --source=<source> --target=<target>"
      exitFailure