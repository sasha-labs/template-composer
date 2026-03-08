{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Directory
import Data.Char
import Data.List (isPrefixOf)
import Data.Configurator

data OperatorType = Replace | Number | Place | Import
  deriving (Show,Eq)

data Node = Quote | LineBreak | Operator OperatorType | Variable String | Value String | Undecided String | EOF | Invalid
  deriving (Show,Eq)

class NodeValue a where
  nodeValue :: a -> String
instance NodeValue Node where
  nodeValue (Variable v) = v
  nodeValue (Value a) = a
  nodeValue (Undecided s) = s
  nodeValue _ = "INVALID"

data Expression = Apply Node Node Node | Nested [Expression]
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
  | otherwise = Undecided [c] : escape cs
  where
    (x:xs) = cs

findEscChar :: Char -> Node
findEscChar _ = Quote

combine :: [Node] -> [Node]
combine [EOF] = [EOF]
combine (Quote : ns) = combineValue ns
combine (Undecided s1 : Undecided s2 : ns)
  | s1++s2 == "--" = skipToBreak ns
  | s1++s2 == ":=" = Operator Replace : combine ns
  | s1++s2 == ":_" = Operator Number : combine ns
  | s1++s2 == "<-" = Operator Place : combine ns
  | s1++s2 == "<=" = Operator Import : combine ns
combine (LineBreak : ns) = combine ns
combine (Undecided s : ns)
  | isSpace c = combine ns
  | otherwise = combineVariable (Variable s : ns)
  where (c:_) = s

skipToBreak :: [Node] -> [Node]
skipToBreak [EOF] = [EOF]
skipToBreak (LineBreak : ns) = combine ns
skipToBreak (_:ns) = skipToBreak ns

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
express (Variable v : Operator o : Value a : ns) = Apply (Variable v) (Operator o) (Value a) : express ns
express (n:ns) = express ns

fileErrorString :: String -> String
fileErrorString s = "FILE " ++ s ++ " NOT FOUND"

importFile :: Expression -> IO (Expression)
importFile (Apply v o a) = do
  if o == Operator Place then do
    fe <- doesFileExist (nodeValue a)
    if fe then do
      b <- readFile (nodeValue a)
      return (Apply v (Operator Replace) (Value b))
    else
      return (Apply v (Operator Replace) (Value (fileErrorString (nodeValue a))))
  else
    return (Apply v o a)

importNest :: Expression -> IO (Expression)
importNest (Apply v o a) = do
  if o == Operator Import then do
    fe <- doesFileExist (nodeValue a)
    if fe then do
      b <- readFile (nodeValue a)
      bwImports <- performImports (word b)
      return (Nested bwImports)
    else
      return (Apply v (Operator Replace) (Value (fileErrorString (nodeValue a))))
  else
    return (Apply v o a)

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
performOp (Apply v o a) s
  | o == Operator Replace = replace (nodeValue v) (nodeValue a) s
  | o == Operator Number = number (nodeValue v) (nodeValue a) s
  | otherwise = s

performOps :: [Expression] -> String -> String
performOps [] s = s
performOps (e:es) s = performOps es (performOp e s)

main = do
  cfg <- load [Required "./job.cfg"]
  mPath <- require cfg "filepath"   :: IO String
  tPath <- require cfg "targetpath" :: IO String
  oPath <- require cfg "outputpath" :: IO String
  mFile <- readFile mPath
  tFile <- readFile tPath
  exlist <- performImports (word mFile)
  writeFile oPath (performOps exlist tFile)