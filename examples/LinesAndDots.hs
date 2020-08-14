module Main where

import ContParser

data LexerState =
  LS !String !LineNumber
  deriving (Show)

newtype LineNumber = LineNumber Int
  deriving (Show)

incLineNumber :: LineNumber -> LineNumber
incLineNumber (LineNumber n) = LineNumber (n+1)

data Token =
    TokDot LineNumber
  | TokEOF
  deriving (Show)

getToken :: P LexerState String Token
getToken = MkP lexer
  where
    lexer cont (LS "" line) =
      cont TokEOF (LS "" line)
    lexer cont (LS ('\n' : s) line) =
      lexer cont (LS s (incLineNumber line))
    lexer cont (LS (c : s) line) =
      case c of
        '.' -> cont (TokDot line) (LS s line)
        _ -> Left "Not a dot"

getTokens :: P LexerState String [Token]
getTokens = do
  tok <- getToken
  case tok of
    TokEOF -> return []
    t -> fmap (t:) getTokens

main :: IO ()
main = do
  s <- getContents
  print $ runP getTokens (LS s (LineNumber 1))
