module Data.Json.Types where

import Control.Applicative

data JsonValue
  = JsonNull
  | JsonBool Bool
  | JsonNumber Integer -- NOTE: no support for floats
  | JsonString String
  | JsonArray [JsonValue]
  | JsonObject [(String, JsonValue)]
  deriving (Show, Eq)

-- NOTE: no proper error reporting
newtype Parser a = Parser
  { runParser :: String -> Maybe (String, a)
  }

instance Functor Parser where
  fmap f (Parser p) = Parser $ (fmap . fmap . fmap) f p

instance Applicative Parser where
  pure a = Parser $ \input -> Just (input, a)
  Parser pf <*> Parser pa = Parser $ \input -> do
    (input', f) <- pf input
    (input'', a) <- pa input'
    return (input'', f a)

instance Alternative Parser where
  empty = Parser $ const Nothing
  Parser p1 <|> Parser p2 = Parser $ \input -> p1 input <|> p2 input
