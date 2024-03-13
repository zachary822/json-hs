{-# LANGUAGE LambdaCase #-}

module Data.Json where

import Control.Applicative
import Data.Char
import Data.Json.Types

jsonNull :: Parser JsonValue
jsonNull = stringP "null" *> pure JsonNull

jsonBool :: Parser JsonValue
jsonBool =
  (stringP "true" *> pure (JsonBool True))
    <|> (stringP "false" *> pure (JsonBool False))

jsonNumber :: Parser JsonValue
jsonNumber = JsonNumber . read <$> notNull (spanP isDigit)

-- NOTE: does not support escaping
stringLiteral :: Parser String
stringLiteral = charP '"' *> spanP (/= '"') <* charP '"'

jsonString :: Parser JsonValue
jsonString = JsonString <$> stringLiteral

ws :: Parser String
ws = spanP isSpace

sepBy :: Parser a -> Parser b -> Parser [a]
sepBy ele sep = (:) <$> ele <*> many (sep *> ele) <|> pure []

jsonArray :: Parser JsonValue
jsonArray = JsonArray <$> (charP '[' *> ws *> elements <* ws <* charP ']')
 where
  elements = jsonValue `sepBy` sep
  sep = ws *> charP ',' <* ws

jsonObject :: Parser JsonValue
jsonObject =
  charP '{'
    *> ws
    *> (JsonObject <$> (pair `sepBy` (ws *> charP ',' <* ws)))
    <* ws
    <* charP '}'
 where
  pair = (,) <$> (stringLiteral <* ws) <*> (charP ':' *> ws *> jsonValue)

jsonValue :: Parser JsonValue
jsonValue =
  jsonNull <|> jsonBool <|> jsonNumber <|> jsonString <|> jsonArray <|> jsonObject

charP :: Char -> Parser Char
charP c = Parser $ \case
  y : ys | y == c -> Just (ys, y)
  _ -> Nothing

stringP :: String -> Parser String
stringP xs = sequenceA (fmap charP xs)

spanP :: (Char -> Bool) -> Parser String
spanP f = Parser $ \input -> let (prefix, rest) = span f input in Just (rest, prefix)

notNull :: Parser [a] -> Parser [a]
notNull (Parser p) = Parser $ \input -> do
  (input', x) <- p input
  if null x then Nothing else Just (input', x)
