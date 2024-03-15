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
jsonNumber = JsonNumber . read <$> some (satisfyP (isDigit))

-- NOTE: does not support escaping
stringLiteral :: Parser String
stringLiteral = charP '"' *> many (satisfyP (/= '"')) <* charP '"'

jsonString :: Parser JsonValue
jsonString = JsonString <$> stringLiteral

ws :: Parser String
ws = many (satisfyP isSpace)

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
charP c = satisfyP (== c)

satisfyP :: (Char -> Bool) -> Parser Char
satisfyP f = Parser $ \case
  y : ys | f y -> Just (ys, y)
  _ -> Nothing

stringP :: String -> Parser String
stringP xs = sequenceA (fmap charP xs)
