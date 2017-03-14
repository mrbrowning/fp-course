{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}

module Course.JsonParser where

import Course.Core
import Course.Parser
import Course.MoreParser
import Course.JsonValue
import Course.Functor
import Course.Applicative
import Course.Monad
import Course.List
import Course.Optional

-- $setup
-- >>> :set -XOverloadedStrings

-- A special character is one of the following:
-- * \b  Backspace (ascii code 08)
-- * \f  Form feed (ascii code 0C)
-- * \n  New line
-- * \r  Carriage return
-- * \t  Tab
-- * \v  Vertical tab
-- * \'  Apostrophe or single quote (only valid in single quoted json strings)
-- * \"  Double quote (only valid in double quoted json strings)
-- * \\  Backslash character
data SpecialCharacter =
  BackSpace
  | FormFeed
  | NewLine
  | CarriageReturn
  | Tab
  | VerticalTab
  | SingleQuote
  | DoubleQuote
  | Backslash
  deriving (Eq, Ord, Show)

-- NOTE: This is not inverse to @toSpecialCharacter@.
fromSpecialCharacter :: SpecialCharacter -> Char
fromSpecialCharacter BackSpace = chr 0x08
fromSpecialCharacter FormFeed = chr 0x0C
fromSpecialCharacter NewLine = '\n'
fromSpecialCharacter CarriageReturn = '\r'
fromSpecialCharacter Tab = '\t'
fromSpecialCharacter VerticalTab = '\v'
fromSpecialCharacter SingleQuote = '\''
fromSpecialCharacter DoubleQuote = '"'
fromSpecialCharacter Backslash = '\\'

-- NOTE: This is not inverse to @fromSpecialCharacter@.
toSpecialCharacter :: Char -> Optional SpecialCharacter
toSpecialCharacter c =
  let table = ('b', BackSpace) :.
              ('f', FormFeed) :.
              ('n', NewLine) :.
              ('r', CarriageReturn) :.
              ('t', Tab) :.
              ('v', VerticalTab) :.
              ('\'', SingleQuote) :.
              ('"' , DoubleQuote) :.
              ('\\', Backslash) :.
              Nil
  in snd <$> find ((==) c . fst) table

specialCharMapping :: List (Char, Chars)
specialCharMapping =
    ('\b', "\\b") :.
    ('\f', "\\f") :.
    ('\n', "\\n") :.
    ('\r', "\\r") :.
    ('\t', "\\t") :.
    ('\v', "\\v") :.
    ('\\', "\\\\") :.
    Nil

toSpecialCharString :: Char -> Chars
toSpecialCharString c =
    let val = snd <$> find ((==) c . fst) specialCharMapping
    in case val of
        Empty -> (\x -> x :. Nil) c
        Full s -> s

fromSpecialCharString :: Chars -> Char
fromSpecialCharString s =
    let val = fst <$> find ((==) s . snd) specialCharMapping
    in case val of
        Empty -> error "welp"
        Full c -> c
  
-- | Parse a JSON string. Handle double-quotes, special characters, hexadecimal characters. See http://json.org for the full list of control characters in JSON.
--
-- /Tip:/ Use `hex`, `fromSpecialCharacter`, `between`, `is`, `charTok`, `toSpecialCharacter`.
--
-- >>> parse jsonString "\" abc\""
-- Result >< " abc"
--
-- >>> parse jsonString "\"abc\"def"
-- Result >def< "abc"
--
-- >>> parse jsonString "\"\\babc\"def"
-- Result >def< "\babc"
--
-- >>> parse jsonString "\"\\u00abc\"def"
-- Result >def< "\171c"
--
-- >>> parse jsonString "\"\\u00ffabc\"def"
-- Result >def< "\255abc"
--
-- >>> parse jsonString "\"\\u00faabc\"def"
-- Result >def< "\250abc"
--
-- >>> isErrorResult (parse jsonString "abc")
-- True
--
-- >>> isErrorResult (parse jsonString "\"\\abc\"def")
-- True
jsonString :: Parser Chars
jsonString = between
    (is '"')
    (is '"')
    ((foldRight (++) Nil) <$>
        (list $
            ((\x -> x :. Nil) <$> hexParser) |||
            ((\x -> x :. Nil) <$> specialCharParser) |||
        ((\x -> x :. Nil) <$> satisfyAll (((/=) '"') :. ((/=) '\\') :. Nil))))

isNot :: Char -> Parser Char
isNot c = satisfy $ not . (==c)

specialCharParser :: Parser Char
specialCharParser =
    fromSpecialCharString <$>
    (foldRight (|||) failed $ (map string) . (map toSpecialCharString) $ specialCharacters)

specialCharacters :: List Char
specialCharacters = (chr 0x08 :. chr 0x0C :. '\n' :. '\r' :. '\t' :. '\v' :. '\'' :. '\\' :. Nil)

hexParser :: Parser Char
hexParser = string "\\u" *> hex

-- | Parse a JSON rational.
--
-- /Tip:/ Use @readFloats@.
--
-- >>> parse jsonNumber "234"
-- Result >< 234 % 1
--
-- >>> parse jsonNumber "-234"
-- Result >< (-234) % 1
--
-- >>> parse jsonNumber "123.45"
-- Result >< 2469 % 20
--
-- >>> parse jsonNumber "-123"
-- Result >< (-123) % 1
--
-- >>> parse jsonNumber "-123.45"
-- Result >< (-2469) % 20
--
-- >>> isErrorResult (parse jsonNumber "-")
-- True
--
-- >>> isErrorResult (parse jsonNumber "abc")
-- True
jsonNumber :: Parser Rational
jsonNumber = f =<< ((++) <$> oneOrZero "-" <*> ((++) <$> digits1 <*> (((++) <$> string "." <*> digits1) ||| valueParser "")))
    where
        f :: Chars -> Parser Rational
        f s =
            let r = readFloats s
            in case r of
                Empty -> failed
                Full (x, _) -> valueParser x

oneOrZero :: Chars -> Parser Chars
oneOrZero s = string s ||| valueParser ""

-- | Parse a JSON true literal.
--
-- /Tip:/ Use `stringTok`.
--
-- >>> parse jsonTrue "true"
-- Result >< "true"
--
-- >>> isErrorResult (parse jsonTrue "TRUE")
-- True
jsonTrue :: Parser Chars
jsonTrue = stringTok "true"

-- | Parse a JSON false literal.
--
-- /Tip:/ Use `stringTok`.
--
-- >>> parse jsonFalse "false"
-- Result >< "false"
--
-- >>> isErrorResult (parse jsonFalse "FALSE")
-- True
jsonFalse :: Parser Chars
jsonFalse = stringTok "false"

-- | Parse a JSON null literal.
--
-- /Tip:/ Use `stringTok`.
--
-- >>> parse jsonNull "null"
-- Result >< "null"
--
-- >>> isErrorResult (parse jsonNull "NULL")
-- True
jsonNull :: Parser Chars
jsonNull = stringTok "null"

-- | Parse a JSON array.
--
-- /Tip:/ Use `betweenSepbyComma` and `jsonValue`.
--
-- >>> parse jsonArray "[]"
-- Result >< []
--
-- >>> parse jsonArray "[true]"
-- Result >< [JsonTrue]
--
-- >>> parse jsonArray "[true, \"abc\"]"
-- Result >< [JsonTrue,JsonString "abc"]
--
-- >>> parse jsonArray "[true, \"abc\", []]"
-- Result >< [JsonTrue,JsonString "abc",JsonArray []]
--
-- >>> parse jsonArray "[true, \"abc\", [false]]"
-- Result >< [JsonTrue,JsonString "abc",JsonArray [JsonFalse]]
jsonArray :: Parser (List JsonValue)
jsonArray = betweenSepbyComma '[' ']' jsonValue

-- | Parse a JSON object.
--
-- /Tip:/ Use `jsonString`, `charTok`, `betweenSepbyComma` and `jsonValue`.
--
-- >>> parse jsonObject "{}"
-- Result >< []
--
-- >>> parse jsonObject "{ \"key1\" : true }"
-- Result >< [("key1",JsonTrue)]
--
-- >>> parse jsonObject "{ \"key1\" : true , \"key2\" : false }"
-- Result >< [("key1",JsonTrue),("key2",JsonFalse)]
--
-- >>> parse jsonObject "{ \"key1\" : true , \"key2\" : false } xyz"
-- Result >xyz< [("key1",JsonTrue),("key2",JsonFalse)]
jsonObject :: Parser Assoc
jsonObject = betweenSepbyComma '{' '}' jsonAssoc

jsonAssoc :: Parser (Chars, JsonValue)
jsonAssoc = (,) <$> ((flip strip $ "\"") <$> jsonString) <* spaces <* charTok ':' <*> jsonValue

strip :: Chars -> Chars -> Chars
strip Nil _ = Nil
strip s Nil = s
strip (c :. cs) xs =
        let s = f cs Nil
        in if elem c xs then s else c :. s
    where
        f Nil acc = reverse acc
        f (c' :. Nil) acc = if elem c' xs then f Nil acc else f Nil (c' :. acc)
        f (c' :. cs') acc = f cs' (c' :. acc)


-- | Parse a JSON value.
--
-- /Tip:/ Use `spaces`, `jsonNull`, `jsonTrue`, `jsonFalse`, `jsonArray`, `jsonString`, `jsonObject` and `jsonNumber`.
--
-- >>> parse jsonValue "true"
-- Result >< JsonTrue
--
-- >>> parse jsonObject "{ \"key1\" : true , \"key2\" : [7, false] }"
-- Result >< [("key1",JsonTrue),("key2",JsonArray [JsonRational False (7 % 1),JsonFalse])]
--
-- >>> parse jsonObject "{ \"key1\" : true , \"key2\" : [7, false] , \"key3\" : { \"key4\" : null } }"
-- Result >< [("key1",JsonTrue),("key2",JsonArray [JsonRational False (7 % 1),JsonFalse]),("key3",JsonObject [("key4",JsonNull)])]
jsonValue :: Parser JsonValue
jsonValue =
    spaces *>
    (((\_ -> JsonTrue) <$> jsonTrue) |||
    ((\_ -> JsonFalse) <$> jsonFalse) |||
    ((\_ -> JsonNull) <$> jsonNull) |||
    (JsonString <$> jsonString) |||
    (JsonRational True <$> jsonNumber) |||
    (JsonArray <$> jsonArray) |||
    (JsonObject <$> jsonObject)) <*
    spaces

-- | Read a file into a JSON value.
--
-- /Tip:/ Use @System.IO#readFile@ and `jsonValue`.
readJsonValue :: Filename -> IO (ParseResult JsonValue)
readJsonValue f = (pure . parse jsonValue) =<< readFile f
