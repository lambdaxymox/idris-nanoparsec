module NanoParsec.Combinators

import NanoParsec.Parser
import Data.String

%access export

oneOf : List Char -> Parser Char
oneOf st = satisfy (flip elem st)

chainl1 : Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p op = do { a <- p; rest a }
    where
        rest a = (do f <- op
                     b <- p
                     rest (f a b))
                <|> return a

chainl : Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainl p op a = (chainl1 p op) <|> return a


char : Char -> Parser Char
char ch = satisfy (ch ==)

digit : Parser Char
digit = satisfy isDigit

spaces : Parser String
spaces = pack <$> (many . oneOf . unpack $ " \n\r")

string : String -> Parser String
string st = pack <$> (string' $ unpack st)
    where
        string' : List Char -> Parser (List Char)
        string' [] = return []
        string' (c :: cs) = char c
                          >>= \_ => string' cs
                          >>= \_ => return (c :: cs)

token : Parser a -> Parser a
token p = p >>= \a => spaces >>= \_ => return a

reserved : String -> Parser String
reserved st = token (string st)

maybeNatural : Parser (Maybe Integer)
maybeNatural = parseInteger <$> (pack <$> some digit)

natural : Parser Integer
natural = fromMaybe <$> maybeNatural

maybeNumber : Parser (Maybe Int)
maybeNumber = do
    sign   <- (unpack <$> string "-") <|> return []
    digits <- some digit
    return $ parseInteger $ pack (sign ++ digits)

number : Parser Int
number = fromMaybe <$> maybeNumber

parens : Parser a -> Parser a
parens p = do
    reserved "("
    val <- p
    reserved ")"
    return val
