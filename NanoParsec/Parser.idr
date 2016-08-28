module NanoParsec.Parser

import NanoParsec.MonadPlus

%access export

record Parser a where
    constructor MkParser
    parse : String -> List (a, String)

runParser : Parser a -> String -> Maybe a
runParser p st =
    case parse p st of
        [(res, "")]  => Just res
        [(_, remSt)] => Nothing
        _            => Nothing

item : Parser Char
item = MkParser $ \st =>
    case st of
        "" => []
        _  => [(strHead st, strTail st)]

implementation Functor Parser where
    map f (MkParser cs) = MkParser (\st => [(f a, b) | (a, b) <- cs st])

implementation Applicative Parser where
    pure a = MkParser (\st => [(a, st)])
    (MkParser cs1) <*> (MkParser cs2) =
        MkParser (\st => [(f a, st2) | (f, st1) <- cs1 st, (a, st2) <- cs2 st1])

implementation Monad Parser where
    p >>= f = MkParser $ \st => concatMap (\(a, st') => parse (f a) st') $ parse p st

combine : Parser a -> Parser a -> Parser a
combine p1 p2 = MkParser (\st => parse p1 st ++ parse p2 st)

failure : Parser a
failure = MkParser (\cs => [])

option : Parser a -> Parser a -> Parser a
option p1 p2 = MkParser $ \st =>
    case parse p1 st of
        []  => parse p2 st
        res => res

satisfy : (Char -> Bool) -> Parser Char
satisfy pred = item >>= \ch => if (pred ch) then (return ch) else failure

implementation MonadPlus Parser where
    mzero = failure
    mplus = option

implementation Alternative Parser where
    empty = failure
    (<|>) = option

some : (Alternative f, Applicative f) => f a -> f (List a)
some v = some_v
    where
        mutual
            some_v : f (List a)
            some_v = (map (::) v) <*> many_v

            many_v : f (List a)
            many_v = some_v <|> pure []


many : (Alternative f, Applicative f) => f a -> f (List a)
many v = many_v
    where
        mutual
            many_v : f (List a)
            many_v = some_v <|> pure []

            some_v : f (List a)
            some_v = (map (::) v) <*> many_v
