module NanoParsec.Parser

import NanoParsec.MonadPlus


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
satisfy pred = item >>= \ch => if (pred ch) then (pure ch) else failure
