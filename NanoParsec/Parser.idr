module NanoParsec.Parser


record Parser a where
    constructor MkParser
    parse : String -> List (a, String)

runParser : Parser a -> String -> Maybe a
runParser p st =
    case parse p st of
        [(res, "")]  => Just res
        [(_, remSt)] => Nothing
        _            => Nothing
