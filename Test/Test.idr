module Test

import NanoParsec.Parser
import NanoParsec.Combinator

test : Show a => Parser a -> String -> IO ()
test p input = case parse p input of
    Just x => printLn x
    Nothing => putStrLn $ "FAIL: " ++ input

listOf : Parser a -> Parser (List a)
listOf p = do
    string "["
    xs <- p `sepBy` (string ",")
    string "]"
    return xs

testListOf1 : IO ()
testListOf1 = test (listOf natural) "[1,2,3,4,5,666]"

testListOf2 : IO ()
testListOf2 = test (listOf natural) "This is not a list, awesome!"
