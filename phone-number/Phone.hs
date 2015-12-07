
module Phone (areaCode, number, prettyPrint) where
import Data.Char

badPhoneNumber = "0000000000"

number :: String -> String
number str = parseNumber $ filter isNumber str

parseNumber :: String -> String
parseNumber str
    | len < 10 || len > 11 = badPhoneNumber 
    | len == 10            = str
    | otherwise            = parseGoodNumber str 
    where len = length str

parseGoodNumber :: String -> String
parseGoodNumber ('1':rest) = rest
parseGoodNumber (_:rest)   = badPhoneNumber

areaCode :: String -> String
areaCode s = take 3 (number s)

prettyPrint :: String -> String
prettyPrint s = prettyPrintParts $ splitAt 3 (number s)

prettyPrintParts :: (String, String) -> String
prettyPrintParts (code, number) = "(" ++ code ++ ") " ++ prettyPrintNumber (splitAt 3 number)

prettyPrintNumber :: (String, String) -> String
prettyPrintNumber (number1, number2) = number1 ++ "-" ++ number2
