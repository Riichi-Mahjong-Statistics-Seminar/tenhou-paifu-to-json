module Jsondata ( 
   JValue, 
   jsonPrettyShow 
) where 

import Data.List

data JValue = JNum Double
            | JInt Int
            | JStr String
            | JBol Bool
            | JNul
            | JArr [JValue]
            | JObj [(String, JValue)]
              deriving (Ord, Eq)

instance Show JValue where
    show = jsonPrettyShow

jsonPrettyShow :: JValue -> String
jsonPrettyShow = prettyJson 0 where
    prettyJson level (JStr s) = (show s)
    prettyJson level (JNum d) = (show d)
    prettyJson level (JInt i) = (show i)
    prettyJson level (JBol b) | b = "true" | not b = "false"
    prettyJson level JNul = "null"
    prettyJson level (JArr []) = "[]"
    prettyJson level (JArr vs) = "[" ++ prettyList level prettyJson vs ++ "]"
    prettyJson level (JObj []) = "{}"
    prettyJson level (JObj ps) = "{" ++ prettyList level prettyPair ps ++ "}"

    prettyList level pretty xs = prettyList' xs ++ "\n" ++ replicate level ' '
        where prettyList' = intercalate "," . map ((indent++) . (pretty level'))
              indent = "\n" ++ replicate level' ' '
              level' = level + 4

    prettyPair level (key, val) = show key ++ ": " ++ prettyJson level val
