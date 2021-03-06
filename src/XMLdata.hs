module XMLdata ( 
    findXMLtoIntList, 
    findXMLtoDoubleList,
    findXMLtoInt,
    findXML,
    findXMLMaybe,
    findXMLMaybetoInt,
    findXMLMaybetoIntList,
    get_all,
    make_all,
    get_tag
) where 

import Text.Regex.PCRE
import Data.List
import Data.Maybe

findXMLtoIntList :: String -> [Int]
findXMLtoIntList str = map (read) (map (!!0) (str =~ ("[0-9-]+") :: [[String]]))

findXMLtoDoubleList :: String -> [Double]
findXMLtoDoubleList str = map (read) (map (!!0) (str =~ ("[0-9.-]+") :: [[String]]))

findXMLtoInt :: String -> String -> Int
findXMLtoInt str pattern = read (findXML str pattern) :: Int

findXML :: String -> String -> String
findXML str pattern = head (map (!!1) (str =~ (pattern ++ "=\"(.*?)\"") :: [[String]]))

findXMLMaybe :: String -> String -> Maybe String
findXMLMaybe str pattern | (null res) == True = Nothing
                         | otherwise          = Just (head res)
                         where res = map (!!1) (str =~ (pattern ++ "=\"(.*?)\"") :: [[String]])

findXMLMaybetoInt :: String -> String -> Maybe Int
findXMLMaybetoInt str pattern | res == Nothing = Nothing
                              | otherwise      = Just (read jres :: Int)
                              where res  = findXMLMaybe str pattern 
                                    jres = fromJust res

findXMLMaybetoIntList :: Maybe String -> Maybe [Int]
findXMLMaybetoIntList (Just str) = Just (findXMLtoIntList str)
findXMLMaybetoIntList Nothing  = Nothing

get_all :: String -> [[String]]
get_all str = (str =~ "<(.*?)/>" :: [[String]])

make_all :: [[String]] -> [String]
make_all str = map (!!1) (drop 1 str)

get_tag :: String -> String
get_tag str = head ( map (!!0) (str =~ "[A-Z]*" :: [[String]]))
