module XMLdata ( 
    findXMLtoIntList, 
    findXMLtoDoubleList,
    findXMLtoInt,
    findXML,
    get_all,
    make_all,
    get_tag
) where 

import Text.Regex.PCRE
import Data.List

findXMLtoIntList :: String -> [Int]
findXMLtoIntList str = map (read) (map (!!0) (str =~ ("[0-9]+") :: [[String]]))

findXMLtoDoubleList :: String -> [Double]
findXMLtoDoubleList str = map (read) (map (!!0) (str =~ ("[0-9.]+") :: [[String]]))

findXMLtoInt :: String -> String -> Int
findXMLtoInt str pattern = read (head (map (!!1) (str =~ (pattern ++ "=\"(.*?)\"") :: [[String]]))) :: Int

findXML :: String -> String -> String
findXML str pattern = head (map (!!1) (str =~ (pattern ++ "=\"(.*?)\"") :: [[String]]))

get_all :: String -> [[String]]
get_all str = (str =~ "<(.*?)/>" :: [[String]])

make_all :: [[String]] -> [String]
make_all str = map (!!1) (drop 1 str)

get_tag :: String -> String
get_tag str = head ( map (!!0) (str =~ "[A-Z]*" :: [[String]]))