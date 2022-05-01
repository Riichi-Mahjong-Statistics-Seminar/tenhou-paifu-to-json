import Text.Regex.PCRE
import Data.List
import System.IO 
import qualified Data.Map as Map

numToCol :: Int -> String
numToCol n | (n `div` 4) <  9 = "m"
           | (n `div` 4) < 18 = "p"
           | (n `div` 4) < 27 = "s"
           | (n `div` 4) < 36 = "z"
numToID  :: Int -> Int
numToID n = (((n `div` 4) `mod` 9) + 1)
numToHai :: Int -> String
numToHai n | n == 16 = "0m" 
           | n == 52 = "0p" 
           | n == 88 = "0s" 
           | otherwise = show(numToID(n)) ++ numToCol(n)

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

act_RYUUKYOKU :: String -> (Int, String)
act_RYUUKYOKU str = (1003, "{\"type\":\"ryuukyoku\"}")

act_DORA :: String -> (Int, String)
act_DORA str = (1002, "{\"dora_marker\":\"" ++ numToHai (findXMLtoInt str "hai") ++ "\",\"type\":\"dora\"}")

act_TSUMOGIRI :: Int -> Int -> Int -> Bool
act_TSUMOGIRI now lst1 lst2 = lstnum == now where
    lstnum = (if lst > 0 then -1 else -lst) where
        lst = (if (lst1 == 1004 || lst1 == 1002) then lst2 else lst1)

get_actor :: Char -> Int
get_actor 'D' = 0
get_actor 'E' = 1
get_actor 'F' = 2
get_actor 'G' = 3
get_actor 'T' = 0
get_actor 'U' = 1
get_actor 'V' = 2
get_actor 'W' = 3

act_DAHAI :: String -> Int -> Int -> (Int, String)
act_DAHAI str lst1 lst2= (numHai, outstr) where
    numHai = read (num) :: Int where
        num = tail str
    outstr = "{\"actor\":" ++ actor ++ ",\"pai\":\"" ++ hai ++ "\",\"type\":\"dahai\",\"tsumogiri\":\"" ++ tsumogiri ++ "\"}" where
        actor = show (get_actor (head str)) :: String
        hai = numToHai (numHai)
        tsumogiri = show (act_TSUMOGIRI numHai lst1 lst2) :: String

act_TSUMO :: String -> (Int, String)
act_TSUMO str = (-numHai, outstr) where
    numHai = read (num) :: Int where
        num = tail str
    outstr = "{\"actor\":" ++ actor ++ ",\"pai\":\"" ++ hai ++ "\",\"type\":\"tsumo\"}" where
        actor = show (get_actor (head str)) :: String
        hai = numToHai (numHai)

act_REACH :: String -> (Int, String)
act_REACH str = (1004, "{\"actor\":" ++ who ++ ",\"type\":\"" ++ typ ++ "\"}") where
    who = findXML str "who"
    typ = (if typnum == 1 then "reach" else "reach_accepted") where
        typnum = findXMLtoInt str "step"

act_AGARI :: String -> (Int, String)
act_AGARI str = (1005, "{\"actor\":" ++ actor ++ ",\"fromwho\":" ++ fromwho ++ ",\"type\":\"agari\"}") where
    actor = findXML str "who"
    fromwho = findXML str "fromWho"

act_ALL :: String -> [(Int, String)] -> [(Int, String)]
act_ALL str tmp = tmp ++ [ret] where
    ret | (tag == "D" || tag == "E" || tag == "F" || tag == "G") = act_DAHAI str lst1 lst2
        | (tag == "T" || tag == "U" || tag == "V" || tag == "W") = act_TSUMO str
        | (tag == "DORA") = act_DORA str
        | (tag == "REACH") = act_REACH str
        | (tag == "AGARI") = act_AGARI str
        | otherwise = (-114514, "")
        where 
            tag = get_tag str
            lst1 = fst (last tmp)
            lst2 = fst (last (init tmp))

do_ALL :: [String] -> [(Int, String)] -> [(Int, String)]
do_ALL [] tmp = tmp
do_ALL (x : xs) tmp = act_ALL x (do_ALL xs tmp)

main = do
    print (map numToHai [0..135])
    str <- getLine
    print (do_ALL (make_all(get_all str)) [])