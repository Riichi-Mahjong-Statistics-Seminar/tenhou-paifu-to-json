import Text.Regex.PCRE
import Data.List
import System.IO 
import qualified Data.Map as Map

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

isTsumogiri :: Int -> (Int, Int) -> Bool
isTsumogiri now (lst1, lst2) = lstnum == now where
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

act_RYUUKYOKU :: String -> JValue
act_RYUUKYOKU str = JObj obj where
    obj = [("type", JStr "ryuukyoku")]

act_DORA :: String -> JValue
act_DORA str = JObj obj where
    obj = 
        [
            ("dora_marker", JStr hai), 
            ("type",        JStr "dora")
        ] where
            hai = numToHai (findXMLtoInt str "hai")      

act_DAHAI :: String -> (Int, Int) -> JValue
act_DAHAI str lst = JObj obj where
    obj = 
        [
            ("actor",     JInt actor),
            ("pai",       JStr hai),
            ("type",      JStr "dahai"),
            ("tsumogiri", JBol tsumogiri)
        ] where
            actor     = get_actor (head str)
            hai       = numToHai num
            num       = read (tail str) :: Int
            tsumogiri = isTsumogiri num lst

act_TSUMO :: String -> JValue
act_TSUMO str = JObj obj where
    obj =
        [
            ("actor", JInt actor), 
            ("pai",   JStr hai), 
            ("type",  JStr "tsumo")
        ] where
            actor = get_actor (head str)
            hai   = numToHai (read (num) :: Int)
            num   = tail str

act_REACH :: String -> JValue
act_REACH str = JObj obj where
    obj =
        [
            ("actor", JInt actor),
            ("type",  JStr typ)
        ] where
            actor = findXMLtoInt str "who"
            typ   = (if typnum == 1 then "reach" else "reach_accepted") where
                typnum = findXMLtoInt str "step"

act_AGARI :: String -> JValue
act_AGARI str = JObj obj where
    obj =
        [
            ("actor",   JInt actor),
            ("fromwho", JInt fromwho),
            ("type",    JStr "agari")
        ] where
            actor   = findXMLtoInt str "who"
            fromwho = findXMLtoInt str "fromWho"

act_INIT :: String -> JValue
act_INIT str = JObj obj where
    obj =
        [
            ("bakaze",      JStr bakaze),
            ("dora_marker", JStr doraMarker),
            ("honba",       JInt honba),
            ("kyoku",       JInt kyoku),
            ("kyotaku",     JInt kyotaku),
            ("oya",         JInt oya),
            ("scores",      JArr jscores),
            ("tehais",      JArr jtehais),
            ("type",        JStr "start_kyoku")
        ] where
            bakaze = (if nowKyu < 4 then "E" else if nowKyu < 8 then "S" else "W")
            doraMarker = numToHai num where
                num = (!!5) seed
            honba = (!!1) seed
            kyoku = (nowKyu `mod` 4) + 1
            kyotaku = (!!2) seed
            oya = findXMLtoInt str "oya"
            jscores = map (\i -> JInt i) scores where
                scores = findXMLtoIntList newstr where -- add "00" to scores
                    newstr = addZero (findXML str "ten") where
                        addZero (',': xs) = "00" ++ "," ++ addZero xs -- add before ','
                        addZero ( x : xs) = [x] ++ (addZero xs) -- else do nothing
                        addZero   x       = x ++ "00" -- add to end
            jtehais = [ JArr (getjTehai ("hai" ++ [i])) | i <- ['0'..'3'] ] where
                getjTehai pat = map (\i -> JStr i)  (getTehai pat) where
                    getTehai pat = getHaiList (findXMLtoIntList (findXML str pat)) where
                        getHaiList hais = map numToHai hais 
            nowKyu = (!!0) seed
            seed = findXMLtoIntList seedstr where
                seedstr = findXML str "seed"
        
--INIT seed="2,0,0,0,0,12" 
-- ten="293,207,261,239"
-- oya="2" 
-- hai0="99,118,13,23,6,33,15,0,120,28,65,27,96" 
-- hai1="19,92,21,52,22,73,78,1,102,112,31,121,79"
-- hai2="105,114,122,80,123,132,83,48,93,85,82,51,103"
-- hai3="29,77,107,4,68,125,55,10,101,74,2,41,130"
act_ALL :: String -> JValue -> JValue
act_ALL str (JArr tmp) = JArr (ret ++ tmp) where
    ret | (tag == "D" || tag == "E" || tag == "F" || tag == "G") = [act_DAHAI str (0, 0)] -- TODO
        | (tag == "T" || tag == "U" || tag == "V" || tag == "W") = [act_TSUMO str]
        | (tag == "DORA") = [act_DORA str]
        | (tag == "REACH") = [act_REACH str]
        | (tag == "AGARI") = [act_AGARI str]
        | (tag == "INIT") = [act_INIT str]
        | otherwise = []
        where
            tag = get_tag str

act_GAME :: String -> JValue
act_GAME str = JObj obj where
    obj = [("type",  JInt typ),
           ("lobby", JInt lobby),
           ("dan",   JArr jdan),
           ("rate",  JArr jrate),
           ("game",  game)] where
              typ = findXMLtoInt str "type"
              lobby = findXMLtoInt str "lobby"
              dan = findXMLtoIntList (findXML str "dan")
              jdan = map (\i -> JInt i) dan
              rate = findXMLtoDoubleList (findXML str "rate")
              jrate = map (\i -> JNum i) rate
              game = do_ALL (make_all(get_all str)) (JArr [])

do_ALL :: [String] -> JValue -> JValue
do_ALL [] tmp = tmp
do_ALL (x : xs) tmp = act_ALL x (do_ALL xs tmp)

main = do
    str <- getLine
    putStrLn (show (act_GAME str)) where
