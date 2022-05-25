module Main where
import Data.List
import Data.List.Index
import Data.Bits
import Data.Maybe
import System.IO 

import Jsondata
import XMLdata

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
    obj =
        [
            ("reason", JStr reason),
            ("type",   JStr "ryuukyoku")
        ] where
            reason = fromMaybe "howanpai" res
            res    = findXMLMaybe str "type"

act_DORA :: String -> JValue
act_DORA str = JObj obj where
    obj =
        [
            ("dora_marker", JStr hai),
            ("type",        JStr "dora")
        ] where
            hai = numToHai (findXMLtoInt str "hai")

act_DAHAI :: String -> Int -> JValue -- the second input means what we had drew
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
            tsumogiri = lst == num

act_TSUMO :: String -> (Int, JValue) -- the first output means what to draw, we use it to judge tsumogiri
act_TSUMO str = (num, JObj obj) where
    obj =
        [
            ("actor", JInt actor),
            ("pai",   JStr hai),
            ("type",  JStr "tsumo")
        ] 
    num   = read (tail str) :: Int
    actor = get_actor (head str)
    hai   = numToHai num

act_REACH :: String -> JValue
act_REACH str = JObj obj where
    obj =
        [
            ("actor", JInt actor),
            ("type",  JStr typ)
        ]
    actor  = findXMLtoInt str "who"
    typ    = (if typnum == 1 then "reach" else "reach_accepted") where
    typnum = findXMLtoInt str "step"

act_AGARI :: String -> JValue
act_AGARI str = JObj obj where
    obj =
        [
            ("honba",       JInt honba),
            ("kyotaku",     JInt kyotaku),
            ("hai",         JArr jhai),
            ("naki",             jnaki),
            ("machi",       JStr machi),
            ("han",         JInt han),
            ("hu",          JInt hu),
            ("score",       JInt score),
            ("yaku",        JArr jyaku),
            ("dora_marker", JArr doraMarker),
            ("ura_marker",       uraMarker),
            ("actor",       JInt actor),
            ("fromwho",     JInt fromwho),
            ("paowho",           paowho),
            ("type",        JStr "agari")
        ]
    
    ba      = findXMLtoIntList (findXML str "ba")
    ten     = findXMLtoIntList (findXML str "ten")
    yaku    | findYaku == Nothing = concatMap (\i -> [i, 13]) yakuman
            | otherwise           = fromJust findYaku
            where
                findYaku = findXMLMaybetoIntList (findXMLMaybe str "yaku")
                yakuman  = findXMLtoIntList (findXML str "yakuman")

    hai     = findXMLtoIntList (findXML str "hai")
    naki    = findXMLMaybetoIntList (findXMLMaybe str "m")
    jnaki   | naki == Nothing = JNul
            | otherwise       = JArr (map (act_NAKI' actor) (fromJust naki))

    yakupr  = getPair yaku where
        getPair :: [Int] -> [(Int, Int)]
        getPair []               = []
        getPair (x1 : (x2 : xs)) = (x1, x2) : (getPair xs)
    
    honba   = (!!0) ba
    kyotaku = (!!1) ba
    jhai    = map (JStr . numToHai) hai
    machi   = numToHai (findXMLtoInt str "machi")
    han     = sum (map snd yakupr)
    hu      = (!!0) ten
    score   = (!!1) ten
    jyaku   = concat (map getJyaku yakupr) where
        getJyaku :: (Int, Int) -> [JValue]
        getJyaku (52, j) = take j (repeat (JInt 52))
        getJyaku (53, j) = take j (repeat (JInt 53))
        getJyaku (54, j) = take j (repeat (JInt 54))
        getJyaku (i , j) = [JInt i]

    doraMarker = map (JStr . numToHai) (findXMLtoIntList (findXML str "doraHai"))
    uraMarker  | doraHaiUra == Nothing = JNul 
               | otherwise             = JArr (map (JStr . numToHai) (fromJust doraHaiUra))
               where doraHaiUra = findXMLMaybetoIntList (findXMLMaybe str "doraHaiUra") 
   
    actor   = findXMLtoInt str "who"
    fromwho = findXMLtoInt str "fromWho"
    paowho  | findPao == Nothing = JNul
            | otherwise          = JInt (fromJust findPao)
            where findPao = findXMLMaybetoInt str "paoWho"

    act_NAKI' :: Int -> Int -> JValue
    act_NAKI' actor nakiRaw = delActor (act_NAKI nakistr) where
        nakistr = "m=\"" ++ show(nakiRaw) ++ "\",who=\"" ++ show(actor) ++ "\""
        delActor :: JValue -> JValue
        delActor (JObj obj) = JObj (deleteAt 0 obj)

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
        ]
    bakaze = (if nowKyu < 4 then "E" else if nowKyu < 8 then "S" else "W")
    doraMarker = numToHai ((!!5) seed)
    honba = (!!1) seed
    kyoku = (nowKyu `mod` 4) + 1
    kyotaku = (!!2) seed
    oya = findXMLtoInt str "oya"

    jscores = map JInt scores where
        scores = findXMLtoIntList newstr where -- add "00" to scores
            newstr = addZero (findXML str "ten") where
                addZero :: String -> String
                addZero (',': xs) = "00" ++ "," ++ addZero xs -- add before ','
                addZero ( x : xs) = [x] ++ (addZero xs) -- else do nothing
                addZero   x       = x ++ "00" -- add to end

    jtehais = [JArr (getjTehai ("hai" ++ [i])) | i <- ['0' .. '3']] where
        getjTehai pat = map JStr (getTehai pat) where
            getTehai pat = map numToHai (findXMLtoIntList (findXML str pat))

    nowKyu = (!!0) seed
    seed = findXMLtoIntList seedstr where
        seedstr = findXML str "seed"

act_NAKI :: String -> JValue
act_NAKI str = obj where
    obj | (nakiRaw .&.  4) /= 0 = act_CHII actor nakiRaw
        | (nakiRaw .&. 24) /= 0 = act_PON  actor nakiRaw -- also shouminkan
        | otherwise             = act_KAN  actor nakiRaw -- daiminkan or ankan
    nakiRaw = findXMLtoInt str "m"
    actor   = findXMLtoInt str "who"

    act_CHII :: Int -> Int -> JValue
    act_CHII actor nakiRaw = JObj obj' where
        obj' =
            [
                ("actor",    JInt actor),
                ("consumed", JArr consumed),
                ("pai",      JStr hai),
                ("target",   JInt target),
                ("type",     JStr "chii")
            ]
        consumed = map (JStr . numToHai) consumedNum
        hai      = numToHai consumedHai
        target   = (actor + 3) `mod` 4

        block1 = shiftR nakiRaw 10
        called = block1 `mod` 3
        base   = (block1 `div` 21) * 8 + (block1 `div` 3) * 4

        tileDetail  = map (.&. 3) [shiftR nakiRaw i | i <- [3, 5, 7]]
        consumedNum = [(tileDetail !! i) + 4 * i + base | i <- [0 .. 2], i /= called]
        consumedHai = (tileDetail !! called) + 4 * called + base

    act_PON :: Int -> Int -> JValue
    act_PON actor nakiRaw = JObj obj' where
        obj' =
            [
                ("actor",    JInt actor),
                ("consumed", JArr consumed),
                ("pai",      JStr hai),
                ("target",   JInt target),
                ("type",     JStr typ)
            ]
        consumed = map (JStr . numToHai) consumedNum
        hai      = numToHai consumedHai
        target   = (actor + targetR) `mod` 4
        typ      | ((shiftR nakiRaw 3) .&. 1) /= 0 = "pon"
                 | otherwise                       = "kakan"

        block1  = shiftR nakiRaw 9
        called  = block1 `mod` 3
        base    = 4 * (block1 `div` 3)
        tile4th = (shiftR nakiRaw 5) .&. 3
        targetR = nakiRaw .&. 3

        ponTile     = [i + base | i <- [0 .. 3], i /= tile4th]
        consumedNum = [(ponTile !! i) | i <- [0 .. 2], i /= called]
        consumedHai | typ == "pon" = ponTile !! called
                    | otherwise    = tile4th + base

    act_KAN :: Int -> Int -> JValue
    act_KAN actor nakiRaw = JObj obj' where
        obj' | typ == "ankan" =
            [
                ("actor",    JInt actor),
                ("consumed", JArr consumed),
                ("type",     JStr typ)
            ]
             | otherwise =
            [
                ("actor",    JInt actor),
                ("consumed", JArr consumed),
                ("pai",      JStr hai),
                ("target",   JInt target),
                ("type",     JStr typ)
            ]
        consumed | typ == "ankan" = map (JStr . numToHai) [base + i | i <- [0 .. 3]]
                 | otherwise      = map (JStr . numToHai) consumedNum
        hai      = numToHai consumedHai
        target   = (actor + targetR) `mod` 4
        typ      | target == actor = "ankan"
                 | otherwise       = "daiminkan"

        block1  = shiftR nakiRaw 8
        called  = block1 `mod` 4
        base    = 4 * (block1 `div` 4)
        targetR = nakiRaw .&. 3

        consumedNum = [i + base | i <- [0 .. 3], i /= called]
        consumedHai = called + base

act_ALL :: (Int, JValue) -> String -> (Int, JValue)
act_ALL (lst, JArr tmp) str = (now, JArr(tmp ++ ret)) where
    ret | (tag == "D" || tag == "E" || tag == "F" || tag == "G") = [act_DAHAI str lst] -- put last draw into it
        | (tag == "T" || tag == "U" || tag == "V" || tag == "W") = [snd (act_TSUMO str)] -- snd means string
        | (tag == "RYUUKYOKU") = [act_RYUUKYOKU str]
        | (tag == "DORA")      = [act_DORA str]
        | (tag == "REACH")     = [act_REACH str]
        | (tag == "AGARI")     = [act_AGARI str]
        | (tag == "INIT")      = [act_INIT str]
        | (tag == "N")         = [act_NAKI str]
        | otherwise = []

    now | (tag == "D" || tag == "E" || tag == "F" || tag == "G") = 0
        | (tag == "T" || tag == "U" || tag == "V" || tag == "W") = fst (act_TSUMO str) -- fst means int
        | (tag == "DORA" || tag == "REACH")  = lst -- open a new dora or reach do not change last draw
        | otherwise = 0 -- or reset it

    tag = get_tag str

act_GAME :: String -> JValue
act_GAME str = JObj obj where
    obj =
        [
            ("type",  JInt typ),
         -- ("lobby", JInt lobby),
            ("dan",   JArr jdan),
            ("rate",  JArr jrate),
            ("game",  game),
            ("owari", JArr owari)
        ]
    typ   = findXMLtoInt str "type"
 -- lobby = findXMLtoInt str "lobby"
    dan   = findXMLtoIntList (findXML str "dan")
    jdan  = map JInt dan
    rate  = findXMLtoDoubleList (findXML str "rate")
    jrate = map JNum rate
    game  = do_ALL (make_all(get_all str))
    owari = map (JInt . round) [xs !! (i*2) | i <- [0 .. 3]]
    xs    = findXMLtoDoubleList (findXML str "owari")
    

do_ALL :: [String] -> JValue
do_ALL xs = snd (foldl act_ALL (0, JArr []) xs)

main = do
    str <- getLine
    putStrLn (show (act_GAME str)) where
