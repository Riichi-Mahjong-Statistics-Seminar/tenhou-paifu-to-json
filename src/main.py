import xml.etree.ElementTree as ET
import os
import json

def numToCol(num : int):
    r = num // 4
    if(r <  9): return "m"
    if(r < 18): return "p"
    if(r < 27): return "s"
    if(r < 36): return "z"
def numToID(num : int):
    return ((num // 4) % 9) + 1
def numToHai(num : int) :
    if(num == 16): return "0m"
    if(num == 52): return "0p"
    if(num == 88): return "0s"
    return str(numToID(num)) + numToCol(num)
def getActor(ch):
    if(ch in ('D', 'T')): return 0
    if(ch in ('E', 'U')): return 1
    if(ch in ('F', 'V')): return 2
    if(ch in ('G', 'W')): return 3
def conv(str):
    return listmap(int, list(str.split(",")))
def listmap(_a, _b):
    return list(map(_a, _b))
def traverseXml(element, ans):
    if len(element) > 0:
        for child in element:
            traverseXml(child, ans)
            # print(child.tag)
            # print(child.attrib)
            if(child.attrib == {}) : ans.append([child.tag[0], {'hai': child.tag[1:]}] )
            else : ans.append([child.tag, child.attrib])

def parse(filename):
    ans = []
    xmlFilePath = os.path.abspath(filename)
    tree = ET.parse(xmlFilePath)
    root = tree.getroot()
    ans.append([root.tag,root.attrib])
    traverseXml(root, ans)
    return ans

def actInit(dat):
    seed = conv(dat["seed"])
    nowKyu = seed[0]
    bakaze = "E" if nowKyu < 4 else ("S" if nowKyu < 8  else "W")
    doraMarker = numToHai(seed[5])
    honba = seed[1]
    kyotaku = seed[2]
    kyoku = (nowKyu % 4) + 1
    oya = int(dat["oya"])
    scores = listmap(lambda x:x*100, conv(dat["ten"]))
    tehais = [
        listmap(numToHai,conv(dat["hai0"])),
        listmap(numToHai,conv(dat["hai1"])),
        listmap(numToHai,conv(dat["hai2"])),
        listmap(numToHai,conv(dat["hai3"]))
    ]
    ret = {
        "bakaze"      : bakaze,
        "dora_marker" : doraMarker,
        "honba"       : honba,
        "kyoku"       : kyoku,
        "kyotaku"     : kyotaku,
        "oya"         : oya,
        "scores"      : scores,
        "tehais"      : tehais,
    }
    return ret

def actRyuukyoku(dat):
    reason = dat["type"] if "type" in dat else "howanpai"
    ret = {
        "reason" : reason,
        "type"   : "ryuukyoku"
    }
    return ret

def actDora(dat):
    hai = numToHai(dat["hai"])
    ret = {
        "dora_marker" : hai,
        "type"        : "dora"
    }
    return ret

def actReach(dat):
    actor = int(dat["who"])
    typenum = int(dat["step"])
    type = "reach" if typenum == 1 else "reach_accepted"
    ret = {
        "actor" : actor,
        "type" : type
    }
    return ret

def actDahai(letter, dat, lstdraw):
    actor = getActor(letter)
    hai = numToHai(int(dat["hai"]))
    tsumogiri = (lstdraw == hai)
    ret = {
        "actor"     : actor,
        "pai"       : hai,
        "type"      : "dahai",
        "tsumogiri" : tsumogiri
    }
    return ret

def actTsumo(letter, dat):
    actor = getActor(letter)
    hai = numToHai(int(dat["hai"]))
    ret = {
        "actor"     : actor,
        "pai"       : hai,
        "type"      : "tsumo",
    }
    return (ret, hai)

def actAgari(dat):
    ba = conv(dat["ba"])
    ten = conv(dat["ten"])
    if "yaku" in dat:
        yaku = conv(dat["yaku"])
    else:
        yaku = listmap(lambda x:[x, 13], conv(dat["yakuman"]))
    hai = listmap(numToHai, conv(dat["hai"]))
    if "m" in dat:
        naki = None # TODO 
    else :
        naki = None
    honba = ba[0]
    kyotaku = ba[1]
    machi = numToHai(int(dat["machi"]))
    han = 0 # TODOc
    hu = ten[0]
    score = ten[1]

    
    ret = {
        "honba"       : honba,
        "kyotaku"     : kyotaku,
        "hai"         : hai,
        "naki"        : jnaki,
        "machi"       : machi,
        "han"         : han,
        "hu"          : hu,
        "score"       : score,
        "yaku"        : jyaku,
        "dora_marker" : doraMarker,
        "ura_marker"  : uraMarker,
        "actor"       : actor,
        "fromwho"     : fromwho,
        "paowho"      : paowho,
        "type"        : "agari"
    }

def round(dat):
    assert(dat[0] [0] == "INIT")
    assert(dat[-1][0] in ("AGARI", "RYUUKYOKU"))
    # return actInit(dat[0][1])
    # return actRyuukyoku(dat[-1][1])


data = (parse("./samples/test.xml"))
f = open("./temp.json","w", encoding="utf-8")
nowRound = []
for i in data:
    tag = i[0]
    dict = i[1]
    # f.write(str(i) + "\n")
    f.write(tag + ":")
    f.write(str(dict) + "\n")
    if(tag == "INIT"):
        nowRound = [i]
    else:
        nowRound += [i]
        if (tag in ("AGARI", "RYUUKYOKU")):
            # f.write(str(nowRound))
            # f.write(str(round(nowRound)) + "\n")
            f.write(json.dumps((round(nowRound)), sort_keys=True, indent=4))

