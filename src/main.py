import xml.etree.ElementTree as ET
import os
import json
import sys

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
    hai = numToHai(int(dat["hai"]))
    ret = {
        "dora_marker" : hai,
        "type"        : "dora"
    }
    return ret

def actReach(dat, junme):
    actor = int(dat["who"])
    typenum = int(dat["step"])
    type = "riichi" if typenum == 1 else "riichi_accepted"
    ret = {
        "junme" : junme[actor],
        "actor" : actor,
        "type" : type
    }
    return ret

def actDahai(letter, dat, lstdraw, junme):
    actor = getActor(letter)
    hai = numToHai(int(dat["hai"]))
    tsumogiri = (lstdraw == hai)
    ret = {
        "junme"     : junme[actor],
        "actor"     : actor,
        "pai"       : hai,
        "type"      : "dahai",
        "tsumogiri" : tsumogiri
    }
    return ret

def actTsumo(letter, dat, junme):
    actor = getActor(letter)
    junme[actor] += 1
    hai = numToHai(int(dat["hai"]))
    ret = {
        "junme"     : junme[actor],
        "actor"     : actor,
        "pai"       : hai,
        "type"      : "tsumo",
    }
    return (ret, hai)

def actChii(actor, nakiRaw, junme):
    tileDetail = [(nakiRaw >> 3) & 3, (nakiRaw >> 5) & 3, (nakiRaw >> 7) & 3]
    block1 = nakiRaw >> 10
    called = block1 % 3
    base = (block1 // 21) * 8 + (block1 // 3) * 4
    target = (actor + 3) % 4
    consumedHai = tileDetail[called] + 4 * called + base
    hai = numToHai(consumedHai)
    consumedNum = []
    for i in range(3):
        if(i != called):
            consumedNum.append(tileDetail[i] + 4 * i + base)
    consumed = listmap(numToHai, consumedNum)
    ret = {
        "junme"    : junme[actor],
        "actor"    : actor,
        "consumed" : consumed,
        "pai"      : hai,
        "target"   : target,
        "type"     : "chii"
    }
    return ret

def actPon(actor, nakiRaw, junme):
    tile4th = (nakiRaw >> 5) & 3
    targetR = nakiRaw & 3
    block1 = nakiRaw >> 9
    called = block1 % 3
    base    = 4 * (block1 // 3)
    target = (actor + targetR) % 4
    type = "pon" if ((nakiRaw >> 3) & 1) != 0 else "kakan"
    ponTile = []
    for i in range(4):
        if(i != tile4th):
            ponTile.append(i + base)
    if type == "pon":
        consumedHai = ponTile[called]
        consumedNum = []
        for i in range(3):
            if(i != called):
                consumedNum.append(ponTile[i])
    else:
        consumedHai = tile4th + base
        consumedNum = ponTile
    consumed = listmap(numToHai, consumedNum)
    hai = numToHai(consumedHai)
    ret = {
        "junme"    : junme[actor],
        "actor"    : actor,
        "consumed" : consumed,
        "pai"      : hai,
        "target"   : target,
        "type"     : type
    }
    return ret

def actKan(actor, nakiRaw, junme):
    targetR = nakiRaw & 3
    target = (actor + targetR) % 4
    block1  = nakiRaw >> 8
    called  = block1 % 4
    base    = 4 * (block1 // 4)
    consumedNum = []
    for i in range(4):
        if(i != called):
            consumedNum.append(i + base)
    consumedHai = called + base
    hai = numToHai(consumedHai)
    if target == actor:
        type = "ankan"
        consumed = listmap(numToHai, [base, base+1, base+2, base+3])
        ret = {
            "junme"    : junme[actor],
            "actor"    : actor,
            "consumed" : consumed,
            "type"     : type
        }
    else:
        type = "daiminkan"
        consumed = listmap(numToHai, consumedNum)
        ret = {
            "junme"    : junme[actor],
            "actor"    : actor,
            "consumed" : consumed,
            "pai"      : hai,
            "target"   : target,
            "type"     : type
        }
    return ret

def actNaki(dat, junme, flag):
    nakiRaw = int(dat["m"])
    actor = int(dat["who"])
    junme[actor] += flag
    if (nakiRaw & 4) != 0:
        return actChii(actor, nakiRaw, junme)
    if (nakiRaw & 24) != 0:
        return actPon(actor, nakiRaw, junme)
    return actKan(actor, nakiRaw, junme)

def actAgari(dat, junme):
    han = 0
    ba = conv(dat["ba"])
    ten = conv(dat["ten"])
    jyaku = []
    if "yaku" in dat:
        yaku = conv(dat["yaku"])
        for i in range(0, len(yaku), 2):
            nowyaku = yaku[i]
            val = yaku[i + 1]
            han += val
            if(nowyaku in (52, 53, 54)):
                for j in range(val):
                    jyaku.append(nowyaku)
            else:
                jyaku.append(nowyaku)
    else:
        yaku = conv(dat["yakuman"])
        for i in range(len(yaku)):
            han += 13
            jyaku.append(yaku[i])
    hai = listmap(numToHai, conv(dat["hai"]))
    honba = ba[0]
    kyotaku = ba[1]
    machi = numToHai(int(dat["machi"]))
    hu = ten[0]
    score = ten[1]
    doraMarker = listmap(numToHai, conv(dat["doraHai"]))
    if "doraHaiUra" in dat:
        uraMarker = listmap(numToHai, conv(dat["doraHaiUra"]))
    else:
        uraMarker = None    
    actor = int(dat["who"])
    fromwho = int(dat["fromWho"])
    if "paoWho" in dat:
        paowho = int(dat["paoWho"])
    else:
        paowho = None
    if "m" in dat:
        nakiRawList = conv(dat["m"])
        naki = []
        for nakiRaw in nakiRawList:
            nakidat = {"m" : nakiRaw, "who" : actor}
            temp = actNaki(nakidat, junme, 0)
            temp.pop("actor")
            naki.append(temp)

    else :
        naki = None
    ret = {
        "honba"       : honba,
        "kyotaku"     : kyotaku,
        "junme"       : junme[actor],
        "hai"         : hai,
        "naki"        : naki,
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
    return ret

def round(dat):
    assert(dat[0] [0] == "INIT")
    assert(dat[-1][0] in ("AGARI", "RYUUKYOKU"))
    roundData = actInit(dat[0][1])
    roundGame = []
    junme = [0, 0, 0, 0]
    for i in range(1, len(dat)):
        tag = dat[i][0]
        dict = dat[i][1]
        if tag in ("D", "E", "F", "G"):
            roundGame.append(actDahai(tag[0], dict, lst, junme))
            lst = 0
        if tag in ("T", "U", "V", "W"):
            temp = actTsumo(tag[0], dict, junme)
            roundGame.append(temp[0])
            lst = temp[1]
        if tag == "RYUUKYOKU":
            roundGame.append(actRyuukyoku(dict))
            lst = 0
        if tag == "DORA":
            roundGame.append(actDora(dict))
        if tag == "REACH":
            roundGame.append(actReach(dict, junme))
        if tag == "AGARI":
            roundGame.append(actAgari(dict, junme))
            lst = 0
        if tag == "N":
            roundGame.append(actNaki(dict, junme, 1))
            lst = 0
    ret = {
        "data" : roundData,
        "game" : roundGame
    }
    return ret
    # return actInit(dat[0][1])
    # return actRyuukyoku(dat[-1][1])

if len(sys.argv) == 3:
    inputfile = sys.argv[1]
    outputfile = sys.argv[2]
else:
    print("usage: py main.py <inputfile> <outputfile>")
    exit()
# print(inputfile)
os.system("pause") 
data = (parse(inputfile))
f = open(outputfile,"w", encoding="utf-8")
nowRound = []
rounds = []
# print(data)
for i in data:
    tag = i[0]
    dict = i[1]
    if(tag == "INIT"):
        nowRound = [i]
    else:
        nowRound += [i]
        if (tag in ("AGARI", "RYUUKYOKU")):
            rounds.append(round(nowRound))
if data[1][0] == "GO":
    type = int(data[1][1]["type"])
    lobby = None
    dan = conv(data[2][1]["dan"])
    rate = listmap(float, list(data[2][1]["rate"].split(",")))
else:
    type = int(data[2][1]["type"])
    if "lobby" in data[2][1]:
        lobby = int(data[2][1]["lobby"])
    else:
        lobby = None
    dan = conv(data[3][1]["dan"])
    rate = listmap(float, list(data[3][1]["rate"].split(",")))
owariData = list(nowRound[-1][1]["owari"].split(","))
owari = listmap(int,[owariData[0], owariData[2], owariData[4], owariData[6]])
ans = {
    "type"     : type,
    "lobby"    : lobby,
    "dan"      : dan,
    "rate"     : rate,
    "games"    : rounds,
    "owari"    : owari
}
f.write(json.dumps(ans, indent=4))