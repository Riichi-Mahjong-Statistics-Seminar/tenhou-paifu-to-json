#include<iostream>
#include<regex>

void stringSplit(const std::string& str, const std::string& split, std::vector<std::string>& res){
    char* strc = new char[str.size() + 1];
    strcpy(strc, str.c_str());
    char* temp = std::strtok(strc, split.c_str());
    while (temp != NULL){
        res.push_back(std::string(temp));        
        temp = std::strtok(NULL, split.c_str());
    }
    delete[] strc;
}
std::string numToHai(int t){
    int num,isR=0;
    std::string color;
    std::string ret;
    if(t<0||t>=136)throw std::runtime_error("Cannot find corresponding hai from tehai");
    if(t==16||t==52||t==88) isR=1;
    t/=4;
    
    if(t<9) color='m';
    else if(t<18) color='p';
    else if(t<27) color='s';
    else color='z';
    
    num=t%9; num++;
    
    if(isR) ret+="0";
    else ret+=(char(num+'0'));
    ret+=color;
    
    return ret;
}

std::vector<std::pair<int, std::string> > Out;
std::string matchXML(const std::string& str, const std::string& pattern, const std::string& errlog){
    std::regex _pattern (pattern + "=\"(.*?)\"");
    std::smatch _typeMatch;
    if(!regex_search(str, _typeMatch, _pattern)) throw std::runtime_error(errlog);
    return _typeMatch[1];
}
std::vector<std::string> matchAll (const std::string&str){
    
    std::regex pattern ("<(.*?)/>");
    std::smatch matchret;
    std::vector<std::string> ret;
    
    std::string::const_iterator itS = str.begin();
    std::string::const_iterator itE = str.end(); 
    
    while(regex_search(itS, itE, matchret, pattern)){
        ret.push_back(matchret[1]);
        itS=matchret[0].second;
    }
    
    return ret;
}
std::string act_RYUUKYOKU(const std::string&str){
    return "{\"type\":\"ryuukyoku\"}";
}
std::string act_DORA(const std::string&str){	
	std::string ret = matchXML(str, "hai", "Error in DORA");
    std::string hai = numToHai(std::atoi(ret.c_str()));
    return "{\"dora_marker\":\"" + hai + "\",\"type\":\"dora\"}";
}
std::pair<int, std::string> act_DAHAI(const std::string&str){
    char tag=str[0];
    std::string num=str.substr(1, str.length());
    
    int Lst = 0;
    std::pair<int, std::string> askWho = Out.back();
    if(askWho.first == 1004 || askWho.first == 1002) askWho = Out[Out.size()-2]; //Reach or Dora
    if(askWho.first>0) Lst = -1;
    else Lst = - askWho.first;
        
    std::string hai,actor,tsumogiri;
    switch(tag){
        case 'D':
            actor="0"; break;
        case 'E':
            actor="1"; break;
        case 'F':
            actor="2"; break;
        case 'G':
            actor="3"; break;
        default:
            throw std::runtime_error("Wrong tag for DAHAI");
    }
    int numhai = std::atoi(num.c_str());
    hai = numToHai(numhai);
    if(numhai == Lst) tsumogiri = "true";
    else tsumogiri = "false";
    return std::make_pair(numhai, "{\"actor\":" + actor + ",\"pai\":\"" + hai + "\",\"type\":\"dahai\",\"tsumogiri\"=\"" + tsumogiri + "\"}");
}
std::pair<int, std::string> act_TSUMO(const std::string&str){
    char tag = str[0];
    std::string num = str.substr(1, str.length());
    
    std::string hai,actor;
    switch(tag){
        case 'T':
            actor="0"; break;
        case 'U':
            actor="1"; break;
        case 'V':
            actor="2"; break;
        case 'W':
            actor="3"; break;
        default:
            throw std::runtime_error("Wrong tag for TSUMO");
    }
    int numhai = std::atoi(num.c_str());
    hai = numToHai(numhai);
    return make_pair(- numhai, "{\"actor\":" + actor + ",\"pai\":\"" + hai + "\",\"type\":\"tsumo\"}");
}
std::string act_INIT(const std::string&str){
    
    std::string bakaze, doraMarker, scores, tehais, honba, kyoku, kyotaku, oya;

	std::vector<std::string> seed;
    std::string seedMatch = matchXML(str, "seed", "Error in INITseed");
    stringSplit(seedMatch, ",", seed);
    if(seed.size()!=6) throw std::runtime_error("Error in INITseed length");
    
    int nowKyu = atoi(seed[0].c_str());
    if(nowKyu<4) bakaze = "E";
    else if(nowKyu<8) bakaze = "S";
    else bakaze = "W";
    
    kyoku = ((char)(nowKyu%4) + 1 + '0');
    honba = seed[1];
    kyotaku = seed[2];
    doraMarker = numToHai(atoi(seed[5].c_str()));
    
    std::string tenMatch = matchXML(str, "ten", "Error in INITten");
    std::vector<std::string> ten;
    stringSplit(tenMatch, ",", ten);
    if(ten.size()!=4) throw std::runtime_error("Error in INITten length");
    scores = "[" + ten[0] + "00," + ten[1] + "00," + ten[2] + "00," + ten[3] + "00]";
    
    oya = matchXML(str, "oya", "Error in INIToya");
    
    tehais = "[";
    for(int i=0;i<4;i++){
        tehais += "[";
        
        std::vector<std::string> _tehais;
        std::string tehaisMatch = matchXML(str, "hai" + std::to_string(i), "Error in INITtehais");
        stringSplit(tehaisMatch, ",", _tehais);
        
        if(_tehais.size()!=13) throw std::runtime_error("Error in INITtehais length");
        for(int j=0;j<12;j++) tehais += "\"" + numToHai(atoi(_tehais[j].c_str())) + "\"," ;
        tehais += "\"" + numToHai(atoi(_tehais[12].c_str())) + "\""; 
        tehais += "]";
        if(i!=3) tehais += ",";
    }
    tehais += "]" ;
    return "{\"bakaze\":\"" + bakaze + "\",\"dora_marker\":\"" + doraMarker + "\",\"honba\":" + honba + ",\"kyoku\":" + kyoku + ",\"kyotaku\":" + kyotaku + ",\"oya\":" + oya + ",\"scores\":" + scores + ",\"tehais\":" + tehais + ",\"type\":\"start_kyoku\"}";
}
std::string act_NAKI(const std::string&str){
    int actor, nakiRaw; 
    
    std::string whoMatch = matchXML(str, "who", "Error in NAKIwho");
    actor = atoi(((std::string)whoMatch).c_str());
    std::string nakiMatch = matchXML(str, "m", "Error in NAKIm");
    nakiRaw = atoi(((std::string)nakiMatch).c_str());
    
    std::string ret;
    if(nakiRaw&(1<<2)){                //chii
        int block1=nakiRaw>>10;
        int called=block1%3,base=block1/3;
        base+=2*(base/7);
        base*=4;
        int target=(actor+3)%4;
        int tileDetail[3]={(nakiRaw>>3)&3,(nakiRaw>>5)&3,(nakiRaw>>7)&3};
        int cnt=0,consumed[2];
        for(int i=0;i<3;i++){
            if(called==i){
                continue;
            }
            consumed[cnt++]=tileDetail[i]+4*i+base;
        }
        ret+="{\"actor\":" + std::to_string(actor) + ",\"consumed\":[\"" + numToHai(consumed[0]) + "\",\"" + numToHai(consumed[1]) + "\"],";
        ret+="\"pai\":\"" + numToHai(tileDetail[called]+called*4+base) + "\",";
        ret+="\"target\":" + std::to_string(target) + ",\"type\":\"chi\"}";
    }else if(nakiRaw&(3<<3)){        //pon or shouminkan
        int block1=nakiRaw>>9;
        int called=block1%3,base=block1/3;
        base*=4;
        int tile4th=(nakiRaw>>5)&3;
        int targetRelative=nakiRaw&3;
        int target=(actor+targetRelative)%4;
        int cntp=0,ponTile[3],cntc=0,consumed[2];
        for(int i=0;i<4;i++){
            if(tile4th==i){
                continue;
            }
            ponTile[cntp++]=i+base;
        }
        for(int i=0;i<3;i++){
            if(called==i){
                continue;
            }
            consumed[cntc++]=ponTile[i];
        }
        if((nakiRaw>>3)&1){        //pon
            ret+="{\"actor\":" + std::to_string(actor) + ",\"consumed\":[\"" + numToHai(consumed[0]) + "\",\"" + numToHai(consumed[1]) + "\"],";
            ret+="\"pai\":\"" + numToHai(ponTile[called]) + "\",";
            ret+="\"target\":" + std::to_string(target) + ",\"type\":\"pon\"}";
        }else{                    //kakan
            ret+="{\"actor\":" + std::to_string(actor) + ",\"consumed\":[\"" + numToHai(ponTile[0]) + "\",\"" + numToHai(ponTile[1]) + "\",\"" + numToHai(ponTile[2]) + "\"],";
            ret+="\"pai\":\"" + numToHai(tile4th+base) + "\",";
            ret+="\"target\":" + std::to_string(target) + ",\"type\":\"kakan\"}";
        }
    }else{                            //daiminkan or ankan
        int block1=nakiRaw>>8;
        int called=block1%4,base=block1/4;
        base*=4;
        int targetRelative=nakiRaw&3;
        int target=(actor+targetRelative)%4;
        int cnt=0,consumed[3];
        for(int i=0;i<4;i++){
            if(called==i){
                continue;
            }
            consumed[cnt++]=i+base;
        }
        if(target==actor){            //ankan
            ret+="{\"actor\":" + std::to_string(actor) + ",\"consumed\":[\"" + numToHai(base) + "\",\"" + numToHai(base+1) + "\",\"" + numToHai(base+2) + "\",\"" + numToHai(base+3) + "\"],";
            ret+="\"pai\":\"" + numToHai(called+base) + "\",";
            ret+="\"target\":" + std::to_string(target) + ",\"type\":\"ankan\"}";
        }else{                        //daiminkan
            ret+="{\"actor\":" + std::to_string(actor) + ",\"consumed\":[\"" + numToHai(consumed[0]) + "\",\"" + numToHai(consumed[1]) + "\",\"" + numToHai(consumed[2]) + "\"],";
            ret+="\"pai\":\"" + numToHai(called+base) + "\",";
            ret+="\"target\":" + std::to_string(target) + ",\"type\":\"daiminkan\"}";
        }
    }
    return ret;
}
std::string act_REACH(const std::string&str){
    std::string who, type ,ret;
    
    who = matchXML(str, "who", "Error in REACHwho");
	type = matchXML(str, "step", "Error in REACHtype");
    
    ret = "{\"actor\":" + who + ",\"type\":\"";
    if(type=="1") ret += "reach";
    else if(type=="2")ret += "reach_accepted";
    else throw std::runtime_error("Error in REACHtype");
    ret += "\"}";
    return ret;
}
std::string act_AGARI(const std::string&str){
    std::string actor, fromwho, ret;
    
    actor = matchXML(str, "who", "Error in AGARIwho");
    fromwho = matchXML(str, "fromWho", "Error in AGARIfromwho");
    
    ret = "{\"actor\":" + actor + ",\"fromwho\":" + fromwho + ",\"type\":\"agari\"}";
    return ret;
}
std::pair<int, std::string> act_ALL(const std::string&str){
    std::regex Pattern ("[A-Z]*");
    std::smatch Match;
    if(!regex_search(str, Match, Pattern)) throw std::runtime_error("Error in getTAG");
    std::string tag=Match[0];
    if(tag=="D" || tag=="E" || tag=="F" || tag=="G") return act_DAHAI(str);
    if(tag=="T" || tag=="U" || tag=="V" || tag=="W") return act_TSUMO(str);
    if(tag=="INIT")    return std::make_pair(0, act_INIT(str));
    if(tag=="N") return std::make_pair(1001, act_NAKI(str));
    if(tag=="DORA") return std::make_pair(1002, act_DORA(str));
    if(tag=="RYUUKYOKU") return std::make_pair(1003, act_RYUUKYOKU(str));    
    if(tag=="REACH") return std::make_pair(1004, act_REACH(str));
    if(tag=="AGARI") return std::make_pair(1005, act_AGARI(str));
    else return std::make_pair(-114514,"");
}
std::vector<std::string>  act_PRE(const std::string&str){
    std::string type, lobby, dan, rating, sex;
	/* TODO (#1#): */
	
}
/*
INIT 0
TSUMO [1,136]
DAHAI [-136,-1]
CONSUME 1001
DORA 1002
RYUUKYOKU 1003
REACH 1004
AGARI 1005
NULL -114514
*/
int main(){
    std::ios::sync_with_stdio(false);
    std::cin.tie(0);
    std::string t; getline(std::cin,t);
    std::vector<std::string> V = matchAll(t);
    for(auto i:V) {
        std::pair<int, std::string> temp = act_ALL(i);
        if(temp.first!=-114514) Out.push_back(temp);
    }
    for(int i=0;i<Out.size();i++){
        std::cout<<Out[i].second;
        if(i!=Out.size()-1) std::cout<<",";
        std::cout<<std::endl;
    }  
}