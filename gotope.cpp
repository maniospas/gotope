// gotope.cpp — Interpreter with raw buffer tape, scoped identifiers, assignment
#include <bits/stdc++.h>
#include <chrono>
#define TAPE_SIZE 1024
#define STORAGE int64_t
#include <cstdint>
using namespace std;
union Cast {
    STORAGE i;
    double d;
};
inline Cast fromi(int64_t v) { Cast c; c.i = v; return c; }
inline Cast fromd(double v) { Cast c; c.d = v; return c; }

struct Label {
    string name;
    STORAGE tape_index;
    STORAGE depth;
};

struct Program {
    STORAGE tape[TAPE_SIZE];
    STORAGE iscall[TAPE_SIZE];
    STORAGE tape_pos = 0;
    vector<struct Label> labels;
    unordered_map<string,int> lastLabel;
    unordered_map<string,int> streamIds;
    int nextStreamId = 0;
};

struct VM {
    Program prog;
    explicit VM(Program p):prog(std::move(p)) {}

    // bool step_once() noexcept {
    //     bool modified = false;
    //     STORAGE* T = prog.tape;
    //     STORAGE* C = prog.iscall;
    //     for (STORAGE i = 0; i < prog.tape_pos; i++) {
    //         if (C[i] != 1) continue;
    //         STORAGE op = T[i];
    //         if (op == 0x02) {STORAGE zpos = i - 1;i++;STORAGE mul = T[T[i]];i++;mul = fromd(fromi(mul).d*fromi(T[T[i]]).d).i;if (T[zpos] != mul) modified = true;T[zpos] = mul;}
    //         if (op == 0x03) {STORAGE zpos = i - 1;i++;STORAGE mul = T[T[i]];i++;mul = fromd(fromi(mul).d+fromi(T[T[i]]).d).i;if (T[zpos] != mul) modified = true;T[zpos] = mul;}
    //         if (op == 0x07) {STORAGE zpos = i - 1;i++;STORAGE mul = T[T[i]];i++;mul = fromd(pow(fromi(mul).d, fromi(T[T[i]]).d)).i;if (T[zpos] != mul) modified = true;T[zpos] = mul;}
    //         if (op == 0x08) {STORAGE zpos = i - 1;i++;STORAGE mul = T[T[i]];i++;mul = fromd((fromi(mul).d<fromi(T[T[i]]).d)?1.0:-1.0).i;if (T[zpos] != mul) modified = true;T[zpos] = mul;}
    //         else if (op == 0x05) { // assignment
    //             i++;
    //             STORAGE lhs = T[i];
    //             i++;
    //             STORAGE rhs = T[i];
    //             if (T[lhs] != T[rhs]) modified = true;
    //             T[lhs] = T[rhs];
    //         }
    //     }
    //     return modified;
    // }

    bool step_once() noexcept {
        bool modified = false;
        STORAGE* T = prog.tape;
        STORAGE* C = prog.iscall;

        #pragma omp parallel for reduction(||:modified)
        for (STORAGE i = 0; i < prog.tape_pos; i++) {
            if (C[i] != 1) continue;
            STORAGE op = T[i];
            if (op == 0x02) {STORAGE zpos=i-1;STORAGE a=T[T[i+1]];STORAGE b=T[T[i+2]];STORAGE r=fromd(fromi(a).d*fromi(b).d).i;if(T[zpos]!=r)modified=true;T[zpos]=r;}
            else if (op == 0x03) {STORAGE zpos=i-1;STORAGE a=T[T[i+1]];STORAGE b=T[T[i+2]];STORAGE r=fromd(fromi(a).d+fromi(b).d).i;if(T[zpos]!=r)modified=true;T[zpos]=r;}
            else if (op == 0x07) {STORAGE zpos=i-1;STORAGE a=T[T[i+1]];STORAGE b=T[T[i+2]];STORAGE r=fromd(pow(fromi(a).d,fromi(b).d)).i;if(T[zpos]!=r)modified=true;T[zpos]=r;}
            else if (op == 0x08) {STORAGE zpos=i-1;STORAGE a=T[T[i+1]];STORAGE b=T[T[i+2]];STORAGE r=fromd((fromi(a).d<fromi(b).d)?1.0:-1.0).i;if(T[zpos]!=r)modified=true;T[zpos]=r;}
            else if (op == 0x05) {STORAGE lhs=T[i+1];STORAGE rhs=T[i+2];if(T[lhs]!=T[rhs])modified=true;T[lhs]=T[rhs];}
        }

        return modified;
    }


    void run() noexcept {
        using clock = std::chrono::high_resolution_clock;
        auto start = clock::now();
        cout << "\033[2J\n"; // clear screen first
        bool running = true;
        STORAGE loops = 0;
        const STORAGE maxloops = 100000;
        while(running) {
            running = step_once();
            loops++;
            if(loops >= maxloops) running = false;
            if(!running || loops %1000==0) { // affect the console at regular intervals
                std::ostringstream out;
                unordered_map<int, vector<string>> streamCaches;
                for (STORAGE i = 0; i < prog.tape_pos; i++) if (prog.iscall[i] == 1 && prog.tape[i] == 0x04) {
                    i++;STORAGE loc = prog.tape[i];i++;STORAGE streamTarget = prog.tape[i]-16;
                    string out;
                    if (prog.iscall[loc] == 2) {
                        while (prog.tape[loc]) {out += (char)prog.tape[loc];++loc;}
                    } else out = to_string(fromi(prog.tape[loc]).d);
                    streamCaches[streamTarget].push_back(move(out));
                }
                out << "\033[2J\033[H";
                for (auto &[id, vec] : streamCaches) if(id==0) for(auto &item : vec) out << item;
                out << "\n";
                auto now = clock::now();
                auto elapsed = std::chrono::duration_cast<std::chrono::milliseconds>(now - start).count();
                if (loops >= maxloops) out << "Exceeded max loops (" << maxloops << " loops, "<<elapsed<<"ms)\n";
                else if(running) out << "Running (terminate with ctrl+c)\n";
                else out << "Done (" << loops << " loops, "<<elapsed<<"ms)\n";
                cout << out.str();
            }
        }
    }
};







static bool isIdentStart(char c){ return std::isalpha((unsigned char)c) || c=='_'; }
static bool isIdent(char c){ return std::isalnum((unsigned char)c) || c=='_'; }

struct Parser {
    string s;
    size_t i=0;
    STORAGE depth=0;
    Program prog;
    Parser(string src):s((src)){}
    void skipWS(){
        while(i<s.size()){
            char c=s[i];
            if(c==' '||c=='\t'||c=='\n'||c=='\r'){ i++; continue; }
            if(c=='/' && i+1<s.size() && s[i+1]=='/'){ i+=2; while(i<s.size() && s[i]!='\n') i++;continue;}
            break;
        }
    }

    void addLabel(const string &name) {
        prog.lastLabel[name] = prog.labels.size();
        prog.labels.push_back({name, prog.tape_pos, depth});
    }
    void addRenamedLabel(const string &name, STORAGE redirect) {
        prog.lastLabel[name] = prog.labels.size();
        prog.labels.push_back({name, redirect, depth});
    }

    bool matchChar(char c){ skipWS(); if(i<s.size() && s[i]==c){ i++; return true;} return false; }
    bool peekChar(char c){ skipWS(); return i<s.size() && s[i]==c; }

    optional<string> parseIdent(){
        skipWS(); size_t j=i;
        if(j<s.size() && isIdentStart(s[j])){
            j++; while(j<s.size() && isIdent(s[j])) j++;
            string id = s.substr(i, j-i); i=j; return id;
        }
        return nullopt;
    }

    // Scoped identifier like a.b.c
    optional<string> parseScopedIdent(){
        skipWS();
        auto id = parseIdent();
        if(!id) return nullopt;
        string name=*id;
        while(peekChar('.')){
            matchChar('.');
            auto sub = parseIdent();
            if(!sub) throw runtime_error("Expected sub-identifier after '.'");
            name += "." + *sub;
        }
        return name;
    }

    optional<long long> parseNumber(){
        skipWS(); size_t j=i; if(j>=s.size()) return nullopt;
        if(!(isdigit((unsigned char)s[j]) || s[j]=='-' || s[j]=='+')) return nullopt;
        size_t k=j+1; while(k<s.size() && isdigit((unsigned char)s[k])) k++;
        long long val = stoll(s.substr(i, k-i)); i=k; return val;
    }

    bool parseString(STORAGE &tape_pos) {
        skipWS();
        if (i >= s.size() || s[i] != '"') return false;
        i++; // opening quote
        STORAGE start = tape_pos;
        while (i < s.size() && s[i] != '"') {
            unsigned char ch;
            if (s[i] == '\\') { // escape sequence
                i++;
                if (i >= s.size()) throw runtime_error("Unterminated escape sequence");
                switch (s[i]) {
                    case 'n': ch = '\n'; break;
                    case 't': ch = '\t'; break;
                    case 'r': ch = '\r'; break;
                    case '\\': ch = '\\'; break;
                    case '"': ch = '"'; break;
                    case '0': ch = '\0'; break;
                    // you can extend with \xHH or \uXXXX parsing if needed
                    default:
                        throw runtime_error(string("Unknown escape sequence: \\") + s[i]);
                }
                i++;
            } else {
                ch = (unsigned char) s[i++];
            }
            prog.tape[tape_pos] = ch;
            prog.iscall[tape_pos] = 0;
            tape_pos++;
        }
        if (i >= s.size() || s[i] != '"') throw runtime_error("Unterminated string literal");
        i++; // closing quote
        prog.tape[tape_pos] = 0x00;
        prog.iscall[tape_pos] = 0;
        tape_pos++;
        prog.iscall[start] = 2; // do this only now, in case string is empty
        return true;
    }

    // Resolve dotted name
    STORAGE resolveScoped(const string& token){
        if(prog.lastLabel.count(token)) {
            if(prog.labels[prog.lastLabel[token]].depth>depth) throw runtime_error("Symbol not in scope, or it has been shadowed: "+token);
            return prog.labels[prog.lastLabel[token]].tape_index;
        }

        vector<string> parts;
        string cur;
        for(char c:token){ if(c=='.'){ parts.push_back(cur); cur.clear(); } else cur.push_back(c); }
        if(!cur.empty()) parts.push_back(cur);
        if(parts.empty()) throw runtime_error("Empty identifier");

        STORAGE found=-1;
        for(STORAGE k=(int)prog.labels.size()-1;k>=0;k--) if(prog.labels[k].name==parts[0]){found=k; break;}
        if(found==-1) throw runtime_error("Unknown symbol: "+parts[0]);
        if(prog.labels[found].depth>depth) throw runtime_error("Symbol not in scope, or it has been shadowed: "+parts[0]);
        STORAGE idx = prog.labels[found].tape_index;
        for(size_t p=1;p<parts.size();++p){
            bool ok=false;
            for(STORAGE k=found+1;k<(int)prog.labels.size();k++){
                if(prog.labels[k].depth<depth+(int)p-1) break;
                if(prog.labels[k].name==parts[p]){idx=prog.labels[k].tape_index; found=k; ok=true; break;}
            }
            if(!ok) throw runtime_error("Unknown sub-element: "+parts[p]);
        }
        return idx;
    }

    vector<STORAGE> parseArgList(STORAGE &tape_pos){
        vector<STORAGE> args;
        if(!matchChar('(')) throw runtime_error("Expected '('");
        while(true){
            skipWS(); if(i>=s.size()) throw runtime_error("Unterminated arg list");
            if(s[i]=='"'){
                args.push_back(tape_pos);
                parseString(tape_pos);
            } else if(isIdentStart(s[i])) {
                auto id = parseScopedIdent();
                args.push_back(resolveScoped(*id));
            } else {
                auto n = parseNumber();
                if(!n.has_value()) throw runtime_error("Expected arg");
                args.push_back(fromd((double)(int)*n).i);
            }
            skipWS();
            if(peekChar(')')){ i++; break; }
            if(!matchChar(',')) throw runtime_error("Expected ',' or ')' in args");
        }
        return args;
    }

    void encodeCall(const string& sym, STORAGE opcode, const vector<STORAGE>& args){
        //STORAGE start = prog.tape_pos;
        prog.iscall[prog.tape_pos] = 0;
        prog.tape[prog.tape_pos++] = 0;
        prog.iscall[prog.tape_pos] = 1;
        prog.tape[prog.tape_pos++] = opcode;
        if(opcode==0x01){ // >name
            STORAGE destLoc = resolveScoped(sym);
            prog.iscall[prog.tape_pos] = 0;
            prog.tape[prog.tape_pos++] = destLoc;
        }
        for(STORAGE a:args) {
            prog.iscall[prog.tape_pos] = 0;
            prog.tape[prog.tape_pos++] = a;
        }
    }

    void parse(){
        prog.streamIds["out"] = prog.nextStreamId++;
        prog.tape[0] = 0;
        prog.iscall[0] = 0;
        prog.tape_pos++;
        while(i<s.size()){
            skipWS();
            if(i>=s.size()) break;
            if(isIdentStart(s[i])){
                size_t save=i;
                auto id=parseScopedIdent();
                skipWS();
                if(s[i]=='{') {
                    i++;
                    addLabel(*id);
                    depth++;
                    continue;
                }
                if(s[i]==':' && i<s.size()-1 && s[i+1]!='=' && s[i+1]!='+' && s[i+1]!='*' && s[i+1]!='<' && s[i+1]!='^') throw runtime_error("Unexpected : must be followed by one of =+*");
                if(s[i]==':' && i<s.size()-1 && s[i+1]!='='){
                    i++;
                    addLabel(*id);
                    continue;
                }

                i=save;
            }
            if(isIdentStart(s[i])) {
                size_t save = i;
                auto lhsScoped = parseScopedIdent();
                skipWS();
                // --- renaming operator := ---
                if (matchChar(':')) {
                    if (!matchChar('=')) throw runtime_error("Expected ':='");
                    if (!lhsScoped) throw runtime_error("Expected identifier before ':='");
                    if (lhsScoped->find('.') != string::npos) throw runtime_error("Renaming ':=' not allowed with dotted names at left-hand-side (maybe you meant to assign with =): " + *lhsScoped);
                    string lhs = *lhsScoped;
                    skipWS();
                    STORAGE rhsLoc;
                    if (s[i] == '"') {
                        STORAGE strStart = prog.tape_pos;
                        parseString(prog.tape_pos);
                        rhsLoc = strStart;
                    } else if (isIdentStart(s[i])) {
                        auto rhs = parseScopedIdent();
                        if (!rhs) throw runtime_error("Expected identifier after ':='");
                        rhsLoc = resolveScoped(*rhs);
                    } else {
                        auto n = parseNumber();
                        if (!n) throw runtime_error("Expected RHS of ':='");
                        const string dummy="__";
                        addLabel(dummy);
                        prog.tape[prog.tape_pos] = fromd((double)(int)*n).i;
                        prog.iscall[prog.tape_pos] = 0;
                        prog.tape_pos++;
                        rhsLoc = prog.labels[prog.lastLabel[dummy]].tape_index;
                    }
                    addRenamedLabel(lhs, rhsLoc);
                    continue;
                }
                // --- normal assignment '=' ---
                if (matchChar('=')) {
                    auto lhs = lhsScoped;
                    if (!lhs) throw runtime_error("Expected identifier before '='");
                    skipWS();
                    STORAGE rhsLoc;
                    if (s[i] == '"') {
                        STORAGE strStart = prog.tape_pos;
                        parseString(prog.tape_pos);
                        rhsLoc = strStart;
                    } else if (isIdentStart(s[i])) {
                        auto rhs = parseScopedIdent();
                        rhsLoc = resolveScoped(*rhs);
                    } else {
                        auto n = parseNumber();
                        if (!n) throw runtime_error("Expected RHS of assignment");
                        string dummy="__";
                        addLabel(dummy);
                        prog.tape[prog.tape_pos] = fromd((double)(int)*n).i;
                        prog.iscall[prog.tape_pos] = 0;
                        prog.tape_pos++;
                        rhsLoc = prog.labels[prog.lastLabel[dummy]].tape_index;
                    }
                    prog.tape[prog.tape_pos] = 0;
                    prog.iscall[prog.tape_pos] = 0;
                    prog.tape_pos++;
                    prog.tape[prog.tape_pos] = 0x05; // assignment opcode
                    prog.iscall[prog.tape_pos] = 1;
                    prog.tape_pos++;
                    prog.tape[prog.tape_pos] = resolveScoped(*lhs);
                    prog.iscall[prog.tape_pos] = 0;
                    prog.tape_pos++;
                    prog.tape[prog.tape_pos] = rhsLoc;
                    prog.iscall[prog.tape_pos] = 0;
                    prog.tape_pos++;
                    continue;
                }
                i = save;
            }

            if (isIdentStart(s[i])) {
                size_t save = i;
                auto lhsScoped = parseScopedIdent();
                skipWS();
                if (matchChar('|')) {
                    if (!lhsScoped) throw runtime_error("Expected identifier before '|'");
                    string streamSym = *lhsScoped;
                    if (!prog.streamIds.count(streamSym))
                        prog.streamIds[streamSym] = prog.nextStreamId++;
                    int streamTarget = 16 + prog.streamIds[streamSym];

                    skipWS();
                    STORAGE rhsLoc;
                    if (s[i] == '"') {
                        STORAGE strStart = prog.tape_pos;
                        parseString(prog.tape_pos);
                        rhsLoc = strStart;
                    } else if (isIdentStart(s[i])) {
                        auto rhs = parseScopedIdent();
                        if (!rhs) throw runtime_error("Expected identifier after '|'");
                        rhsLoc = resolveScoped(*rhs);
                    } else {
                        auto n = parseNumber();
                        if (!n) throw runtime_error("Expected RHS after '|'");
                        string dummy="__";
                        addLabel(dummy);
                        prog.tape[prog.tape_pos] = fromd((double)(int)*n).i;
                        prog.iscall[prog.tape_pos] = 0;
                        prog.tape_pos++;
                        rhsLoc = prog.labels[prog.lastLabel[dummy]].tape_index;
                    }

                    vector<STORAGE> args;
                    args.push_back(rhsLoc);
                    args.push_back(streamTarget);
                    encodeCall("|", 0x04, args);
                    continue;
                }
                i = save;
            }

            if (peekChar('|')) runtime_error("A stream name is expected at the LHS of |");

            //if(peekChar('>')){ matchChar('>'); auto id=parseScopedIdent(); auto args=parseArgList(prog.tape_pos); encodeCall(*id,0x01,args); continue; prog.iscall[prog.tape_pos] = 0;prog.tape[prog.tape_pos++] = 0x00;}
            if(peekChar('{')){ matchChar('{'); depth++;continue;}
            if(peekChar('}')){ matchChar('}'); depth--;if(depth<0) throw runtime_error("Imbalanced brackets - extra }");continue;}
            if(peekChar('*')){ matchChar('*'); auto args=parseArgList(prog.tape_pos); if(args.size()!=2) throw runtime_error("* requires two arguments"); encodeCall("*",0x02,args); continue; }
            if(peekChar('+')){ matchChar('+'); auto args=parseArgList(prog.tape_pos); if(args.size()!=2) throw runtime_error("+ requires two arguments");encodeCall("+",0x03,args); continue; }
            if(peekChar('^')){ matchChar('^'); auto args=parseArgList(prog.tape_pos); if(args.size()!=2) throw runtime_error("+ requires two arguments");encodeCall("^",0x07,args); continue; }
            if(peekChar('<')){ matchChar('<'); auto args=parseArgList(prog.tape_pos); if(args.size()!=2) throw runtime_error("+ requires two arguments");encodeCall("<",0x08,args); continue; }
            if(peekChar('&')){ matchChar('&'); auto args=parseArgList(prog.tape_pos); if(args.size()!=1) throw runtime_error("& requires one argument");
                args.push_back(0);
                encodeCall("&",0x06,args); continue;
            }
            if(s[i]=='"'){ parseString(prog.tape_pos); continue; }
            auto n=parseNumber();
            if(n.has_value()){
                prog.tape[prog.tape_pos]=fromd((double)(int)*n).i;
                prog.iscall[prog.tape_pos]=0;
                prog.tape_pos++;
                continue;
            }
            throw runtime_error("Leftover expression - only strings and numbers can be placed here");
        }
        if(depth) throw runtime_error("Imbalanced brackets - not closed {");
        STORAGE j = prog.tape_pos;
        while(j < TAPE_SIZE){
            prog.tape[j]=0;
            prog.iscall[j]=0;
            j++;
        }
    }
};


int main(int argc,char**argv){
    string src;
    if(argc>1){
        ifstream f(argv[1]);
        if(!f){ cerr<<"Cannot open "<<argv[1]<<"\n"; return 1;}
        src.assign((istreambuf_iterator<char>(f)),{});
    } else src.assign((istreambuf_iterator<char>(cin)),{});
    try{Parser p(src); p.parse(); VM vm(p.prog); vm.run();}
    catch(const exception&e){cerr<<"Error: "<<e.what()<<"\n"; return 1;}
    return 0;
}
