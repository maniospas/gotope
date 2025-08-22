// gotope.cpp — Interpreter with raw buffer tape, scoped identifiers, assignment
#include <bits/stdc++.h>
#define TAPE_SIZE 1024
using namespace std;

struct Program {
    int tape[TAPE_SIZE];
    int iscall[TAPE_SIZE];   // 1 if this slot belongs to a call/op, 0 otherwise
    int tape_pos = 0;
    vector<pair<string,int>> labels;     // name -> tape index (multiple allowed)
    unordered_map<string,int> lastLabel; // quick lookup: name -> last occurrence
};

static bool isIdentStart(char c){ return std::isalpha((unsigned char)c) || c=='_'; }
static bool isIdent(char c){ return std::isalnum((unsigned char)c) || c=='_'; }

struct Parser {
    string s; size_t i=0; Program prog;
    Parser(string src):s((src)){}

    void skipWS(){
        while(i<s.size()){
            char c=s[i];
            if(c==' '||c=='\t'||c=='\n'||c=='\r'){ i++; continue; }
            if(c=='/' && i+1<s.size() && s[i+1]=='/'){ // line comment
                i+=2; while(i<s.size() && s[i]!='\n') i++;
                continue;
            }
            break;
        }
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

    bool parseString(int &tape_pos){
        skipWS(); if(i>=s.size() || s[i] != '"') return false; i++;
        int start = tape_pos;
        while(i<s.size() && s[i] != '"'){
            prog.tape[tape_pos] = (unsigned char)s[i++];
            prog.iscall[tape_pos] = 0;
            tape_pos++;
        }
        if(i>=s.size() || s[i] != '"') throw runtime_error("Unterminated string literal");
        i++; // closing quote
        prog.tape[tape_pos] = 0x00;
        prog.iscall[tape_pos] = 0;
        tape_pos++;
        return true;
    }

    // Resolve dotted name
    int resolveScoped(const string& token){
        if(prog.lastLabel.count(token)) return prog.lastLabel[token];

        vector<string> parts;
        string cur;
        for(char c:token){ if(c=='.'){ parts.push_back(cur); cur.clear(); } else cur.push_back(c); }
        if(!cur.empty()) parts.push_back(cur);
        if(parts.empty()) throw runtime_error("Empty identifier");

        int found=-1;
        for(int k=(int)prog.labels.size()-1;k>=0;k--){
            if(prog.labels[k].first==parts[0]){
                found=k; break;
            }
        }
        if(found==-1) throw runtime_error("Unknown base: "+parts[0]);

        int idx = prog.labels[found].second;
        for(size_t p=1;p<parts.size();++p){
            bool ok=false;
            for(int k=found+1;k<(int)prog.labels.size();k++){
                if(prog.labels[k].first==parts[p]){
                    idx=prog.labels[k].second; found=k; ok=true; break;
                }
            }
            if(!ok) throw runtime_error("Unknown sub-element: "+parts[p]);
        }
        return idx;
    }

    vector<int> parseArgList(int &tape_pos){
        vector<int> args;
        if(!matchChar('(')) throw runtime_error("Expected '('");
        while(true){
            skipWS(); if(i>=s.size()) throw runtime_error("Unterminated arg list");
            if(s[i]=='"'){
                args.push_back(tape_pos);
                parseString(tape_pos);
            } else if(isIdentStart(s[i])){
                auto id = parseScopedIdent(); args.push_back(resolveScoped(*id));
            } else {
                auto n = parseNumber(); if(!n.has_value()) throw runtime_error("Expected arg");
                args.push_back((int)*n);
            }
            skipWS();
            if(peekChar(')')){ i++; break; }
            if(!matchChar(',')) throw runtime_error("Expected ',' or ')' in args");
        }
        return args;
    }

    void encodeCall(const string& sym, int opcode, const vector<int>& args){
        int start = prog.tape_pos;
        prog.iscall[prog.tape_pos] = 0;
        prog.tape[prog.tape_pos++] = 0;
        prog.iscall[prog.tape_pos] = 1;
        prog.tape[prog.tape_pos++] = opcode;
        if(opcode==0x01){ // >name
            int destLoc = resolveScoped(sym);
            prog.iscall[prog.tape_pos] = 0;
            prog.tape[prog.tape_pos++] = destLoc;
        }
        for(int a:args) {
            prog.iscall[prog.tape_pos] = 0;
            prog.tape[prog.tape_pos++] = a;
        }
    }

    void parse(){
        prog.tape[0] = 0;
        prog.iscall[0] = 0;
        prog.tape_pos++;
        while(i<s.size()){
            skipWS(); if(i>=s.size()) break;
            if(isIdentStart(s[i])){
                size_t save=i; auto id=parseScopedIdent(); skipWS();
                if(i<s.size() && s[i]==':' && s[i+1]!='='){
                    i++;
                    prog.labels.push_back({*id, prog.tape_pos});
                    prog.lastLabel[*id] = prog.tape_pos;
                    continue;
                } else { i=save; }
            }
            if (isIdentStart(s[i])) {
                size_t save = i;
                auto lhsScoped = parseScopedIdent();
                skipWS();

                // --- renaming operator := ---
                if (matchChar(':')) {
                    if (!matchChar('=')) throw runtime_error("Expected ':='");
                    if (!lhsScoped) throw runtime_error("Expected identifier before ':='");
                    if (lhsScoped->find('.') != string::npos)
                        throw runtime_error("Renaming ':=' not allowed with dotted names: " + *lhsScoped);

                    string lhs = *lhsScoped;
                    skipWS();

                    int rhsLoc;
                    if (s[i] == '"') {
                        int strStart = prog.tape_pos;
                        parseString(prog.tape_pos);
                        rhsLoc = strStart;
                    } else if (isIdentStart(s[i])) {
                        auto rhs = parseScopedIdent();
                        if (!rhs) throw runtime_error("Expected identifier after ':='");
                        rhsLoc = resolveScoped(*rhs);
                    } else {
                        auto n = parseNumber();
                        if (!n) throw runtime_error("Expected RHS of ':='");
                        string dummy="__";
                        prog.labels.push_back({dummy, prog.tape_pos});
                        prog.lastLabel[dummy] = prog.tape_pos;
                        prog.tape[prog.tape_pos] = (int)*n;
                        prog.iscall[prog.tape_pos] = 0;
                        prog.tape_pos++;
                        rhsLoc = prog.lastLabel[dummy];
                    }

                    // Just alias, no tape write
                    prog.labels.push_back({lhs, rhsLoc});
                    prog.lastLabel[lhs] = rhsLoc;
                    continue;
                }


                // --- normal assignment '=' ---
                if (matchChar('=')) {
                    auto lhs = lhsScoped;
                    if (!lhs) throw runtime_error("Expected identifier before '='");
                    skipWS();
                    int rhsLoc;
                    if (s[i] == '"') {
                        int strStart = prog.tape_pos;
                        parseString(prog.tape_pos);
                        rhsLoc = strStart;
                    } else if (isIdentStart(s[i])) {
                        auto rhs = parseScopedIdent();
                        rhsLoc = resolveScoped(*rhs);
                    } else {
                        auto n = parseNumber();
                        if (!n) throw runtime_error("Expected RHS of assignment");
                        string dummy="__";
                        prog.labels.push_back({dummy, prog.tape_pos});
                        prog.lastLabel[dummy] = prog.tape_pos;
                        prog.tape[prog.tape_pos] = (int)*n;
                        prog.iscall[prog.tape_pos] = 0;
                        prog.tape_pos++;
                        rhsLoc = prog.lastLabel[dummy];
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

                // otherwise rewind
                i = save;
            }


            if(peekChar('>')){ matchChar('>'); auto id=parseScopedIdent(); auto args=parseArgList(prog.tape_pos); encodeCall(*id,0x01,args); continue; prog.iscall[prog.tape_pos] = 0;prog.tape[prog.tape_pos++] = 0x00;}
            if(peekChar('*')){ matchChar('*'); auto args=parseArgList(prog.tape_pos); encodeCall("*",0x02,args); continue; }
            if(peekChar('+')){ matchChar('+'); auto args=parseArgList(prog.tape_pos); encodeCall("+",0x03,args); continue; }
            if (peekChar('`')) {
                matchChar('`');
                skipWS();
                vector<int> args;
                if (s[i] == '"') {args.push_back(prog.tape_pos);parseString(prog.tape_pos);
                } else if (isIdentStart(s[i])) {auto id = parseScopedIdent();args.push_back(resolveScoped(*id));
                } else {auto n = parseNumber();if (!n) throw runtime_error("Expected argument after '`'");args.push_back((int)*n);}
                encodeCall("'", 0x04, args);
                continue;
            }
            if(s[i]=='"'){ parseString(prog.tape_pos); continue; }
            auto n=parseNumber();
            if(n.has_value()){
                prog.tape[prog.tape_pos]=(int)*n;
                prog.iscall[prog.tape_pos]=0;
                prog.tape_pos++;
                continue;
            }
            throw runtime_error("Leftover expression: only strings and numbers can be placed here");
        }
        int j = prog.tape_pos;
        while(j < TAPE_SIZE){
            prog.tape[j]=0;
            prog.iscall[j]=0;
            j++;
        }
    }
};

// --- VM ---
struct VM {
    Program prog;
    vector<int> printCache;
    explicit VM(Program p):prog(std::move(p)){}
    bool step_once(){
        int* T=prog.tape;
        int* C=prog.iscall;
        for(int i=0;i<prog.tape_pos;i++){
            if(!C[i]) continue; // skip data
            int op=T[i];
                 if(op==0x02){int zpos=i-1;i++;int mul=T[T[i]];i++;mul*=T[T[i]];T[zpos]=mul;}
            else if(op==0x03){int zpos=i-1;i++;int sum=T[T[i]];i++;sum+=T[T[i]];T[zpos]=sum;}
            else if(op==0x04){i++; int loc=T[i++]; printCache.push_back(T[loc]);}
            else if(op==0x05){int lhs=T[++i];int rhs=T[++i];T[lhs]=T[rhs];}
        }
        return true;
    }
    void run(){
        cout<<"\033[2J\n";
        while(step_once()){
            std::ostringstream out;
            out << "\033[H"; // clear screen
            out << "Running (terminate with ctrl+c)";
            out << "\n";
            for(auto item:printCache){
                out << item << "\n";
            }
            printCache.clear();
            cout << out.str();
        }
    }

};

int main(int argc,char**argv){
    string src;
    if(argc>1){
        ifstream f(argv[1]);
        if(!f){ cerr<<"Cannot open "<<argv[1]<<"\n"; return 1;}
        src.assign((istreambuf_iterator<char>(f)),{});
    }
    else src.assign((istreambuf_iterator<char>(cin)),{});
    try{
        Parser p(src); p.parse(); VM vm(p.prog); vm.run();
    } catch(const exception&e){
        cerr<<"Error: "<<e.what()<<"\n"; return 1;
    }
    return 0;
}
