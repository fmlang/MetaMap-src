#ifndef TOKEN_H
#define TOKEN_H

#include <stdlib.h>
#include <string>

#define EOS '\0'
/*
#define punctsym   1
#define stringsym  2
#define numericsym 3
#define hyphensym  4
#define periodsym  5
#define spacesym   6
#define eofsym     7
#define lambda     99
#define fatalsym   100
*/

using namespace std;

class Token {
 public:
  int tokentype;
  string text;
  Token(int type, string strbuf);
  Token(int type, const char* strbuf);
  Token(int type, const char ch);
  int getType();
  string getString();
  string getTypeString();
};

enum symbols { lambda, punctsym, stringsym, numericsym, hyphensym,
	       periodsym, spacesym, eofsym, fatalsym, };


#endif /*TOKEN_H*/
