#ifndef SCANNER_H
#define SCANNER_H
#include "Token.h"

/*
  example of use:

  list<string> tokens;
  Token* token;
  Scanner scan(buffer);
  token = scan.getNextToken();
  while (token->getType() != eofsym) {
    switch (token->getType())
      {
      case stringsym: 
	tokens.push_back(token->getString());	
	break;
      case numericsym:
	tokens.push_back(token->getString());	
	break;
      }
    token = scan.getNextToken();
*/

class Scanner {
  const char *buffer;
  const char *bp;
  char getch();
  char nextch();
  void scan_errmsg_unexpch(char);
  void scan_errmsg_line();
  void scanfatal(string);
public:
  Scanner(string);
  void reset(string);
  Token* getNextToken();
  Token* getToken();
};

#endif /*SCANNER_H*/
