#ifndef LIST_UTILS_H
#define LIST_UTILS_H

#include <stdlib.h>
#include <string>
#include <list>

using namespace std;

// split string into string tokens delimited by separator
list<string> split(string, char);
// join list of string tokens into single string with tokens delimited
// by separator
string join(list<string> aList, char separator);

#endif /*LIST_UTILS_H*/
