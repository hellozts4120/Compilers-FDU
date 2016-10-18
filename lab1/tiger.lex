%{
#include <string.h>
#include "util.h"
#include "tokens.h"
#include "errormsg.h"

int charPos=1;

int yywrap(void)
{
 charPos=1;
 return 1;
}


void adjust(void)
{
 EM_tokPos=charPos;
 charPos+=yyleng;
}

int commentStateNum = 0;
char *buffer;
unsigned int len = 0;
unsigned int capacity;
const int INITIAL_CAPACITY = 16;

void initBuffer(void) {
  capacity = 16;
  buffer = checked_malloc(INITIAL_CAPACITY);
  buffer[0] = 0;
}

void appendBuffer(char c) {
  int curLen = strlen(buffer);
  if (curLen + 1 >= capacity) {
    capacity = capacity * 2;
    char *tempBuffer;
    tempBuffer = checked_malloc(capacity);
    memcpy(tempBuffer, buffer, curLen + 1);
    free(buffer);
    buffer = tempBuffer;
  }
  buffer[curLen] = c;
  buffer[curLen + 1] = 0;
}

%}

%x IN_COMMENT IN_STRING

%%


  /* Cases when meet with white spaces, change line or so. */
[ \t\r]	 {adjust(); continue;}
\n	 {adjust(); EM_newline(); continue;}


  /* Cases when meet with punctuation symbols*/
","	 {adjust(); return COMMA;}
":"  {adjust(); return COLON;}
";"  {adjust(); return SEMICOLON;}
"("  {adjust(); return LPAREN;}
")"  {adjust(); return RPAREN;}
"["  {adjust(); return LBRACK;}
"]"  {adjust(); return RBRACK;}
"{"  {adjust(); return LBRACE;}
"}"  {adjust(); return RBRACE;}
"."  {adjust(); return DOT;}
"+"  {adjust(); return PLUS;}
"-"  {adjust(); return MINUS;}
"*"  {adjust(); return TIMES;}
"/"  {adjust(); return DIVIDE;}
"="  {adjust(); return EQ;}
"<>" {adjust(); return NEQ;}
"<"  {adjust(); return LT;}
"<=" {adjust(); return LE;}
">"  {adjust(); return GT;}
">=" {adjust(); return GE;}
"&"  {adjust(); return AND;}
"|"  {adjust(); return OR;}
":=" {adjust(); return ASSIGN;}

  /* Cases when meet with system preserved words */
for  	   {adjust(); return FOR;}
while    {adjust(); return WHILE;}
to       {adjust(); return TO;}
break    {adjust(); return BREAK;}
let      {adjust(); return LET;}
in       {adjust(); return IN;}
end      {adjust(); return END;}
function {adjust(); return FUNCTION;}
var      {adjust(); return VAR;}
type     {adjust(); return TYPE;}
array    {adjust(); return ARRAY;}
if       {adjust(); return IF;}
then     {adjust(); return THEN;}
else     {adjust(); return ELSE;}
do       {adjust(); return DO;}
of       {adjust(); return OF;}
nil      {adjust(); return NIL;}


  /* Case when meet with Identifiers */
[a-z|A-Z]+[a-z|A-Z|0-9|_]* {
    adjust();
    yylval.sval = yytext;
    return ID;
}


  /* Case of Integer literal */
[0-9]+	 {
    adjust(); 
    yylval.ival = atoi(yytext); 
    return INT;
}


  /* Case of Comment */
"/*" {
    adjust();
    commentStateNum += 1;
    BEGIN(IN_COMMENT);
}

"*/" {
    adjust();
    EM_error(EM_tokPos, "Not in comment state currently!");
    yyterminate();
}

<IN_COMMENT>{

    /* Use it if it's allowed to use nested comment...
    "/*" {
        adjust();
        commentStateNum += 1;
        continue;
    }
    */

    "*/" {
        adjust();
        commentStateNum -= 1;
        if (commentStateNum == 0) {
            BEGIN(INITIAL);
        }
    }

    "\n" {
        adjust();
        EM_newline();
        continue;
    }

    <<EOF>> {
        EM_error(EM_tokPos, "EOF detected!");
        yyterminate();
    }

    . {adjust();}
}

  /* Case of Strings */
\"  {
  adjust();
  initBuffer();
  len = charPos - 1;
  BEGIN(IN_STRING);
}

<IN_STRING>{
  \" {
    adjust();
    if (buffer[0] == '\0') {
      yylval.sval = "(null)";
    } else {
      yylval.sval = buffer;
    }

    EM_tokPos = len;
    BEGIN(INITIAL);

    return STRING;
  }

  <<EOF>> {
    EM_error(EM_tokPos, "EOF detected!");
    yyterminate();
  }

  . {
    adjust();
    char *temp = yytext;
    while (*temp) {
      appendBuffer(*temp++);
    }
  }

  \\n {adjust();appendBuffer('\n');}
  \\t {adjust();appendBuffer('\t');}
  \\v {adjust();appendBuffer('\v');}
  \\a {adjust();appendBuffer('\a');}
  \\r {adjust();appendBuffer('\r');}
  \\b {adjust();appendBuffer('\b');}
  \\f {adjust();appendBuffer('\f');}
  
}



.  {
    adjust(); 
    EM_error(EM_tokPos,"illegal token");
}
