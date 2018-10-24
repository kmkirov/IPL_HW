%{

#include "XPPLanguageTypesEnum.h"

%}

DIGIT    [0-9]
ID       [a-z_$][a-z0-9_$]*

%%


"!" 	return RelationNot;
"!="    return RelationNotEqual;
"#"    return PrefixMacro;
"&"    return BynaryAnd;
"&&"    return LogicalAnd;
"("    return LeftBracket;
")"    return RightBracket;
"*"    return MULT;
"^"    return BYNARYXOR;
"|"    return BYNARYOR;
"||"    return LOGICALOR;
"~"    return ARITMETICNOT;
"+"    return PLUS;
"++"    return INCREMENT;
"+="    return ADDITIVEASSIGNMENT;
","    return COMMA;
"-"    return MINUS;
"--"    return DECREMENT;
"-="    return SUBSTRACTIVEASSIGNMENT;
"."    return DOT;
"/"    return DEVICE;
"\\"   XPPLanguageTypes: ESCAPE;
"@"    return ESCAPEOFKEYWORD;
":"    return colons;
"::"    return DOUBLEcolons;
";"    return SEMICOLLUMN;
"<"    return LESSTHAN;
"<<"    return ShiftleftOperator;
"<="    return LESSTHANOREQUAL;
"="    return ASSIGNOPERATOR;
"=="    return EQUALCHECK;
">"    return GREATERTHAN;
">="    return GREATERTHANOREQUAL;
">>"    return ShiftRightOperator;
"?"    return TernalOperator;
"["    return LeftSquareBrackets;
"]"    return RightSquareBrackets;
"{"    return LeftCurlyBrackets;
"}"    return RightCurlyBrackets;
"abstract"    return ABSTRACT;
"anytype"    return ANYTYPE;
"asc"    return ASC;
"at"    return AT;
"avg"    return AVG;
"by"    return BY;
"byref"    return BYREF;
" "    return SPACE ;
"\0"    return EOF1 ;
"\n"    return NEWLINE ;
"\t"    return TAB ;
"catch"    return CATCH;
"changeCompany"    return CHANGECOMPANY;
"class"    return CLASS;
"client"    return CLIENT;
"container"    return CONTAINER;
"continue"    return CONTINUE;
"count"    return COUNT;
"crossCompany"    return CROSSCOMPANY;
"date"    return DATE;
"default"    return DEFAULT;
"delegate"    return DELEGATE;
"delete_from"    return DELETE_FROM;
"desc"    return DESC;
"display"    return DISPLAY;
"div"    return DIV;
"do"    return DO;
"edit"    return EDIT;
"else"    return ELSE;
"eventHandler"    return EVENTHANDLER;
"exists"    return EXISTS;
"extends"    return EXTENDS;
"false"    return FALSE;
"final"    return FINAL;
"firstFast"    return FIRSTFAST;
"firstOnly"    return FIRSTONLY;
"firstOnly10"    return FIRSTONLY10;
"firstOnly100"    return FIRSTONLY100;
"firstOnly1000"    return FIRSTONLY1000;
"flush"    return FLUSH;
"for"    return FOR;
"forceLiterals"    return FORCELITEREALS;
"forceNestedLoop"    return FORCENESTEDLOOP;
"forcePlaceholders"    return FORCEPLACEHOLDERS;
"forceSelectOrder"    return FORCESELECTORDER;
"forUpdate"    return FORUPDATE;
"from"    return FROM;
"group"    return GROUP;
"if"    return IF;
"implements"    return IMPLEMENTS;
"insert_recordset"    return INSERT_RECORDSET;
"int"    return INT;
"int64"    return INT64;
"interface"    return INTERFACE;
"is"    return IS;
"join"    return JOIN;
"like"    return LIKE;
"maxof"    return MAXOF;
"minof"    return MINOF;
"mod"    return MOD;
"new"    return NEW;
"next"    return NEXT;
"noFetch"    return NOFETCH;
"notExists"    return NOTEXISTS;
"null"    return NULL1;
"optimisticLock"    return OPTIMISTICLOCK;
"order"    return ORDER;
"outer"    return OUTER;
"pause"    return PAUSE;
"pessimisticLock"    return PESSIMISTICLOCK;
"print"    return PRINT;
"private"    return PRIVATE;
"protected"    return PROTECTED;
"public"    return PUBLIC;
"real"    return REAL;
"repeatableRead"    return REAPEATABLEREAD;
"retry"    return RETRY;
"return"   return RETURN;
"reverse"    return REVERSE;
"select"    return SELECT;
"server"    return SERVER;
"setting"    return SETTING;
"static"    return STATIC;
"str"    return STR;
"sum"    return SUM;
"super"    return SUPER;
"switch"    return SWITCH;
"this"    return THIS;
"throw"    return THROW;
"true"    return TRUE;
"try"    return TRY;
"ttsAbort"    return TTSABORT;
"ttsBegin"    return TTSBEGIN;
"ttsCommit"    return TTSCOMMIT;
"update_recordset"    return UPDATE_RECORDSET;
"validTimeState"    return VALIDTIMESTATE;
"void"    return VOID;
"where"    return WHERE;
"while"    return WHILE; 
"break"    return BREAK;
"breakpoint"    return BREAKPOINT;
"window"        return WINDOW;
'\0'            return EOF1;

%%

int yywrap()
{
    return 1;
}

