#pragma once
enum XPPLanguageTypes {
	Error = -1
	, EOF1 = 0
	, RelationNot
	, RelationNotEqual
	, PrefixMacro
	, BynaryAnd
	, LogicalAnd
	, LeftBracket
	, RightBracket
	, MULT
	, BYNARYXOR
	, BYNARYOR
	, LOGICALOR
	, ARITMETICNOT
	, PLUS
	, INCREMENT
	, ADDITIVEASSIGNMENT
	, COMMA
	, MINUS
	, DECREMENT
	, SUBSTRACTIVEASSIGNMENT
	, DOT
	, DEVICE
	, ESCAPE
	, ESCAPEOFKEYWORD
	, colons
	, DOUBLEcolons
	, SEMICOLLUMN
	, LESSTHAN
	, ShiftleftOperator
	, LESSTHANOREQUAL
	, ASSIGNOPERATOR
	, EQUALCHECK
	, GREATERTHAN
	, GREATERTHANOREQUAL
	, ShiftRightOperator
	, TernalOperator
	, LeftSquareBrackets
	, RightSquareBrackets
	, LeftCurlyBrackets
	, RightCurlyBrackets
	, ABSTRACT
	, ANYTYPE
	, ASC
	, AT
	, AVG
	, BREAK
	, BREAKPOINT
	, BY
	, BYREF
	, CASE
	, CATCH
	, CHANGECOMPANY
	, CLASS
	, CLIENT
	, CONTAINER
	, CONTINUE
	, COUNT
	, CROSSCOMPANY
	, DATE
	, DEFAULT
	, DELEGATE
	, DELETE_FROM
	, DESC
	, DISPLAY
	, DIV
	, DO
	, EDIT
	, ELSE
	, EVENTHANDLER
	, EXISTS
	, EXTENDS
	, FALSE
	, FINAL
	, FIRSTFAST
	, FIRSTONLY
	, FIRSTONLY10
	, FIRSTONLY100
	, FIRSTONLY1000
	, FLUSH
	, FOR
	, FORCELITEREALS
	, FORCENESTEDLOOP
	, FORCEPLACEHOLDERS
	, FORCESELECTORDER
	, FORUPDATE
	, FROM
	, GROUP
	, IF
	, IMPLEMENTS
	, INSERT_RECORDSET
	, INT
	, INT64
	, INTERFACE
	, IS
	, JOIN
	, LIKE
	, MAXOF
	, MINOF
	, MOD
	, NEW
	, NEXT
	, NOFETCH
	, NOTEXISTS
	, NULL1
	, OPTIMISTICLOCK
	, ORDER
	, OUTER
	, PAUSE
	, PESSIMISTICLOCK
	, PRINT
	, PRIVATE
	, PROTECTED
	, PUBLIC
	, REAL
	, REAPEATABLEREAD
	, RETRY
	, RETURN
	, REVERSE
	, SELECT
	, SERVER
	, SETTING
	, STATIC
	, STR
	, SUM
	, SUPER
	, SWITCH
	, THIS
	, THROW
	, TRUE
	, TRY
	, TTSABORT
	, TTSBEGIN
	, TTSCOMMIT
	, UPDATE_RECORDSET
	, VALIDTIMESTATE
	, VOID
	, WHERE
	, WHILE
	, WINDOW
	, SPACE
	, TAB

	, NEWLINE
	, IDENTIFIER
	, NUMBER

};