/* -*- Mode: C++; tab-width: 8; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 * vim: set ts=8 sts=4 et sw=4 tw=99:
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

#ifndef frontend_TokenKind_h
#define frontend_TokenKind_h

/*
 * List of token kinds and their ranges.
 *
 * The format for each line is:
 *
 *   macro(<TOKEN_KIND_NAME>, <DESCRIPTION>)
 *
 * or
 *
 *   range(<TOKEN_RANGE_NAME>, <TOKEN_KIND_NAME>)
 *
 * where ;
 * <TOKEN_KIND_NAME> is a legal C identifier of the token, that will be used in
 * the JS engine source, with `TOK_` prefix.
 *
 * <DESCRIPTION> is a string that describe about the token, and will be used in
 * error message.
 *
 * <TOKEN_RANGE_NAME> is a legal C identifier of the range that will be used to
 * JS engine source, with `TOK_` prefix. It should end with `_FIRST` or `_LAST`.
 * This is used to check TokenKind by range-testing:
 *   TOK_BINOP_FIRST <= tt && tt <= TOK_BINOP_LAST
 *
 * Second argument of `range` is the actual value of the <TOKEN_RANGE_NAME>,
 * should be same as one of <TOKEN_KIND_NAME> in other `macro`s.
 *
 * To use this macro, define two macros for `macro` and `range`, and pass them
 * as arguments.
 *
 *   #define EMIT_TOKEN(name, desc) ...
 *   #define EMIT_RANGE(name, value) ...
 *   FOR_EACH_TOKEN_KIND_WITH_RANGE(EMIT_TOKEN, EMIT_RANGE)
 *   #undef EMIT_TOKEN
 *   #undef EMIT_RANGE
 *
 * If you don't need range data, use FOR_EACH_TOKEN_KIND instead.
 *
 *   #define EMIT_TOKEN(name, desc) ...
 *   FOR_EACH_TOKEN_KIND(EMIT_TOKEN)
 *   #undef EMIT_TOKEN
 *
 * Note that this list does not contain ERROR and LIMIT.
 */
#define FOR_EACH_TOKEN_KIND_WITH_TAGGED(macro, tagged) \
    macro(EOF,         "end of script") \
    \
    /* only returned by peekTokenSameLine() */ \
    macro(EOL,          "line terminator") \
    \
    macro(SEMI,         "';'") \
    macro(COMMA,        "','") \
    macro(HOOK,         "'?'")    /* conditional */ \
    macro(COLON,        "':'")    /* conditional */ \
    macro(INC,          "'++'")   /* increment */ \
    macro(DEC,          "'--'")   /* decrement */ \
    macro(DOT,          "'.'")    /* member operator */ \
    macro(TRIPLEDOT,    "'...'")  /* rest arguments and spread operator */ \
    macro(LB,           "'['") \
    macro(RB,           "']'") \
    macro(LC,           "'{'") \
    macro(RC,           "'}'") \
    macro(LP,           "'('") \
    macro(RP,           "')'") \
    macro(ARROW,        "'=>'")   /* function arrow */ \
    macro(NAME,         "identifier") \
    macro(NUMBER,       "numeric literal") \
    macro(STRING,       "string literal") \
    \
    /* start of template literal with substitutions */ \
    macro(TEMPLATE_HEAD,    "'${'") \
    /* template literal without substitutions */ \
    macro(NO_SUBS_TEMPLATE, "template literal") \
    \
    macro(REGEXP,       "regular expression literal") \
    tagged(RESERVED_WORD_LITERAL_TAG, 1 << 7, "reserved word tag")      \
    tagged(TRUE,  0 | RESERVED_WORD_LITERAL_TAG,       "boolean literal 'true'") \
    tagged(FALSE, 1 | RESERVED_WORD_LITERAL_TAG,       "boolean literal 'false'") \
    tagged(NULL, 2 | RESERVED_WORD_LITERAL_TAG,        "null literal")   \
    \
    tagged(KEYWORD_TAG, 1 << 8, "keyword") \
    tagged(THIS, 0 | KEYWORD_TAG,        "keyword 'this'")      \
    tagged(FUNCTION, 1 | KEYWORD_TAG,    "keyword 'function'") \
    tagged(IF,       2 | KEYWORD_TAG,    "keyword 'if'") \
    tagged(ELSE,     3 | KEYWORD_TAG,    "keyword 'else'") \
    tagged(SWITCH,   4 | KEYWORD_TAG,    "keyword 'switch'") \
    tagged(CASE,     5 | KEYWORD_TAG,    "keyword 'case'") \
    tagged(DEFAULT,  6 | KEYWORD_TAG,    "keyword 'default'") \
    tagged(WHILE,    7 | KEYWORD_TAG,    "keyword 'while'") \
    tagged(DO,       8 | KEYWORD_TAG,    "keyword 'do'") \
    tagged(FOR,      9 | KEYWORD_TAG,    "keyword 'for'") \
    tagged(BREAK,    10 | KEYWORD_TAG,    "keyword 'break'") \
    tagged(CONTINUE, 11 | KEYWORD_TAG,    "keyword 'continue'") \
    tagged(VAR,      12 | KEYWORD_TAG,    "keyword 'var'") \
    tagged(CONST,    13 | KEYWORD_TAG,    "keyword 'const'") \
    tagged(WITH,     14 | KEYWORD_TAG,    "keyword 'with'") \
    tagged(RETURN,   15 | KEYWORD_TAG,    "keyword 'return'") \
    tagged(NEW,      16 | KEYWORD_TAG,    "keyword 'new'") \
    tagged(DELETE,   17 | KEYWORD_TAG,    "keyword 'delete'") \
    tagged(TRY,      18 | KEYWORD_TAG,    "keyword 'try'") \
    tagged(CATCH,    19 | KEYWORD_TAG,    "keyword 'catch'") \
    tagged(FINALLY,  20 | KEYWORD_TAG,    "keyword 'finally'") \
    tagged(THROW,    21 | KEYWORD_TAG,    "keyword 'throw'") \
    tagged(DEBUGGER, 22 | KEYWORD_TAG,    "keyword 'debugger'") \
    tagged(EXPORT,   23 | KEYWORD_TAG,    "keyword 'export'") \
    tagged(IMPORT,   24 | KEYWORD_TAG,    "keyword 'import'") \
    tagged(CLASS,    25 | KEYWORD_TAG,    "keyword 'class'") \
    tagged(EXTENDS,  26 | KEYWORD_TAG,    "keyword 'extends'") \
    tagged(SUPER,    27 | KEYWORD_TAG,    "keyword 'super'") \
    \
    /* contextual keywords */ \
    tagged(CONTEXTUAL_KEYWORD_TAG, 1 << 9, "contextual keyword") \
    tagged(AS,    0 | CONTEXTUAL_KEYWORD_TAG,      "'as'") \
    tagged(ASYNC, 1 | CONTEXTUAL_KEYWORD_TAG,      "'async'") \
    tagged(AWAIT, 2 | CONTEXTUAL_KEYWORD_TAG,      "'await'") \
    tagged(EACH,  3 | CONTEXTUAL_KEYWORD_TAG,      "'each'") \
    tagged(FROM,  4 | CONTEXTUAL_KEYWORD_TAG,      "'from'") \
    tagged(GET,   5 | CONTEXTUAL_KEYWORD_TAG,      "'get'") \
    tagged(LET,   6 | CONTEXTUAL_KEYWORD_TAG,      "'let'") \
    tagged(OF,    7 | CONTEXTUAL_KEYWORD_TAG,      "'of'") \
    tagged(SET,   8 | CONTEXTUAL_KEYWORD_TAG,      "'set'") \
    tagged(STATIC, 9 | CONTEXTUAL_KEYWORD_TAG,      "'static'") \
    tagged(TARGET, 10 | CONTEXTUAL_KEYWORD_TAG,      "'target'") \
    tagged(YIELD,  11 | CONTEXTUAL_KEYWORD_TAG,      "'yield'") \
    \
    /* future reserved words */ \
    tagged(FUTURE_RESERVED_KEYWORD_TAG, 1 << 10, "reserved word") \
    tagged(ENUM, 0 | FUTURE_RESERVED_KEYWORD_TAG,        "reserved word 'enum'") \
    \
    /* reserved words in strict mode */ \
    tagged(STRICT_RESERVED_KEYWORD_TAG, 1 << 11, "reserved word") \
    tagged(IMPLEMENTS,  0 | STRICT_RESERVED_KEYWORD_TAG, "reserved word 'implements'") \
    tagged(INTERFACE,   1 | STRICT_RESERVED_KEYWORD_TAG, "reserved word 'interface'") \
    tagged(PACKAGE,     2 | STRICT_RESERVED_KEYWORD_TAG, "reserved word 'package'") \
    tagged(PRIVATE,     3 | STRICT_RESERVED_KEYWORD_TAG, "reserved word 'private'") \
    tagged(PROTECTED,   4 | STRICT_RESERVED_KEYWORD_TAG, "reserved word 'protected'") \
    tagged(PUBLIC,      5 | STRICT_RESERVED_KEYWORD_TAG, "reserved word 'public'") \
    \
    /* \
     * The following token types occupy contiguous ranges to enable easy \
     * range-testing. \
     */ \
    /* \
     * Binary operators tokens, TOK_OR thru TOK_POW. These must be in the same \
     * order as F(OR) and friends in FOR_EACH_PARSE_NODE_KIND in ParseNode.h. \
     */ \
    tagged(BINOP_TAG, 1 << 12, "binop") \
    tagged(OR,     0 | BINOP_TAG,      "'||'")   /* logical or */ \
    tagged(AND,    1 | BINOP_TAG,      "'&&'")   /* logical and */ \
    tagged(BITOR,  2 | BINOP_TAG,      "'|'")    /* bitwise-or */ \
    tagged(BITXOR, 3 | BINOP_TAG,      "'^'")    /* bitwise-xor */ \
    tagged(BITAND, 4 | BINOP_TAG,      "'&'")    /* bitwise-and */ \
    tagged(STRICTEQ, 5 | BINOP_TAG,   "'==='") \
    tagged(EQ,       6 | BINOP_TAG,   "'=='") \
    tagged(STRICTNE, 7 | BINOP_TAG,   "'!=='") \
    tagged(NE,       8 | BINOP_TAG,   "'!='") \
    tagged(LT,       9 | BINOP_TAG,   "'<'") \
    tagged(LE,       10 | BINOP_TAG,   "'<='") \
    tagged(GT,       11 | BINOP_TAG,   "'>'") \
    tagged(GE,       12 | BINOP_TAG,   "'>='") \
    \
    tagged(INSTANCEOF,  13 | BINOP_TAG | KEYWORD_TAG, "keyword 'instanceof'") \
    tagged(IN,          14 | BINOP_TAG | KEYWORD_TAG, "keyword 'in'") \
    \
    tagged(LSH,     15 | BINOP_TAG,    "'<<'") \
    tagged(RSH,     16 | BINOP_TAG,    "'>>'") \
    tagged(URSH,    17 | BINOP_TAG,    "'>>>'") \
    tagged(ADD,     18 | BINOP_TAG,    "'+'") \
    tagged(SUB,     19 | BINOP_TAG,    "'-'") \
    tagged(MUL,     20 | BINOP_TAG,    "'*'") \
    tagged(DIV,     21 | BINOP_TAG,    "'/'") \
    tagged(MOD,     22 | BINOP_TAG,    "'%'") \
    tagged(POW,     23 | BINOP_TAG,    "'**'") \
    \
    /* Unary operation tokens. */ \
    tagged(UNOP_TAG, 1 << 13, "unop") \
    tagged(TYPEOF,   0 | UNOP_TAG | KEYWORD_TAG,    "keyword 'typeof'") \
    tagged(VOID,     1 | UNOP_TAG | KEYWORD_TAG,    "keyword 'void'") \
    tagged(NOT,      2 | UNOP_TAG,    "'!'") \
    tagged(BITNOT,   3 | UNOP_TAG,    "'~'") \
    \
    /* Assignment ops, per TokenKindIsAssignment */ \
    tagged(ASSIGNMENT_TAG, 1 << 14, "assignment") \
    tagged(ASSIGN,       0 | ASSIGNMENT_TAG, "'='")     \
    tagged(ADDASSIGN,    1 | ASSIGNMENT_TAG, "'+='") \
    tagged(SUBASSIGN,    2 | ASSIGNMENT_TAG, "'-='") \
    tagged(BITORASSIGN,  3 | ASSIGNMENT_TAG, "'|='") \
    tagged(BITXORASSIGN, 4 | ASSIGNMENT_TAG, "'^='") \
    tagged(BITANDASSIGN, 5 | ASSIGNMENT_TAG, "'&='") \
    tagged(LSHASSIGN,    6 | ASSIGNMENT_TAG, "'<<='") \
    tagged(RSHASSIGN,    7 | ASSIGNMENT_TAG, "'>>='") \
    tagged(URSHASSIGN,   8 | ASSIGNMENT_TAG, "'>>>='") \
    tagged(MULASSIGN,    9 | ASSIGNMENT_TAG, "'*='") \
    tagged(DIVASSIGN,    10 | ASSIGNMENT_TAG, "'/='") \
    tagged(MODASSIGN,    11 | ASSIGNMENT_TAG, "'%='") \
    tagged(POWASSIGN,    12 | ASSIGNMENT_TAG, "'**='")

namespace js {
namespace frontend {

// Values of this type are used to index into arrays such as isExprEnding[],
// so the first value must be zero.
enum TokenKind {
#define EMIT_ENUM(name, desc) TOK_##name,
#define EMIT_ENUM_TAGGED(name, value, desc) TOK_##name = TOK_##value,
    FOR_EACH_TOKEN_KIND_WITH_TAGGED(EMIT_ENUM, EMIT_ENUM_TAGGED)
#undef EMIT_ENUM
#undef EMIT_ENUM_TAGGED
    TOK_LIMIT                      // domain size
};

inline bool
TokenKindIsBinaryOp(TokenKind tt)
{
    return tt & TOK_BINOP_TAG;
}

inline bool
TokenKindIsAssignment(TokenKind tt)
{
    return tt & TOK_ASSIGNMENT_TAG;
}

inline MOZ_MUST_USE bool
TokenKindIsKeyword(TokenKind tt)
{
    return tt & TOK_KEYWORD_TAG;
}

inline MOZ_MUST_USE bool
TokenKindIsContextualKeyword(TokenKind tt)
{
    return tt & TOK_CONTEXTUAL_KEYWORD_TAG:
}

inline MOZ_MUST_USE bool
TokenKindIsFutureReservedWord(TokenKind tt)
{
    return tt & TOK_FUTURE_RESERVED_KEYWORD_TAG;
}

inline MOZ_MUST_USE bool
TokenKindIsStrictReservedWord(TokenKind tt)
{
    return tt & TOK_STRICT_RESERVED_KEYWORD_TAG;
}

inline MOZ_MUST_USE bool
TokenKindIsReservedWordLiteral(TokenKind tt)
{
    return tt & TOK_RESERVED_WORD_LITERAL_TAG;
}

inline MOZ_MUST_USE bool
TokenKindIsReservedWord(TokenKind tt)
{
    return tt & (TOK_KEYWORD_TAG |
                 TOK_FUTURE_RESERVED_KEYWORD_TAG |
                 TOK_RESERVED_WORD_LITERAL_TAG);
}

inline MOZ_MUST_USE bool
TokenKindIsPossibleIdentifier(TokenKind tt)
{
    return tt == TOK_NAME ||
           (tt & (TOK_CONTEXTUAL_KEYWORD_TAG | TOK_STRICT_RESERVED_KEYWORD_TAG));
}

inline MOZ_MUST_USE bool
TokenKindIsPossibleIdentifierName(TokenKind tt)
{
    return tt == TOK_NAME ||
           (tt & (TOK_KEYWORD_TAG |
                  TOK_FUTURE_RESERVED_KEYWORD_TAG |
                  TOK_RESERVED_WORD_LITERAL_TAG |
                  TOK_CONTEXTUAL_KEYWORD_TAG |
                  TOK_STRICT_RESERVED_KEYWORD_TAG));
}

} // namespace frontend
} // namespace js

#endif /* frontend_TokenKind_h */
