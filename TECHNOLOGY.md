# Backus-Naur Form (BNF)

In computer science, BNF (Backus Normal Form or Backus-Naur Form) is one of the two main notation techniques for context-free grammars, often used to describe the syntax of languages used in computing, such as computer programming languages, document formats, instruction sets and communication protocols.

## Introduction

A BNF specification is a set of derivation rules, written as

```
<symbol> ::= __expression__
```

where `<symbol>` is a *nonterminal*, and the `__expression__` consists of one or more sequences of symbols; more sequences are separated by the pipe `|`, indicating a choice, the whole being a possible substitution for the symbol on the left.

## Example

As an example, consider this possible BNF for a U.S. postal address:

```
 <postal-address> ::= <name-part> <street-address> <zip-part>
      <name-part> ::= <personal-part> <last-name> <opt-suffix-part> <EOL>
                    | <personal-part> <name-part>
  <personal-part> ::= <initial> "." | <first-name>
 <street-address> ::= <house-num> <street-name> <opt-apt-num> <EOL>
       <zip-part> ::= <town-name> "," <state-code> <ZIP-code> <EOL>
<opt-suffix-part> ::= "Sr." | "Jr." | <roman-numeral> | ""
    <opt-apt-num> ::= <apt-num> | ""
```

This translates into English as:

* A postal address consists of a name-part, followed by a street-address part, followed by a zip-code part
* A name-part consists of either: a personal-part followed by a last name followed by an optional suffix (Jr., Sr., or dynastic number) and end-of-line, or a personal part followed by a name part (this rule illustrates the use of recursion in BNFs, covering the case of people who use multiple first and middle names and/or initials).
* A personal-part consists of either a first name or an initial followed by a dot.
* A street address consists of a house number, followed by a street name, followed by an optional apartment specifier, followed by an end-of-line.
* A zip-part consists of a town-name, followed by a comma, followed by a state code, followed by a ZIP-code followed by an end-of-line.
* A opt-suffix-part consists of a suffix, such as "Sr.", "Jr." or a roman-numeral, or an empty string (i.e. nothing).
* A opt-apt-num consists of an apartment number or an empty string (i.e. nothing).

## Extended Backus-Naur Form (EBNF)

### Basics

EBNF is a code that expresses the grammar of a formal language. An EBNF consists of terminal symbols and non-terminal production rules which are the restrictions governing how terminal symbols can be combined into a legal sequence. Examples of terminal symbols include alphanumeric characters, punctuation marks, and whitespace characters.

The EBNF defines production rules where sequences of symbols are respectively assigned to a nonterminal:

```
digit excluding zero = "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" ;
digit                = "0" | digit excluding zero ;
```

This production rule defines the nonterminal *digit* which is on the left side of the assignment. The vertical bar represents an alternative and the terminal symbols are enclosed with quotation marks followed by a semicolon as terminating character. Hense a *digit* is a *0* or a *digit excluding zero* that can be *1* or *2* or *3* and so forth until *9*.

A production rule can also include a sequence of terminals or nonterminals, each separated by a comma:

```
twelve                          = "1", "2" ;
two hundred one                 = "2", "0", "1" ;
three hundred twelche           = "3", twelve ;
twelve thousand two hundred one = twelve, two hundred one ;
```

Expressions that may be omitted or repeated can be represented through curly braces `{ ... }`.

```
natural number = digit excluding zero, { digit } ;
```

In this case, the strings *1*, *2*, ..., *10*, ..., *12345*, ... are correct expressions. To represent this, everything that is set within the curly braces may be repeated arbitrarily often, including not at all.

An option can be represented through squared brackets `[ ... ]`. That is, everything that is set within the square brackets may be present just once, or not at all:

```
integer = "0" | [ "-" ], natural number ;
```

Therefore an integer is a zero (*0*) or a natural number that may be preceded by an optional minus sign.

EBNF also provides, among other things, the syntax: to describe repetitions (of a specified number of times), to exclude some part of a production, and to insert comments in an EBNF grammar.

Usage | Notation
--- | ---
definition | =
concatenation | ,
termination | ;
alternation | \|
optional | [ ... ]
repetition | { ... }
grouping | ( ... )
terminal string | " ... "
terminal string | ' ... '
comment | (* ... *)
special sequence | ? ... ?
exception | -

A Pascal-like programming language that allows only assignments can be defined in EBNF as follows:

```
(* a simple program syntax in EBNF *)
program = 'PROGRAM', white space, identifier, white space,
          'BEGIN', white space,
          { assignment, ";", white space },
          'END.' ;
identifier = alphabetic character, { alphabetic character | digit } ;
number = [ "-" ], digit, { digit } ;
string = '"', { all characters - '"' }, '"' ;
assignment = identifier, ":=", ( number | identifier | string ) ;
alphabetic character = "A" | "B" | "C" | "D" | "E" | "F" | "G"
                     | "H" | "I" | "J" | "K" | "L" | "M" | "N"
                     | "O" | "P" | "Q" | "R" | "S" | "T" | "U"
                     | "V" | "W" | "X" | "Y" | "Z" ;
digit = "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" ;
white space = ? white space characters ? ;
all characters = ? all visible characters ? ;
```

A syntactically correct program then would be:

```
PROGRAM DEMO1
BEGIN
	A:=3;
	B:=45;
	H:=-100023;
	C:=A;
	D123:=B34A;
	BABOON:=GIRAFFE;
	TEXT:="Hello world!";
END.
```

### Advantages over BNF

Any grammar defined in EBNF can also be represented in BNF, though representations in the latter are generally lengthier. E.g., options and repitions cannot be directly expressed in BNF and require the use of an intermediate rule or alternative production defined to be either nothing or the optional production for option, or either the repeated production of itself, recursively, for repetition. The same constructs can still be used in EBNF.

The BNF uses the symbols (<, >, |, ::=) for itself, but does not include quotes around terminal strings. This prevents these characters from being used in the languages, and requires a special symbol for the empty string. In EBNF, terminals are strictly enclosed within quotation marks (" or '). The angle brackets (<, >) for nonterminals can be omitted.

BNF syntax can only represent a rule in one line, whereas in EBNF a terminating character, the semicolon, marks the end of a rule.

Furthermore, EBNF includes mechanisms for enhancements, defining the number of repetitions, excluding alternatives, comments, etc.

# Context-free grammars

## LL grammar

Eine kontextfreie Grammatik heißt LL(k)-Grammatik für eine natürliche Zahl k, wenn jeder Ableitungsschritt eindeutig durch die nächsten k Symbole der Eingabe (Lookahead) bestimmt ist. Das bedeutet, die Frage, welches Nichtterminalsymbol mit welcher Regel als Nächstes expandiert werden soll, kann eindeutig mit Hilfe der nächsten k Symbole der Eingabe bestimmt werden.

## LR grammar

Man nennt eine kontextfreie Grammatik LR(k)-Grammatik, wenn jeder Reduktionsschritt eindeutig durch k Symbole der Eingabe (Lookahead) bestimmt ist. Das bedeutet, die Frage, zu welchem Nichtterminalsymbol mit welcher Regel als nächstes reduziert werden soll, kann eindeutig mit Hilfe der nächsten k Symbole der Eingabe bestimmt werden.

## LALR parser

In einfachen Worten bedeutet das, dass im zuvor berechneten LR(1)-Automaten Zustände zusammengeführt werden, deren Kern identisch ist. Der Kern zweier Zustände ist identisch, falls die Items der beiden Zustände bis auf die Follow-Mengen (Lookaheads) identisch sind.

## Simple LR parser

In computer science, a Simple LR or SLR parser is a type of LR parser with small parse tables and a relatively simple parser generator algorithm. As with other types of LR(1) parser, an SLR parser is quite efficient at finding the single correct bottom-up parse in a single left-to-right scan over the input stream, without guesswork or backtracking. The parser is mechanically generated from a formal grammar for the language.

# Bison

[http://dinosaur.compilertools.net/bison/](http://dinosaur.compilertools.net/bison/)

## Languages and Context-Free Grammars

In order for Bison to parse a language, it must be described by a **context-free grammar**. This means that you specify one or more **syntactic groupings** and give rules for constructing them from their parts. For example, in the C language, one kind of grouping is called an *expression*. One rule for making an expression might be „An expression can be made of a minus sign and another expression“. Another would be „An expression can be an integer“. As you can see, rules are often recursive, but there must be at least one rule which leads out of the recursion.

The most common formal system for presenting such rules for humans to read is **Backus-Naur Form** or *BNF*, which was developed in order to specify the language Algol 60. Any grammar expressed in BNF is a context-free grammar. The input to Bison is essentially machine-readable BNF.

Not all context-free languages can be handled by Bison, only those that are LALR(1). In brief, this means that it must be possible to tell how to parse any portion of an input string with just a single token of look-ahead. Strictly speaking, that is a description of an LR(1) grammar, and LALR(1) involves additional restrictions that are hard to explain simply; but it is rare in actual practice to find an LR(1) grammar that fails to be LALR(1).

In the formal grammatical rules for a language, each kind of syntactic unit or grouping is named by a **symbol**. Those which are built by grouping smaller constructs according to grammatical rules are called **nonterminal symbols**; thos which can’t be subdivided are called **terminal symbols** or **token types**. We call a piece of input corresponding to a single terminal symbol a **token**, and a piece corresponding to a single nonterminal symbol a **grouping**.

We can use the C language as an example of what symbols, terminal and nonterminal, mean. The tokens of C are identifiers, constants (numeric and string), and the various keywords, arithmetic operators and punctuation marks. So the terminal symbols of a grammar for C include *identifier*, *number*, *string*, plus one symbol for each keyword, operator or punctuation mark: *if*, *return*, *const*, *static*, *int*, *char*, *plus-sign*, *open-brace*, *close-brace*, *comma* and many more.

Here is a simple C function subdivided into tokens:

```c
int                /* keyword `int` */
square (x)         /* identifier, open-paren, identifier, close-paren */
    int x;         /* keyword `int`, identifier, semicolon */
{                  /* open-brace */
    return x * x;  /* keyword `return`, identifier, asterisk, identifier, semicolon */
}                  /* close-brace */
```

The syntactic groupings of C include the expression, the statement, the declaration, and the function definition. These are represented in the grammar of C by nonterminal symbols *expression*, *statement*, *declaration* and *function definition*. The full grammar uses dozens of additional language constructs, each with its own nonterminal symbol, in order to express the meanings of these four. The example above is a function definition; it contains one declaration, and one statement. In the statement, each *x* is an expression and so is *x * x*.

Each nonterminal symbol must have grammatical rules showing how it is made out of simpler constructs. For example, one kind of C statement is the `return` statement; this would be described with a grammar rule which reads informally as follows: A *statement* can be made of a `return` keyword, an *expression* and a *semicolon*. There would be many other rules for *statement*, one for each kind of statement in C.

One nonterminal symbol must be distinguished as the special one which defines a complete utterance in the language. It is called the *start symbol*. In a compiler, this means a complete input program. In the C language, the nonterminal symbol *sequence of definitions and declarations* plays this role.

For example `1 + 2` is a valid C expression (a valid part of a C program) but it is not valid as an entire C program. In the context-free grammar of C, this follows from the fact that *expression* is not the start symbol.

The Bison parser reads a sequence of tokens as its input, and groups the tokens using the grammar rules. If the input is valid, the end result is that the entire token sequence reduces to a single grouping whose symbol is the grammar’s start symbol. If we use a grammar for C, the entire input must be a `sequence of definitions and declarations`. If not, the parser reports a syntax error.

## From Formal Rules to Bison Input

A formal grammar is a mathematical construct. To define the language for Bison, you must write a file expressing the grammar in Bison syntax: a **Bison grammar** file.

A nonterminal symbol in the formal grammar is represented in Bison input as an identifier, like an identifier in C. By convention, it should be in lower case, such as `expr`, `stmt` or `declaration`.

The Bison reprensentation for a terminal symbol is also called a **token type**. Toke types as well can be represented as C-like identifiers. By convention, these identifiers should be upper case to distinguish them from nonterminals: for example, `INTEGER`, `IDENTIFIER`, `IF` or `RETURN`. A terminal symbol that stands for a particular keyword in the language should be named after that keyword converted to upper case. The terminal symbol `error` is reserved for error recovery.

A terminal symbol can be represented as a character literal, just like a C character constant. You should do this whenever a token is just a single character (parenthesis, plus-sign, etc.): use that same character in a literal as the terminal symbol for that token.

A third way to represent a terminal symbol is with a C string constant containing several characters.

The grammar rules also have an expression in Bison syntax. For example, here is the Bison rule for a C `return` statement. The semicolon in quotes is a literal character token, representing part of the C syntax for the statement; the naked semicolon, and the colon, are Bison punctuation used in every rule.

```
stmt:    RETURN expr ';'
         ;
```

## Semantic Values

## Semantic Actions 

# Jison

[https://github.com/zaach/jison](https://github.com/zaach/jison)

## The Concepts of Jison

Until the Bison guide is properly ported for Jison, you can refer to it for the major concepts, which are equivalent (except for the bits about static typing of semantic values, and other obvious C artefacts).

## Specifying a Language

The process of parsing a language commonly involves two phases: **lexical analysis** (tokenizing) and **parsing**, which the Lex/Yacc and Flex/Bison combinations are famous for. Juson lets you specify a parser much like you would using Bison/Flex, with separate files for tokenization ules and for the language grammar, or with the tokenization rules embedded in the main grammar.

For example, here is the grammar for the calculator parser:

```
/* description: Parses and executes mathematical expressions. */

/* lexical grammar */
%lex

%%
\s+                    /* skip whitespace */
[0-9]+("."[0-9]+)?\b   return 'NUMBER';
"*"                    return '*'
"/"                    return '/';
"-"                    return '-';
"+"                    return '+';
"^"                    return '^';
"("                    return '(';
")"                    return ')';
"PI"                   return 'PI';
"E"                    return 'E';
<<EOF>>                return 'EOF';

/lex

/* operator associations and precedence */

%left '+' '-'
%left '*' '/'
%left '^'
%left UMINUS

%start expressions

%% /* language grammar */

expressions
    : e EOF
        {print($1); return $1;}
    ;

e
    : e '+' e
        {$$ = $1+$3;}
    : e '-' e
        {$$ = $1-$3;}
    : e '*' e
        {$$ = $1*$3;}
    : e '/' e
        {$$ = $1/$3;}
    : e '^' e
        {$$ = Math.pow($1, $3);}
    : '-' e %prec UMINUS
        {$$ = -$2;}
    : '(' e ')'
        {$$ = $2;}
    : NUMBER
        {$$ = Number(yytext);}
    : E
        {$$ = Math.E;}
    : PI
        {$$ = Math.PI;}
    ;
```

which compiles down to this JSON representation used directly by Jison:

```
{
	"lex": {
		"rules": [
			["\\s+", "/* skip whitespace */"],
			["[0-9]+("."[0-9]+)?\b", "return 'NUMBER';"],
			["\\*", "return '*';"],
			["\\/", "return '/';"],
			["-", "return '-';"],
			["\\+", "return '+';"],
			["\\^", "return '^';"],
			["\\(", "return '(';"],
			["\\)", "return ')';"],
			["PI\\b", "return 'PI';"],
			["E\\b", "return 'E';"],
			["$", "return 'EOF';"]
		]
	},
	"operators": {
		["left", "+", "-"],
		["left", "*", "/"],
		["left", "^"],
		["left", "UMINUS"]
	},
	"bnf": {
		"expressions": [["e EOF", "print($1); return $1;"]],
		"e": [
			["e + e", "$$ = $1 + $3;"],
			["e - e", "$$ = $1 - $3;"],
			["e * e", "$$ = $1 * $3;"],
			["e / e", "$$ = $1 / $3;"],
			["e ^ e", "$$ = Math.pow($1, $3);"],
			["- e", "$$ = -$2;", {"prec": "UMINUS"}],
			["( e )", "$$ = Number(yytext);"],
			["E", "$$ = Math.E;"],
			["PI", "$$ = Math.PI;"]
		]
	}
}
```

Jison accepts both the Bison/Flex style format, or the raw JSON format, e.g.

```bash
$ node bin/jison examples/calculator.jison
```

or

```bash
$ node bin/jison examples/calculator.json
```

When the lexical grammar resides in its own (.jisonlex) file, use that as the second argument to Jison, e.g.:

```bash
node bin/jison examples/classy.jison examples/classy.jisonlex
```

## Lexical Analysis

Jison includes a rather rudimentary scanner generator, though **any module that supports the basic scanner API could be used** in its place.

The format of the input file (including macro support) and the style of the pattern matchers are modeled after Flex. Several metacharacters have been added, but there is also one minor inconvenience compared to Flex patterns, namely exact string patterns must be placed in quotes e.g.:

```
// Bad
[0-9]+zomg   print(yytext);

// Good
[0-9]+"zomg"   print(yytext);

// Actions that span multiple lines should be surrounded by braces:
[0-9]+"zomg"   %{ print(yytext);
                  return 'ZOMG'; %}
```

A recently added feature are start conditions, which allow certain rules to only match in certain states. If the lexer is not in that state, then the rule is ignored. The lexer starts in the `INITIAL` state, but can move to new states specified by you. An example below shows where Jison differs, namely `this.begin('state')` instead of `BEGIN(STATE)` for changing states within an action:

```
%s expect

%%
expect-floats      this.begin('expect');

<expect>[0-9]+"."[0-9]+ {
    console.log("found a float, = " + yytext);
}
<expect>\n %{
    /* that's the end of the line, so
     * we need another "expect-number"
     * before we'll recognize any more
     * numbers
     */
    this.begin('INITIAL');
%}

[0-9]+             console.log("found an integer, = " + yytext);

"."                console.log("found a dot");
```

If you use `%x` instead of `%s` to declare your start condition then *only* rules that match the current start condition will be considered.

Consider the following example of a scanner that simply scans all double-quote delimited strings in a text file but disallows newlines inside quotations:

```
%x string

%%
["]                this.begin("string");
<string>[^"\n]*    return "STRING";
<string>[\n]       return "NEWLINE_IN_STRING";
<string><<EOF>>    return "EOF_IN_STRING";
<string>["]        this.popState();

[.\n]+             /* skip over text not in quotes */
<<EOF>>            return "EOF";
```

Additionally, use `this.popState()` within an action to revert to the previous state.

Using the JSON format, start conditions are defined with an array before the rule’s matcher:

```
{
	"rules": [
		[["expect"], "[0-9]+\".\"[0-9]+", "console.log('found a float, = ' + yytext);"]
	]
}	
```

## Tracking Locations

Jison’s lexical analyzer will track line number and column number information for each token and make them available within parser actions. The API is identical to Bison’s.

## Custom Scanners

You don’t have to use the builtin Jison lexical scanner. An object with a `lex` and a `setInput` function would suffice, e.g.:

```
parser.lexer = {
	lex: function() {
		return 'NIL';
	},
	setInput: function(str) {
	}
};	
```

This lexer would simply return `NIL` tokens *ad infinitum*.

The following example demonstrates a scanner that looks for upper and lower case letters, ignoring all whitespace.

```
// myscanner.js

function AlphabetScanner() {

	var text = '';	

	this.yytext = '';
	
	this.yyloc = {
		first_column: 0,
		first_line: 1,
		last_line: 1,
		last_column: 0
	};
	
	this.yylloc = this.yyloc;

	this.setInput = function(text_) {
		text = text_;
	};
	
	this.lex = function() {
		
		// Return the EOF token when we run out of text.
		if (text === '') {
			return 'EOF';
		}
		
		// Consume a single character and increment our column numbers.
		var c = text.charAt(0);
		text = text.substring(1);
		
		this.yytext = c;
		this.yyloc.first_column++;
		this.yyloc.last_column++;
		
		if (c === '\n') {
			
			// Increment our line number when we hit newlines.
			this.yyloc.first_line++;
			this.yyloc.last_line++;
			
			// Try to keep lexing because we aren’t interested in newlines.
			return this.lex();
			
		} else if (/[a-z]/.test(c)) {
			return 'LOWER_CASE';
		} else if (/[A-Z]/.test(c)) {
			return 'UPPER_CASE';
		} else if (/\s/.test(c)) {
			
			// Try to keep lexing because we aren’t interested in whitespace.
			return this.lex();
			
		} else {
			return 'INVALID';
		}
		
	};
	
}

parser.lexer = new AlphabetScanner();
```

## Sharing Scope

In Bison, code is expected to be lexically defined within the scope of the semantic actions. E.g., chunks of code may be included in the generated parser source, which are available from semantic actions.

Jison supports inline code blocks like Bison, but also exposes state that can be accessed from other modules. Instead of pulling code into the generated module, the generated module can be required and used by other modules. The parser has a `yy` property which is exposed to actions as the `yy` free variable. Any functionality attached to this property is available in both lexical and semantic actions through the `yy` free variable.

An example from `orderly.js`:

```
var parser = require('./orderly/parse').parser;

// set parser's shared scope
parser.yy = require('./orderly/scope');

// returns the JSON object
var parse = exports.parse = function(input) {
	return parser.parse(input);
};
```

The `scope` module contains logic for building data structures, which is used within the semantic actions.

## Parsing algorithms

Like Bison, Jison can recognize languages described by LALR(1) grammars, though it also has modes for LR(0), SLR(1), and LR(1). It also has a special mode for generating LL(1) parse tables and could be extended to generate a recursive descent parser for LL(k) languages in the future.

--

# Jacob

[https://github.com/Canna71/Jacob](https://github.com/Canna71/Jacob)

Possibly Acronym for: JAvascript COmpiler Bison-like or maybe even Just Another Compiler to OBjects

Jacob is a tool like Flex and Bison to generate interpreters or compilers. This can be used for example to create a DSL (domain specific language) to be used in NodeJS or in the browser.

Generating a language interpreter (or compiler) involves two steps:

1. aggregate the input characters into a series of *tokens*: this is done by module called *lexer*
2. interpreting the series of tokens as a language, according to a grammar: this is done by a module called *parser*

Also, you will define an actual behaviour which is the semantic of the language, that is, what the program should do according to a language statement.

Given appropriate instructions, Jacob will generate both the lexer and the parser. We’ll see how to specify the actual behaviour of your parser.

## Usage

From a command line to generate the lexer use the following command line:

```bash
jacob -t tokens.jacoblex [-l lexer.js]
```

The `-t` argument specify the token specifications file, the optional `-l` parameter specify the name of the generated file. For the token specification file extension you can use whatever extension you want (here `.jacoblex` is used) except `.js` since `.js` file be interpreted as javascript modules containing the internal representation of the tokens. You could also use this instead of the lexer language descripted later.

Analogously to generate the parser you would use:

```bash
jacob -g grammar.jacobgram [-p parser.js]
```

Usually you’ll generate both modules with just one invocation:

```bash
jacob -t tokens.jacoblex [-l lexer.js] -g grammar.jacobgram [-p parser.js]
```

## Lexer

In order for Jacob to create a lexer you have to provide it with a `.jacoblex` file which looks like the following:

```
%moduleName MyLexer

%%

digits = [0-9]

%%
<>{digits}*\.{digits}+ {
    this.jjval = parseFloat(this.jjtext);
    return 'float';
}

<>{digits}+ {
    this.jjval = parseInt(this.jjtext);
    return 'integer';
}

<>print {
    return 'print';
}

<>\w+ {
    return 'id';
}

<>\s* { }

<>. {
    return this.jjtext;
}

<>$ {
    console.log('EOF'); return 'EOF';
}
```

The syntax is similar to Flex’s, with some differences. The file is split in three areas, separated by a double percent. In the first area are the directives. The only currently supported is `%moduleName`, which allow you to specify the name of the generated module.

The second section contains definitions, that allow you to assign names to regular expressions.

The third section contains the actual rules. In order to recognize a token, you specify the regular expression that matches it, and then assign it an action.

Take for example the following: `<>\w+ { return 'id'; }` The double angled brakets are used to specify (optional) starting state of the rule (more on that later), in this case the rule is active in the `DEFAULT` state. The regular expression `\w+` matches one or more alphanumeric chars. The associated action (between curly braces) is a javascript function that should return the name of the matched token. This name is the name that can be used in the grammar file.

### Regular Expressions Syntax

Jacob implements most, if not all, the regular expressions mechanism found in most lexer, including forward lookahead.

Here is the summary:

Pattern | Description
--- | ---
`x` | matches character *x*
`.` | matches any character except newline
`[xyz]` | this is a character class: it matches either *x*, *y* or *z*
`[a-f]` | character class with range: it matches every character from *a* to *f* included
`[^a-f]` | range negation: matches everything BUT *a* to *f* included
`r*` | matches 0 or more times the regular expression r
`r+` | matches 1 or more times the regular expression r
`r?` | matches 0 or 1 time the regular expression r
`r{2-5}` | matches from 2 to 5 times the regular expression r
`r{2,}` | matches 2 or more times the regular expression r
`r{,5}` | matches from 0 to 5 times the regular expression r
`r{4}` | matches the regular expression r exactly 4 times
`{digits}` | matched the definition named *digits*
`\X` | *\\* is the escape character, can be used to insert characters like *\\n*, *\\r*, *\\t* or to escape regex special characters like *\**
`\x2a` | matches character with hex code 2a
`\u2103` | matches unicode character U+2103
`rs` | matches the regular expression r followed by the regular expression s
`r|s` | matches the regular expression r or s
`r/s` | lookahead: matches r only if it is followed by s
`^r` | matches the regular expression r only at the beginning of a line
`r$` | matches the regular expression r only at the end of a line
`ab(cd)*` | matches ab, abcd, abcdcd, abcdcdcdcd etc.

### Lexer Actions

In the actions you should specify what the lexer should do after recognizing a token. The simplest action is the empty one: `<>\s* { }`.

This is useful to ignore a given input. Since the action won’t return any token name, the lexer will continue processing the input without outputting any token for the matched content, thus in fact ignoring that input. In the example above the whitespace is ignored.

Another common situation is having to parse the input to have a meaningful token:

```
<>{digits}+ {
    this.jjval = parseInt(this.jjtext);
    return 'integer';
}
```

Inside actions, this points to the lexer itself. In the lexer `jjtext` contains the text the regular expression matched. `jjval` by default contains the same text as `jjtext` but you can change it inside the action. In the example above the text is parsed to get an integer value, which is then stored in `jjval`. Note that `jjval` is the value that is used in the parsing phase by your interpreter/compiler. Another powerful thing you could do inside an action is to change the lexer’s state. Take this example:

```
<>\/\* { this.pushState('BLOCKCOMMENT'); }
<BLOCKCOMMENT>\*\/ { this.popState(); }
<BLOCKCOMMENT>(\n|\r|.) { }
```

When the lexer encounters a `/*` sequence, it will enter a `BLOCKCOMMENT` state because of the action `this.pushState('BLOCKCOMMENT');`. In this state, the only active rules are the ones in which the state list (the list inside angular brackets) contains the `BLOCKCOMMENT` identifier. So while the lexer is in `BLOCKCOMMENT` state, it will ignore any character because of the rule `(\n|\r\.) { }` The only way to change the state is to encounter a `*/` sequence in which the action `this.popState();` will resume the state that was active before encountering the first `/*` sequence. The previous rules thus can be used to ignore block comments with a C-like syntax.

Here is a table of all the members of the generated lexer that are available for you inside the actions:

Member | Description
--- | ---
`jjtext` | the text matched by the regex
`jjval` | the value of the current token, by default the same as jjtext
`jjpos` | the position of the current token inside the input string
`less(n)` | this function can be called to push back `n` characters into the input stream
`isEOF()` | returns `true` if the input is at the end

Of course the generated Lexer is a JavaScript object, so you can dynamically add any member or method you need in your actions.

### Using the Lexer

After you generate a lexer, you create one using the constructor:

```
var MyLexer = require('mylexer'); // mylexer.js being the file generated by jacob
var lexer = new MyLexer();
lexer.setInput('string to be parsed');

var firstToken = lexer.nextToken();

if (lexer.isEOF(firstToken)) {
	// ...
}
```

After setting the input, you call the `nextToken()` to make the lexer read the next token. Each call to `nextToken()` will yield a new token, or a special object meaning you reached the end of the input. A non-EOF token looks like this:

```
{
	name: "integer", // this is the name given to the token by the action
	value: 12, // the action parsed the input into a number
	lexeme: "12", // the original section of the input corresponding to this token
	position: 35, // the position in the input string at which the token started
	pos: { // the position using lines and columns (useful for reporting parsing errors)
		col: 2,
		line: 7
	}
}
```

At the end of the input, the lexer will return an special object to signal that we reached the end, to test if a token is the EOF (end of file) token, use `lexer.isEOF(token)`.

Usually you don’t use the lexer by itself, of course, but you pass it over to a parser.

## Parser

In order to generate a parser you need to give Jacob the specification file containing an attributed grammar which describes the language you want to interpret/compile. Simply put, the grammar file will contain the grammar rules and the actions that the parser must execute after recognizing each rule. Jacob can generate **SLR**, **LALR** and **LR1** parser type. If not specified, Jacob will choose the most appropriate parser type given the grammar.

Here is an example of a jacob grammar file:

```
%moduleName MyParser

%left 'PLUS' '-'
%left '*' '/'

Program = { Statement } function() {};

Statement =
	'id' '=' Expression
		function(id, _, exp) { this[id] = exp; }
 	| 'print' Expression
 		function(_, exp) { console.log(exp); }
;
 		
Expression =
	Expression 'PLUS' Expression
		function(e1, _, e2) { return e1 + e2; }
	| Expression '-' Expression
		function(e1, _, e2) { return e1 - e2; }
	| Expression '*' Expression
		function(e1, _, e2) { return e1 * e2; }
	| Expression '/' Expression
		function(e1, _, e2) { return e1 / e2; }
	| 'integer'
		function(i) { return i; }
	| 'id'
		function(id) { return this[id]; }
	| '(' Expression ')'
		function(_, e) { return e; }
;
```

### Directives

At the top of the file you define directives, those can be:

Directive | Description
--- | ---
`%moduleName <name>` | Sets the name of the generated module.
`%mode SLR|LALR|LR1` | Sets the type of the generated parser. If not provided the simplest type able to parse the grammar is used.
`%left|%right token1 [token2 token3 ...]` | Sets the precedence and the associativity of an operator. The operator defined first have lower precedence. The name used for the tokens should be the ones that the lexer is returning in their actions. They could be the actual input character (es: *-*, *°*) or an actual name (es: *PLUS*). The important thing is that they match what the lexer is returning.
`%nonassoc` | Tells the parser that that token is not associative, so that it will raise an error whenever it will be used is an expression with other operator of the same precedence.

### EBNF

The actual grammar is specified in Extended Backus-Naur Form, with every rule followed by an action consisting in a javascript function.

The EBNF in the example defines rules using Nonterminal symbols (*Program*, *Statement*, *Expression*) and terminal symbols (*(*, *)*, *integer*, *\**, …). Terminal symbols are contained in single quotes and should match the name of the tokens as yielded by the lexer.

Each production can have several alternatives (separated by the pipe symbol) and each alternative can have its own action function. The action function will receive a parameter for each element of the corresponding right-hand-side part of the production.

Each rule is then terminated with a semicolon (*;*).

EBNF is more handier than BNF because it also adds shortcuts to define repitions, optionals and grouping:

Shortcut | Description
--- | ---
`{ ... }` | Means 0 or more (...)
`[ ... ]` | Means 0 or one (...)
`( ... )` | Will group the content into one group. This is useful to inline some rules that don’t need a special action for themselves.

Grouping example:

```
Assignment = Identifier ':=' ( 'integer' | Identifier | 'string' ) function(id, _, rhsvalue) { /* ... */ };
```

### Using the Parser

Like with the Lexer, you create the parser using its constructor

```
var MyParser = require('./myparser'); // myparser.js is the file generated by jacob
var parser = new MyParser();
```

To start the parsing, you call the `parse()` method, passing a lexer as the first parameter:

```
var MyLexer = require('./mylexer'); // mylexer.js is the file generated by jacob
var lexer = new MyLexer();
parser.parse(lexer);
```

What the `parse()` method do and yields depends entirely of what you put inside the grammar actions. If writing a simple expression interpreter, for example, it could yield the final result. If writing a compiler it could yield the source code or, even better, an Abstract Syntax Tree.

Usually, though, for achieving non trivial results, you must integrate the grammar actions with the outside world, through the use of an *execution context* and an *environment*.

### Execution Context

When the grammar actions are evaluated, their *this* is referring to an object that acts as an execution context. This can be used to store data, identifier tables, and so on. You can pass your own object to act as execution context in the `parse()` method:

```
var ctx = {}; // This object can be whatever you need. Grammar actions will be evaluated in this object's context

parser.parse(lexer, ctx);
// ctx will now contain whatever the grammar actions put there
```

If not provided, the parser will create an empty object to be used as execution context for the actions.

### Enviroment

Inside the actions, you might need to use other modules, for example containing the classes of your AST. To make those modules accessible to your grammar actions during parsing time, you pass an environment object to the parser constructor:

```
var MyParser = require('myparser');
var astclasses = require('astclasses');
var othermodule = require('someothermodule');

var parser = new MyParser({
	ast: astclasses,
	other: othermodule
});
```

The actions of your grammar can reach the environmental module using any of the following names: *environment*, *env*, *modules*, *imports*. For example:

```
Statement = 'id' '=' Expression function(id, _, _exp) {
	return new imports.ast.Assigment(id, exp);
}
```

# Examples

* [https://github.com/stanistan/aql-parser-js](https://github.com/stanistan/aql-parser-js)
* [https://github.com/asimihsan/tablequeryjs](https://github.com/asimihsan/tablequeryjs)