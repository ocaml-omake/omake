The TeX language is defined dynamically; it isn't context-free.
However, most documents use the LaTeX style conventions where macros
have single static definitions.

This parser doesn't look for definitions, so you have to define the
properties of macros using the table MACROS in parse/LaTeX/Macros.

    MACROS. +=
        $|\macroname| = $(Arity.new ...)
        $|envname|    = $(Arity.new ...)

The $(Arity.new ...) method takes several optional arguments.

    ~opt-arity = <i>      # Number of optional arguments (defaults to 0).
    ~opt-mask = <bitmask> # Which optional arguments to spell check (defaults to 0).
    ~arity = <i>          # Number of normal argument (defaults to 0).
    ~mask = <bitmask>     # Which arguments to spell check (defaults to all).
    ~verbatim = <bool>    # This is a verbatim environment (defaults to false).

-- Spell checking

Use the Shell alias "spell <filename>" to spell-check a LaTeX document.

The dictionaries are in:

   /usr/dict/words
   /usr/share/dict/words
   ~/.words
   .words
   .spelling

The reason for having both .words and .spelling is because the Mac OS X
dictionary in /usr/share/dict/words is really bad, so you need to get
a real file from a Linux system and put it in .words.  Then use .spelling
for any other local entries.

-- Programming

The AST objects are defined in Parse.om.

    Text         # opaque text
    White        # white space
    Comment      # comment
    Macro        # macro, with arguments
    BracesBlock  # { ... }
    MathBlock    # $...$
    BeginBlock   # \begin{envname}...\end{envname}
