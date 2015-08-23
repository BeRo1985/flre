FLRE - Fast Light Regular Expressions - A fast light regular expression library

FLRE ( **F** ast **L** ight **R** egular **E** xpressions) is a fast, safe and efficient regular expression library, which is implemented in Object Pascal (Delphi and Free Pascal). 

It implements the many of the most common Perl and POSIX features, except irregular expression features like backreferences and so on, which aren't supported at FLRE, hence also "Light" at the FLRE name. It also finds the leftmost-first match, the same match that Perl and PCRE would, and can return submatch information. But it also features a flag for a yet experimental POSIX-style leftmost-longest match behaviour mode. 

FLRE is licensed under the LGPL v2.1 with static-linking-exception.

The sercet of FLRE speed is that FLRE uses multiple subengines with automatic selection:

* **Fixed string search** for pure static literal string regexs, SBNDMQ2 (a shift-and/shift-or variant with boyer-moore-style skipping) for short strings shorter than 32 chars, and boyer-moore for strings longer than or equal 32 chars.
* **Approximate heuristic regular expression prefix matching** based on SBNDMQ2 for to find _possible_ match begin boundaries very fast. 
* **On the fly computed DFA** (aka lazy DFA, DFA with caching of parallel-threaded-NFA/Thompson-NFA states) to find the whole match boundaries. For submatches, if they exists, it uses the remain subengines after the DFA match pass. This subengine is very fast. 
* **One pass NFA** This subengine is quite still fast. But it can process simple regexs only, where it's always immediately obvious when a repetition ends. For example `x(y|z)` and `x*yx*` are onepass, but `(xy)|(xz)` and `x*x*` not, but the regex abstract syntax tree optimizer optimizes the most cases anyway out, before the bytecode and the OnePassNFA state map will generated. The base idea is from the re2 regex engine from Google.
* **Bit state NFA** This subengine is quite still fast. But it can process short regexs only with less than 512 regex VM bytecode instructions. It's a backtracker with a manual stack in general, but it members the already visited (state, string position) pairs in a bitmap. The base idea is even from the re2 regex engine from Google.
* **Parallel threaded NFA** (aka Thompson NFA / Pike VM). This subengine supports the most 08/15 regular expression syntax features _except_ backtracking-stuff as backreferences and so on. And this subengine is also still fast, but not so fast like the onepass NFA subengine.

And as an addon, FLRE features prefix presearching. So for example the prefix for the example regex `{{{/\bHel(?:lo|loop) [A-Za-z]+\b/}}}` is `Hello`.

And FLRE can process 0-based null terminated C/C++ and 1-based (Object-)Pascal strings.

For a more complete engine with more features including backreferences and unicode support etc., see [BRRE](https://github.com/BeRo1985/brre)

IRC: #flre on freenode

