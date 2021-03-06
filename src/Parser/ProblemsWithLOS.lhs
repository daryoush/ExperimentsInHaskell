> module Parser.ProblemsWithLOS where

This module contains examples of problems with using Lists computation as Parser.

Issues listed in "Polish Parsers, Step by Step" paper

1. Backtracking implementations based on the list of successes method keep a reference to 
the full sequence of
input tokens, until it has become clear that no further
alternatives will be found.

Same concept stated in Combinator Parsers: From toys to Tools.

disadvantage of parsers constructed in this way is
that parsing gets slow when productions have many alternatives, since all alternatives are 
tried sequentially at each
branching point, thus causing a large number of symbol comparisons. This eﬀect becomes worse 
when a naive user uses
the combinators to describe very large grammars as in:
fold1 (<|>) (map symbol [1..1000])
Here on the average 500 comparisons are needed in order to
recognize a symbol. Such parsers may easily be implicitly
constructed by the use of more complicated derived combinators, without the user actually noticing. 

Note the paper "Combinator Parsers..."    explained how we can
implement a parsing algorithm that performs a breadth-
ﬁrst instead of a depth-ﬁrst search for a complete parse,
and sequentially accesses the input stream; it furthermore
removes the LL(1) restrictions and works with unbounded
look-ahead. This completely cures the ﬁrst problem mentioned above, without 
any annotations provided by the programmer nor a need to understand how the parsing process
proceeds internally, but it introduces the second problem
mentioned.  

2. Parsers only start producing a result once the complete
input has been examined, and thus do not exhibit online behaviour.

Same concept stated in "Combinator Parser, a short Tutorial" paper.

When we inspect the code for the sequential composition closely however, and
investigate when the first element of the resulting list will be produced, we see
that this is only the case after the right-hand side parser of <*> returns its first result. 
For the root symbol this implies that we get only to see the result after we have found our  
first complete parse. So, taking the observation of the orevious subsection into account, at 
the end of the first complete parse we have stored the complete input and the complete result 
in memory. For long inputs
this may become prohibitively costly, especially since garbage collection will
take a lot of time without actually collecting a lot of garbage.

To illustrate the dierence consider the parser:
parse (pMany (pSym 'a')) (listToStr ('a' : undefined)

The parsers we have seen thus far will produce  undefined here. An online parser will
return 'a' : undefined  instead, since the initial 'a' could be succesfully recognised
irrespective of what is behind it in the input.


3. Simple backtracking implementations are grossly ineﬃcient when dealing with 
ambiguous grammars; for
each possible parse for a part of the text, the rest of the
text is parsed once. This becomes immediately clear
if we look at the code for the sequential composition
of two parsers: p <*> q. If the parser p can succeed in
several diﬀerent ways by consuming the same preﬁx of
the input, the parser q is called once for each of those
successful parses of p, with the same remaining input
as argument.

Same concept stated in "Combinator Parser, a short Tutorial" paper.

The backtracking implementation may lead to unexpected space consumption.
After the parser p in a sequential composition p<*>q has found its first complete parse, parsing 
by q commences. Since this may fail further alternatives for p
may have to be tried, even when it is obvious from the grammar that these will
all fail. In order to be able to continue with the backtracking process (i.e. go
back to a previous choice point) the implementation keeps a reference in the
input which was passed to the composite parser. Unfortunately this is also the
case for the root symbol, and thus the complete input is kept in memory at least
until the first complete parse has been found, and its witness has been selected
as the one to use for further processing

Same concept stated in Combinator Parsers: From toys to Tools.

A further source of potential ineﬃciency is caused by non-determinism. When many 
alternatives may recognize strings
with a common preﬁx, this preﬁx will be parsed several
times, with usually only one of those alternatives eventually
succeeding. So for highly “non-deterministic” grammars the
price paid may be high, and even turn out to be exponential
in the size of the input. Although it is well known how to
construct deterministic automata out of non-deterministic
ones, this knowledge is not used in this implementation, nor
may it easily be incorporated. 

As a consequence of the ﬁrst two points the constructed
result and the initial input are both present in memory once
a complete parse has been found.    This is highly undesirable
if we process input that represents a long list of similar items
such as a bibtex ﬁle, or when we describe a lexical scanner
that returns a list of input tokens. In such situations we
do not want ﬁrst read and recognize a complete input ﬁle
before producing any output: output should be produced as
soon as an individual element has been recognized.