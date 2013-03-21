sample files in the doc dir are downloaded from http://okmij.org/ftp/tagless-final/course/index.html#lecture

Initial and final, deep and shallow: the first-order case

Intro1.hs [2K] 
Algebraic data type/initial representation of expressions 
Constructor functions: the intimation of the final representation (or, shallow embedding)
Intro2.hs [3K] 
Symantics: parameterization of terms by interpreters

Intro3.hs [2K] 
Initial and Final, Deep and Shallow, First-class

ExtI.hs [<1K] 
Algebraic data types are indeed not extensible

ExtF.hs [2K] 
Adding a new expression form to the final view: solving the expression problem

Serialize.hs [7K] 
Serialization and de-serialization

SerializeExt.hs [4K] 
De-serializing the extended language



Type-preserving embedding of higher-order, typed DSLs

Using simply-typed lambda-calculus with constants as a sample DSL, we demonstrate its various embeddings into Haskell. We aim at a type-preserving embedding and efficient and type-preserving evaluations. The tagless-final embedding not only achieves this goal, it also makes the type-preservation patently clear. Tagless-final evaluators are well-typed Haskell programs with no pattern-matching on variant types. It becomes impossible for the evaluators to get stuck. Since the type preservation of the evaluators is apparent not only to us but also to a Haskell compiler, the evaluators can be efficiently compiled. Tagless-final embeddings are also extensible, letting us add to the syntax of the DSL, preserving and reusing old interpreters.
IntroHOT.hs [3K] 
The illustration of problems of embedding a typed DSL into a typed metalanguage 
Either the Universal type (and hence spurious partiality, type tags and inefficiency), or fancy type systems seem inevitable. The problem stems from algebraic data types' being too broad: they express not only well-typed DSL terms but also ill-typed ones.

Term.agda [2K] 
< http://www.iis.sinica.edu.tw/~scm/2008/typed-lambda-calculus-interprete/ > 
Shin-Cheng Mu: Typed Lambda-Calculus Interpreter in Agda. September 24, 2008 
Shin-Cheng Mu solves the problem of the type-preserving tagless interpretation of simply-typed lambda-calculus, relying on dependent types and type functions in full glory.

IntroHOIF.hs [6K] 
Tagless-initial and Tagless-final evaluators

TTFdB.hs [7K] 
Typed, tagless, final, with de Bruijn indices: Expressing the type system of simply-typed lambda-calculus in Haskell98. No dependent types are necessary after all. The types of methods in the Symantics type class read like the axioms and inference rules of the implication fragment of minimal logic.

TTF.hs [7K] 
Typed, tagless, final, in the higher order abstract syntax (HOAS). We illustrate extending the DSL with more constants, types, and expression forms.

TTIF.hs [8K] 
Initial-final isomorphism, in the higher-order case

