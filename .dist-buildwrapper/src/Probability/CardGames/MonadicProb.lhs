
> module Probability.CardGames.MonadicProb where

>import Data.Ratio

We will use a monad transformer to add probabilistic computation on top of 
a monad as a transformer.

Here is how the monad transformer works

        The idea is that you use a Monad Transformer. A Monad Transformer
        if like a layer of onion peel. You start with the Identity monad
        and then use Monad Transformers to add layers of functionality.
        So to get a two-state monad you take the Identity and add two
        layers of stateness to it. To get the answer at the end you need
        to unwrap the State layers and then unwrap the Identity layer too.
        
        
        When you're inside your 'do' expression you need a way to choose
        which Monad you're talking to. Ordinary Monad commands will
        talk to the outermost layer and you can use 'lift' to send your
        commands inwards by one layer.
        
        http://blog.sigfpe.com/2006/05/grok-haskell-monad-transformers.html
        

We also use Ratios for the probabality.  Or actaully Rational which is defined
as:

type Rational = Ratio Integer

A distribution is a collection of items with their relative frequency plus a 
normalizing factor for the whole distribution to add to one.   In case of uniform
distribution the normalizing factor would be the number of items in the distribution


