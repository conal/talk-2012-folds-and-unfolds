%% -*- latex -*-
\documentclass[serif]{beamer}

\usepackage{beamerthemesplit}

\usepackage{graphicx}
\usepackage{color}
\DeclareGraphicsExtensions{.pdf,.png,.jpg}

\useinnertheme[shadow]{rounded}
% \useoutertheme{default}
\useoutertheme{shadow}
\useoutertheme{infolines}

\input{macros}

%include polycode.fmt
%include forall.fmt
%include greek.fmt
%include mine.fmt

\title{Folds and unfolds all around us}
\author{\href{http://conal.net}{Conal Elliott}}
\institute{\href{http://tabula.com/}{Tabula}}
% Abbreviate date/venue to fit in infolines space
%% \date{\href{http://www.meetup.com/haskellhackersathackerdojo/events/105583982/}{March 21, 2013}}
\date{Spring, 2013}

\nc\wpicture[2]{\includegraphics[width=#1]{pictures/#2}}

\nc\wfig[2]{
\begin{center}
\wpicture{#1}{#2}
\end{center}
}
\nc\fig[1]{\wfig{4in}{#1}}

\setlength{\itemsep}{2ex}
\setlength{\parskip}{1ex}

\setlength{\blanklineskip}{1.5ex}

\nc\usebg[1]{\usebackgroundtemplate{\wpicture{1.2\textwidth}{#1}}}

\begin{document}

\frame{\titlepage
\vspace{-0.2in}
\wfig{2in}{bicurve-pot}
}

\nc\framet[2]{\frame{\frametitle{#1}#2}}

\nc\hidden[1]{}


\framet{Preliminaries}{

This talk is a literate Haskell program.

%% The underscore in ``OPTIONS_GHC" tweaks latex
\hidden{

> {-# LANGUAGE DeriveFunctor, TypeOperators #-}
> {-# OPTIONS_GHC -Wall #-}
> {-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-type-defaults #-}

}

> module FoldsAndUnfolds where

I'll use some non-standard (for Haskell) type notation:

> type Unit  = ()
> type (:+)  = Either
> type (:*)  = (,)
>
> infixl 7 :*
> infixl 6 :+

}


\framet{Recursive functional programming}{

On numbers:

> fact0 0 = 1
> fact0 n = n * fact0 (n - 1)

On lists:

< data [a] = [] | a : [a]

> productL :: [Integer] -> Integer
> productL []      = 1
> productL (a:as)  = a * productL as
> 
> rangeL :: Integer -> Integer -> [Integer]
> rangeL l h  | l > h      = []
>             | otherwise  = l : rangeL (succ l) h

}\framet{Recursive functional programming}{

On (binary leaf) trees:

> data T a = L a | B (T a) (T a) deriving Show

> productT :: T Integer -> Integer
> productT (L a)    = a
> productT (B s t)  = productT s * productT t

> rangeT :: Integer -> Integer -> T Integer
> rangeT l h  | l == h     = L l
>             | otherwise  = B (rangeT l m) (rangeT (m+1) h)
>  where m = (l+h) `div` 2

}\framet{Recursive functional programming?}{

\wfig{3in}{bread-and-butter}

}\framet{Structured functional programming}{

\ 

\begin{quotation}
... recursive equations are the ``assembly language'' of functional programming, and direct recursion the \texttt{goto}.
\end{quotation}
\begin{flushright}
Jeremy
Gibbons,
\emph{\href{http://www.cs.ox.ac.uk/publications/publication2335-abstract.html}{Origami programming}}
\end{flushright}

\ 

A structured alternative:
\begin{itemize}
\item identify commonly useful patterns,
\item determine their properties, and
\item apply the patterns and properties.
\end{itemize}


}\framet{Folds (``catamorphisms'')}{

Contract a structure \emph{down to} a single value.

For lists:

> foldL :: (a -> b -> b) -> b -> ([a] -> b)
> foldL _ b []      = b
> foldL f b (a:as)  = f a (foldL f b as)

< sumL      = foldL (+) 0
< productL  = foldL (*) 1
< reverseL  = foldL (\ a r -> r ++ [a]) []

For trees:

> foldT :: (b -> b -> b) -> (a -> b) -> (T a -> b)
> foldT _ l (L a)    = l a
> foldT b l (B s t)  = b (foldT b l s) (foldT b l t)

< productT = foldT (*) id

}\framet{Unfolds (``anamorphisms'')}{

Expand a structure \emph{up from} a single value.

Lists:

> unfoldL :: (b -> Maybe (a :* b)) -> (b -> [a])
> unfoldL f b = case f b of
>                 Just (a,b')  -> a : unfoldL f b'
>                 Nothing      -> []

> rangeL' :: Integer :* Integer -> [Integer]
> rangeL' = unfoldL g
>  where
>    g (l,h)  | l > h      = Nothing
>             | otherwise  = Just (l, (succ l, h))

}\framet{Unfolds (``anamorphisms'')}{

Trees:

> unfoldT :: (b -> a :+ b :* b) -> (b -> T a)
> unfoldT g x = case g x of
>                 Left a       -> L a
>                 Right (c,d)  -> B (unfoldT g c) (unfoldT g d)

> rangeT' :: Integer :* Integer -> T Integer
> rangeT' = unfoldT g
>  where
>    g (l,h)  | l == h     = Left l
>             | otherwise  = Right ((l,m) , (m+1,h))
>      where m = (l+h) `div` 2


}\framet{Factorial again}{

Assembly language:

< fact0 0 = 1
< fact0 n = n * fact0 (n - 1)

You may have seen this Haskelly definition:

< fact1 n = product [1 .. n]

\emph{Theme:} replace control structures by data structures and standard combining forms.

Carry this theme further.

}\framet{Combining |unfold| and |fold|}{

Equivalently,

> fact1 = productL . rangeL 1

\emph{Note}: composition of unfold (|rangeL|) and fold |productL|.

More explicit:

> fact2 = foldL (*) 1 . unfoldL g
>  where
>    g 0  = Nothing
>    g n  = Just (n,n-1)

This combination of |unfold| and |fold| is called a ``hylomorphism''.

}\framet{Fibonacci}{

Assembly language:

> fib0 0  = 0
> fib0 1  = 1
> fib0 n  = fib0 (n-1) + fib0 (n-2)

Via trees:

> fibT :: Integer -> T Integer
> fibT 0  = L 0
> fibT 1  = L 1
> fibT n  = B (fibT (n-1)) (fibT (n-2))
> 
> sumT :: T Integer -> Integer
> sumT = foldT (+) id
> 
> fib1 :: Integer -> Integer
> fib1 = sumT . fibT

}\framet{Fibonacci}{

More explicitly hylomorphic:

< unfoldT :: (b -> a :+ b :* b) -> (b -> T a)

> fib2 :: Integer -> Integer
> fib2 = foldT (+) id . unfoldT g
>  where
>    g 0  = Left 0
>    g 1  = Left 1
>    g n  = Right (n-1,n-2)

}\framet{Generalizing folds and unfolds}{

Summary of |fold| and |unfold|:

< foldL    :: (a -> b -> b) -> b -> ([a] -> b)
< 
< unfoldL  :: (b -> Maybe (a :* b)) -> (b -> [a])
< 
< SPACE
< foldT    :: (b -> b -> b) -> (a -> b) -> (T a -> b)
< 
< unfoldT  :: (b -> a :+ b :* b) -> (b -> T a)

Why the asymmetry?

}\framet{Playing with type isomorphisms}{

< foldL  :: (a -> b -> b) -> b            -> ([a] -> b)
<        =~ (a :* b -> b) -> b            -> ([a] -> b)
<        =~ (a :* b -> b) -> (Unit -> b)  -> ([a] -> b)
<        =~ (a :* b -> b) :* (Unit -> b)  -> ([a] -> b)
<        =~ ((a :* b :+ Unit) -> b)       -> ([a] -> b)
<        =~ (Maybe (a :* b) -> b)         -> ([a] -> b)

Why |Maybe (a :* b)|?

Because

< [a]  =~ Maybe (a :* (Maybe (a :* (Maybe (a :* (...))))))
<      =~ Fix (/\ b -> Maybe (a :* b))

}\framet{Regularizing}{

Recall:

< foldL :: (a -> b -> b) -> b -> ([a] -> b)

A more standard interface:

> foldLF :: (Maybe (a :* b) -> b) -> ([a] -> b)
> foldLF h = foldL (curry (h . Just)) (h Nothing)

Now the duality emerges:

< unfoldL  :: (b -> Maybe (a :* b)) -> (b -> [a])
< foldLF   :: (Maybe (a :* b) -> b) -> ([a] -> b)

Similarly for tree fold and unfold.

}\framet{List and tree |unfold| and |fold| -- pictures}{
\begin{center}
\begin{tikzcd}
  \Maybe (a \times b) \&  \\
  b \arrow{u}{g} \rar[swap]{\unfoldL g} \& \List{a}
\end{tikzcd}
\hspace{6ex}
\begin{tikzcd}
  {} \& \Maybe (a \times b) \arrow{d}{h} \\
  \List{a} \arrow[swap]{r}{\foldL h} \& b
\end{tikzcd}
\vspace{6ex}
\begin{tikzcd}
  a + b \times b \&  \\
  b \arrow[swap]{u}{g} \rar{\unfoldT g} \& \Tree{a}
\end{tikzcd}
\hspace{6ex}
\begin{tikzcd}
  {} \& a + b \times b \arrow{d}{h} \\
  \Tree{a} \arrow[swap]{r}{\foldT h} \& b
\end{tikzcd}
\end{center}
}\framet{General regular algebraic data types -- pictures}{

Build up from ``base functor'' $F$ to fixpoint $\FixF$:

\begin{center}
\begin{tikzcd}
  \F b \&  \\
  b \arrow{u}{g} \rar[swap]{\unfold g} \& \FixF
\end{tikzcd}
\hspace{6ex}
\begin{tikzcd}
  {} \& \F b \arrow{d}{h} \\
  \FixF \arrow[swap]{r}{\fold h} \& b
\end{tikzcd}
\end{center}

}\framet{General regular algebraic data types -- Haskell}{

Build up from ``base functor'' |f|:

> newtype Fix f = Roll { unRoll :: f (Fix f) }
> 
> fold :: Functor f => (f b -> b) -> (Fix f -> b)
> fold h = h . fmap (fold h) . unRoll
> 
> unfold :: Functor f => (a -> f a) -> (a -> Fix f)
> unfold g = Roll . fmap (unfold g) . g
> 
> hylo :: Functor f => (f b -> b) -> (a -> f a) -> (a -> b)
> hylo h g = fold h . unfold g

Let's revisit our examples.

}\framet{Factorial via list |hylo|}{

> data LF a t = NilF | ConsF a t deriving Functor
> type L' a = Fix (LF a)
>
> fact3 :: Integer -> Integer
> fact3 = hylo h g
>  where
>    g :: Integer -> LF Integer Integer
>    g 0  = NilF
>    g n  = ConsF n (n-1)
>    h :: LF Integer Integer -> Integer
>    h NilF         = 1
>    h (ConsF n u)  = n * u

}\framet{Fibonacci via tree |hylo|}{

> data TF a t = LF a | BF t t deriving Functor
> type T' a = Fix (TF a)
> 
> fib3 :: Integer -> Integer
> fib3 = hylo h g
>  where
>    g :: Integer -> TF Integer Integer
>    g 0  = LF 0
>    g 1  = LF 1
>    g n  = BF (n-1) (n-2)
>    h :: TF Integer Integer -> Integer
>    h (LF n)    = n
>    h (BF u v)  = u + v

}\framet{Factorial via tree hylo}{

> type Range = Integer :* Integer
>
> fact4 :: Integer -> Integer
> fact4 n = hylo h g (1,n)
>  where
>    g :: Range -> TF Integer Range
>    g (lo,hi) =  case lo `compare` hi of
>                   GT  -> LF 1
>                   EQ  -> LF lo
>                   LT  -> let mid = (lo+hi) `div` 2 in
>                            BF (lo,mid) (mid+1,hi)
>    h :: TF Integer Integer -> Integer
>    h (LF i)    = i
>    h (BF u v)  = u * v

Parallel-friendly!

}\framet{Another look and |unfold| and |fold|}{

\begin{center}
\begin{tikzcd}[column sep=10ex]
  \F a \rar{\fmapp{\unfold g}} \& \FFixF \dar{\Roll} \\
  a \uar{g} \rar[dashed,swap]{\unfold g} \& \FixF
\end{tikzcd}
\hspace{4ex}
\begin{tikzcd}[column sep=10ex]
  \FFixF \rar{\fmapp{\fold h}} \& \F b \dar{h} \\
  \FixF \uar{\unRoll} \rar[dashed,swap]{\fold h} \& b
\end{tikzcd}
\end{center}

< newtype Fix f = Roll { unRoll :: f (Fix f) }
< 
< unfold :: Functor f => (a -> f a) -> (a -> Fix f)
< unfold g = Roll . fmap (unfold g) . g
< 
< fold :: Functor f => (f b -> b) -> (Fix f -> b)
< fold h = h . fmap (fold h) . unRoll

}\framet{Another look and |hylo|}{

\vspace{5ex}

\begin{center}
\begin{tikzcd}[column sep=10ex]
  a % \arrow[dashed,swap,bend right]{rr}{\hylo h\,g}
    \arrow[dashed]{rr}{\hylo h\,g}\& \& b
\end{tikzcd}
\end{center}

}\framet{Another look and |hylo|}{
\vspace{8ex}
\begin{center}
\begin{tikzcd}[column sep=10ex]
  a \rar{\unfold g} \& \FixF \rar{\fold h}  \& b
\end{tikzcd}
\end{center}

Definition of |hylo|.

}\framet{Another look and |hylo|}{

\begin{center}
\begin{tikzcd}[column sep=10ex]
  \F a \rar{\fmapp{\unfold g}} \& \FFixF \dar[shift left=0.7ex]{\Roll} \rar{\fmapp{\fold h}} \& \F b \dar{h} \\
  a % \arrow[dashed,swap,bend right]{rr}{\hylo h\, g}
    \uar{g} \rar[dashed,swap]{\unfold g} \& \FixF \uar[shift left=0.7ex]{\unRoll} \rar[dashed,swap]{\fold h} \& b
\end{tikzcd}
\end{center}

By definitions of |fold| and |unfold|.

}\framet{Another look and |hylo|}{

\begin{center}
\begin{tikzcd}[column sep=10ex]
  \F a \rar{\fmapp{\unfold g}} \& \FFixF \rar{\fmapp{\fold h}} \& \F b \dar{h} \\
  a % \arrow[dashed,swap,bend right]{rr}{\hylo h\, g}
    \uar{g} \rar[dashed,swap]{\unfold g} \& \FixF \rar[dashed,swap]{\fold h} \& b
\end{tikzcd}
\end{center}

Since |unRoll| and |Roll| are inverses.

}\framet{Another look and |hylo|}{

\begin{center}
\begin{tikzcd}[column sep=10ex]
  \F a \arrow{rr}{\fmapp{\fold h \comp \unfold g}} \& \& \F b \dar{h} \\
  a % \arrow[dashed,swap,bend right]{rr}{\hylo h\, g}
    \uar{g} \arrow[dashed,swap]{rr}{\fold h \comp \unfold g} \& \& b
\end{tikzcd}
\end{center}

By the |Functor| law: |fmap v . fmap u == fmap (v . u)|.

}\framet{Another look and |hylo|}{

\begin{center}
\begin{tikzcd}[column sep=10ex]
  \F a \arrow{rr}{\fmapp{\hylo h\, g}} \& \& \F b \dar{h} \\
  a \uar{g} \arrow[dashed,swap]{rr}{\hylo h\, g} \& \& b
\end{tikzcd}
\end{center}

Definition of |hylo|.
Directly recursive!

}\framet{All together}{
\begin{center}
\begin{tikzcd}[column sep=15ex]
  \F a \arrow[bend left=50]{rr}{\fmapp{\hylo h \, g}}
       \arrow[bend left=25]{rr}{\fmapp{\fold h \comp \unfold g}}
       \arrow{r}{\fmapp{\unfold g}} \& \FFixF \arrow[shift left=0.7ex]{d}{\Roll} \arrow{r}{\fmapp{\fold h}} \& \F b \dar{h} \\
  a \arrow[dashed,swap,bend right=50]{rr}{\hylo h\, g}
    \arrow{u}{g} \arrow[dashed,swap]{r}{\unfold g} \& \FixF \arrow[shift left=0.7ex]{u}{\unRoll} \arrow[dashed,swap]{r}{\fold h} \& b
\end{tikzcd}
\end{center}
}\framet{Reversed}{
\begin{center}
\begin{tikzcd}[column sep=15ex]
     \F b   
  \& \FFixF 
            \arrow[swap]{l}{\fmapp{\fold h}}
            \arrow[shift left=0.7ex]{d}{\Roll}
  \& \F a   
            \arrow[swap]{l}{\fmapp{\unfold g}}
            \arrow[bend right=50,swap]{ll}{\fmapp{\hylo h \, g}}
            \arrow[bend right=25,swap]{ll}{\fmapp{\fold h \comp \unfold g}}
            \dar{h}
\\   b      
            \arrow{u}{g}
  \& \FixF  
            \arrow[dashed]{l}{\unfold g}
            \arrow[shift left=0.7ex]{u}{\unRoll}
  \& a
            \arrow[dashed]{l}{\fold h}
            \arrow[dashed,bend left=50]{ll}{\hylo h\, g}
\end{tikzcd}
\end{center}
}\framet{|fold| and |unfold| via |hylo|}{

|hylo| subsumes both |fold| and |unfold|:

< unfold g  = hylo Roll g
<
< fold h    = hylo h unRoll

since

< hylo h g == fold h . unfold g

and

< fold Roll == id == unfold unRoll

}\framet{Summary}{

\begin{minipage}[c]{0.6\textwidth}
\begin{itemize}
\item \emph{Fold} and \emph{unfold} are structured replacements for the ``assembly language'' of recursive definitions.

\item Unifying view of |fold| \& |unfold| across data types via
\emph{functor fixpoints}.

\item Recursive programs have a systematic translation to |unfold| and |fold|.

\item The translation reveals parallelism clearly and simply.

\end{itemize}
\end{minipage}
% \hspace{1ex}
\begin{minipage}[c]{0.2\textwidth}
\wpicture{2in}{yoda}
\end{minipage}

}\framet{A cautionary tale}{

\wfig{4.7in}{goto-raptor-xkcd}

}


\nc{\pcredit}[3]{\item \href{#1}{\wpicture{0.75in}{#3}} #2}

\framet{Picture credits}{

\begin{itemize}

\pcredit{https://popularkinetics.wordpress.com/2008/07/24/the-art-and-science-of-folding-paper/}{Robert Lang's Origami BiCurve Pot 13}{bicurve-pot}
\pcredit{http://www.mofga.org}{Maine Organic Farmers}{bread-and-butter}
\pcredit{http://imgbit.com/i313}{unknown}{yoda.jpg}
\pcredit{https://xkcd.com/292/}{Randall Munroe (xkcd)}{goto-raptor-xkcd}

\end{itemize}
}

\end{document}
