%% -*- latex -*-
\documentclass[serif]{beamer}

% \usetheme{Warsaw} 

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
\date{\href{http://www.meetup.com/haskellhackersathackerdojo/events/105583982/}{March 21, 2013}}

\nc\wpicture[2]{\includegraphics[width=#1]{pictures/#2}}
% \nc\wpicture[2]{}

\nc\wfig[2]{
\begin{center}
\wpicture{#1}{#2}
\end{center}
}
\nc\fig[1]{\wfig{4in}{#1}}

%% \nc\fb[1]{\fbox{#1\vspace{-2ex}}}

\nc\usebg[1]{
\usebackgroundtemplate{\wpicture{1.2\textwidth}{#1}}
}

\setlength{\itemsep}{2ex}

\setlength{\parskip}{1ex}

% \setlength{\blanklineskip}{0.66084ex}
\setlength{\blanklineskip}{1.5ex}

\begin{document}

\frame{\titlepage
% \usebg{origami_swan}
\vspace{-0.2in}
\wfig{2in}{origami/robert-langs-origami-bicurve-pot-13}
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
> 
> type (:+) = Either
> type (:*) = (,)
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
> rangeL l h  | l >= h     = []
>             | otherwise  = l : rangeL (succ l) h

}\framet{Recursive functional programming}{

On (binary leaf) trees:

> data T a = L a | B (T a) (T a)

> productT :: T Integer -> Integer
> productT (L a)    = a
> productT (B s t)  = productT s * productT t
> 
> rangeT :: Integer -> Integer -> T Integer
> rangeT l h  | l == h     = L l
>             | otherwise  = B (rangeT l m) (rangeT (m+1) h)
>  where m = (l+h) `div` 2

}\framet{Structured functional programming}{

\begin{quotation}
... recursive equations are the ``assembly language'' of functional programming, and direct recursion the \texttt{goto}.
\end{quotation}
\begin{flushright}
Jeremy
Gibbons,
\emph{\href{http://www.cs.ox.ac.uk/publications/publication2335-abstract.html}{Origami programming}}
\end{flushright} 

Identify commonly useful patterns, determine their properties, and apply them.

}\framet{Catamorphisms (folds)}{

Contract a structure \emph{down} to a single value.

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

}\framet{Anamorphisms (unfolds)}{

Expand a structure \emph{up} to a single value.

Lists:

> unfoldL :: (b -> Maybe (a :* b)) -> (b -> [a])
> unfoldL f b = case f b of
>                 Just (a,b')  -> a : unfoldL f b'
>                 Nothing      -> []

> rangeL' :: Integer :* Integer -> [Integer]
> rangeL' = unfoldL g
>  where
>    g (l,h)  | l >= h     = Nothing
>             | otherwise  = Just (l, (succ l, h))

}\framet{Anamorphisms (unfolds)}{

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

< foldL  :: (a -> b -> b) -> b          -> ([a] -> b)
<        =~ (a :* b -> b) -> b          -> ([a] -> b)
<        =~ (a :* b -> b) -> (() -> b)  -> ([a] -> b)
<        =~ (a :* b -> b) :* (() -> b)  -> ([a] -> b)
<        =~ ((a :* b :+ ()) -> b)       -> ([a] -> b)
<        =~ (Maybe (a :* b) -> b)       -> ([a] -> b)

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

}\framet{General regular algebraic data types}{

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
> fact5 :: Integer -> Integer
> fact5 n = hylo h g (1,n)
>  where
>    g :: Range -> TF Integer Range
>    g (lo,hi) =
>      case lo `compare` hi of
>        GT  -> LF 1
>        EQ  -> LF lo
>        LT  -> let mid = (lo+hi) `div` 2 in
>                 BF (lo,mid) (mid+1,hi)
>    h :: TF Integer Integer -> Integer
>    h (LF i)    = i
>    h (BF u v)  = u * v

Much more parallel-friendly!

}\framet{Another look}{

%% \fig{fold-diag}
%% \fig{unfold-diag}
%% \fig{hylo-diag}

%% \tikzset{global scale/.style={
%%     scale=0.5,
%%     every node/.style={scale=0.5}
%%   }
%% }

\begin{tikzcd}[column sep=8ex]
  \F (\FixF) \rar{\fmapp{\fold h}} \& \F b \dar{h} \\
  \FixF \uar{\unRoll} \rar{\fold h} \& b
\end{tikzcd}
\begin{tikzcd}[column sep=10ex]
  \F a \rar{\fmapp{\unfold g}} \& \F (\FixF) \dar{\Roll} \\
  a \uar{g} \rar{\unfold g} \& \FixF
\end{tikzcd}
\begin{tikzcd}[column sep=10ex]
  f\, a \rar{\fmapp{\hylo h\, g}} \rar{} \& f\, b \dar{h} \\
  a \uar{g} \rar{\hylo h\, g} \rar{} \& b
\end{tikzcd}


< newtype Fix f = Roll { unRoll :: f (Fix f) }
< 
< fold :: Functor f => (f b -> b) -> (Fix f -> b)
< fold h = h . fmap (fold h) . unRoll
< 
< unfold :: Functor f => (a -> f a) -> (a -> Fix f)
< unfold g = Roll . fmap (unfold g) . g
< 
< hylo :: Functor f => (f b -> b) -> (a -> f a) -> (a -> b)
< hylo h g = fold h . unfold g

}\framet{Optimizing |hylo|}{

<   hylo h g
< == {- definition of hylo -}
<   fold h . unfold g
< == {- definitions of fold and unfold -}
<   h . fmap (fold h) . unRoll . Roll . fmap (unfold g) . g
< == {- unRoll and Roll are inverses -}
<   h . fmap (fold h) . fmap (unfold g) . g
< == {- Functor law: fmap v . fmap u == fmap (v . u) -}
<   h . fmap (fold h . unfold g) . g
< == {- definition of hylo -}
<   h . fmap (hylo h g) . g

Directly recursive.

}\framet{|fold| and |unfold| via |hylo|}{

> unfold' :: Functor f => (a -> f a) -> (a -> Fix f)
> unfold' g = hylo Roll g
> 
> fold' :: Functor f => (f b -> b) -> (Fix f -> b)
> fold' h = hylo h unRoll

}\framet{Summary}{

\begin{minipage}[c]{0.6\textwidth}
\begin{itemize}
\item \emph{Fold} and \emph{unfold} are structured replacements for the ``assembly language'' of recursive definitions.

\item Unifying view of |fold| and |unfold| across data types:
\emph{functor fixpoints}.

\item Recursive programs have a systematic translation to |unfold| and |fold| (``forestation'').

\item The translation reveals parallelism clearly and simply.

\end{itemize}
\end{minipage}
% \hspace{1ex}
\begin{minipage}{0.2\textwidth}
\wpicture{2in}{origami/yoda}
\end{minipage}

}\framet{A cautionary tale}{

\fig{goto-raptor-xkcd}

}


\nc{\pcredit}[3]{\item \href{#1}{\wpicture{0.75in}{#3}}: #2}

\framet{Picture credits}{

\begin{itemize}

\pcredit{https://popularkinetics.wordpress.com/2008/07/24/the-art-and-science-of-folding-paper/}{Robert Lang's Origami BiCurve Pot 13}{origami/robert-langs-origami-bicurve-pot-13}
\pcredit{http://imgbit.com/i313}{unknown}{origami/yoda.jpg}
\pcredit{https://xkcd.com/292/}{Randall Munroe (xkcd)}{goto-raptor-xkcd}

\end{itemize}
}

\end{document}
