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

% \setlength{\parskip}{2ex}

\begin{document}

\frame{\titlepage
% \usebg{origami_swan}
\vspace{-0.2in}
\wfig{2in}{origami/heart-1}
}

\nc\framet[2]{
\frame{
\frametitle{#1}
#2
}}

\framet{Recursive functional programming}{

% Folds: `length`, `sum`, `product`, `take`, `reverse` (quadratic & linear)

> sum []      = 0
> sum (a:as)  = a + sum as

> product []      = 1
> product (a:as)  = a * product as

> reverse []      = []
> reverse (a:as)  = reverse as ++ [a]

> reverse = rev []
>  where
>   rev [] xs      = xs
>   rev (a:as) xs  = rev as (a:xs)

}

\framet{More recursively defined functions}{

% Unfolds: |units :: Int -> [()]|, |range| (|enumFromTo|), |primes|, |zip|, |iterate|

> range :: Int -> Int -> [Int]
> range l h  | l >= h     = []
>            | otherwise  = l : range (succ l) h

> zip :: [a] -> [b] -> [(a,b)]
> zip (a:as)  (b:bs)  = (a,b) : zip as bs
> zip _       _       = []

> iterate :: (a -> a) -> a -> [a]
> iterate f a = a : iterate f (f a)

}

\framet{Recursive functional programming?}{

> product []      = 1
> product (a:as)  = a * product as

> range :: Int -> Int -> [Int]
> range l h  | l >= h     = []
>            | otherwise  = l : range (succ l) h

\pause
% The bread and butter of functional programming?
\wfig{2in}{bread-and-butter}
}

\framet{Goto}{
% \vspace{0.6in}
\begin{center}
\begin{minipage}[t]{0.7\textwidth}
\begin{center}
{ \it
In a sense, recursive equations are the \\
`assembly language' of functional programming, \\
and direct recursion the `goto`.
}
\end{center}
% \vspace{2ex}
\begin{flushright}
-- Jeremy Gibbons
\end{flushright}
\end{minipage}
\end{center}

\pause
\vspace{2ex}
What is our ``structured programming''?

\ \pause

Higher-order functions capturing common patterns.

}

\framet{Examples: list consumers}{

> sum []      = 0
> sum (a:as)  = a + sum as

> product []      = 1
> product (a:as)  = a * product as

> reverse []      = []
> reverse (a:as)  = reverse as ++ [a]

\ \pause

What's the common pattern here?

}

\framet{List \emph{fold}}{

> foldL :: b -> (a -> b -> b) -> ([a] -> b)
> foldL b _ []      = b
> foldL b f (a:as)  = f a (foldL b f as)

> sum      = foldL (+)  0
> product  = foldL (*)  1
> reverse  = foldL []   (\ a r -> r ++ [a])

}

\framet{Examples: list producers}{

> range :: Int -> Int -> [Int]
> range l h  | l >= h     = []
>            | otherwise  = l : range (succ l) h

> zip :: [a] -> [b] -> [(a,b)]
> zip (a:as)  (b:bs)  = (a,b) : zip as bs
> zip _       _       = []

> iterate :: (a -> a) -> a -> [a]
> iterate f a = a : iterate f (f a)

\ \pause

What's the common pattern here?

}

\framet{List \emph{unfold}}{

> unfoldL      :: (b -> Maybe (a,b)) -> (b -> [a])
> unfoldL f b  =  case f b of
>                   Just (a,b')  -> a : unfoldL f b'
>                   Nothing      -> []

> range :: (Int,Int) -> [Int]
> range = unfoldL ( \ (l,h) -> 
>  if l >= h then Nothing else Just (l, (succ l, h)) )

> zip :: ([a],[b]) -> [(a,b)]
> zip = unfoldL g
>  where
>    g (a:as  , b:bs  )  = Just ((a,b), zip as bs)
>    g (_     , _     )  = Nothing

% > iterate :: (a -> a) -> a -> [a]
% > iterate f = unfoldL g where g a = Just (a, f a)

}

\framet{Binary leaf trees}{

> data T a = L a | B (T a) (T a)

> sumT :: Num a => T a -> a
> sumT (L a)    = a
> sumT (B s t)  = sumT s + sumT t

> rangeT :: Int -> Int -> T Int
> rangeT l h  | l == h     = L l
>             | otherwise  = B (rangeT l m) (rangeT (m+1) h)
>  where m = (l+h) `div` 2

> reverseT :: T a -> T a
> reverseT (L a)    = L a
> reverseT (B s t)  = B (reverseT t) (reverseT s)

\pause

Again, what's in common?

}


\framet{Tree folds}{

> foldT :: (a -> b) -> (b -> b -> b) -> (T a -> b)
> foldT p q (L a)    = p a
> foldT p q (B s t)  = q (foldT p q s) (foldT p q t)

\pause

> sumT :: Num a => T a -> a
> sumT = foldT id (+)

> reverseT :: T a -> T a
> reverseT = foldT L (flip B)

}

\framet{Tree unfolds}{

> unfoldT :: (b -> Either a (b,b)) -> (b -> T a)
> unfoldT h b =  case h b of 
>                  Left   a      -> L a
>                  Right  (s,t)  -> B (unfoldT h s) (unfoldT h t)

\pause

> reverseT :: T a -> T a
> reverseT = unfoldT h
>   where  h (L a)    = Left a
>          h (B s t)  = Right (t,s)

> rangeT :: (Int, Int) -> T Int
> rangeT = unfoldT h
>   where  h (l,h)  | l == h     = Left l
>                   | otherwise  = Right ((l,m),(m+1,h))
>            where m = (l+h) `div` 2

}

\framet{\emph{What's next?}}{

Working here.

}


\end{document}

%% ------------------ Junk

\nc\facto[1]{
\frame{\frametitle{Factorial}
\fig{fact/#1}
}
}

\facto{a}
\facto{b}
\facto{c}
\facto{d}
