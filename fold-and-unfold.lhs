% -*- latex -*-
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


\begin{center}
\fbox{\begin{minipage}[t]{0.35\textwidth}

> length :: [a] -> Int
> length []      = 0
> length (a:as)  =
>   1 + length as

\end{minipage}}
\pause
\fbox{\begin{minipage}[t]{0.5\textwidth}

> product :: [a] -> Int
> product []      = 1
> product (a:as)  =
>   a * product as

\end{minipage}}
\end{center}

\pause
% The bread and butter of functional programming?
\wfig{2in}{bread-and-butter}
}

\framet{Recursive functional programming}{

% Folds: `length`, `sum`, `product`, `take`, `reverse` (quadratic & linear)

> sum []      = 0
> sum (a:as)  = 1 + sum as

> product []      = 1
> product (a:as)  = a * product as

> take 0 _ = []
> take n (a:as) = a : take (n-1) as

> reverse []      = []
> reverse (a:as)  = reverse as ++ [a]

> reverse = rev []
>  where
>   rev [] xs      = xs
>   rev (a:as) xs  = rev as (a:xs)

}

\framet{More recursively defined functions}{

% Unfolds: |units :: Int -> [()]|, |range| (|enumFromTo|), |primes|, |zip|, |iterate|

> take 0 _ = []
> take n (a:as) = a : take (n-1) as

> range :: Int -> Int -> [Int]
> range l h  | l >= h     = []
>            | otherwise  = l : range (succ l) h

> zip :: [a] -> [b] -> [(a,b)]
> zip (a:as)  (b:bs)  = (a,b) : zip as bs
> zip _       _       = []

> iterate :: (a -> a) -> a -> [a]
> iterate f a = a : iterate f (f a)

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
