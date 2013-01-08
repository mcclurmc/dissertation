\LoadClass[a4paper,12pt]{report}

\newif\if@oxfordlogo@
\@oxfordlogo@true

\newif\if@proposal@
\@proposal@true

\DeclareOption{logo}{\@oxfordlogo@true}
\DeclareOption{nologo}{\@oxfordlogo@false}
\DeclareOption{dissertation}{\@proposal@false}
\DeclareOption{proposal}{\@proposal@true}

\ProcessOptions

\RequirePackage[margin=1in]{geometry}
\RequirePackage{graphicx}
\RequirePackage{url}

\def\textfraction{0}
\def\topfraction{1}

\def\college#1{\def\@college{#1}}
\def\organisation#1{\def\@organisation{#1}}
\def\award#1{\def\@award{#1}}
\def\version#1{\def\@version{#1}}

\college{}
\organisation{}
\award{}
\version{}

\def\maketitle{%
  \null \vskip 30mm \thispagestyle{empty} {\flushleft {\LARGE\bfseries
      \@title\par} \vskip 4mm {\LARGE \@author\par} \vskip 20mm
    \ifx\@college\empty\else{\Large\@college\par}\fi
    \ifx\@organisation\empty\else\vskip
    1mm{\Large\@organisation\par}\fi \vfill
    \ifx\@award\empty\else{%
      \hbox to \linewidth{%
        \if@oxfordlogo@\hbox to 40pt{\vbox to 40pt{%
            \includegraphics[height=40pt]{oxfordlogo}}}%
        \hskip 8pt\fi%
        \hbox{\vbox to 40pt{%
            \vss\vss\hbox{\large%
              \if@proposal@ Project Proposal\else%
              A dissertation submitted for the\fi%
            }\vss%
            \vss\hbox{\large MSc in \@award}\vss}}}%
        \hss}\fi \vskip 10mm \null} \newpage}

\def\section{\@startsection{section}{1}{\z@}%
  {-2.9ex \@plus -1ex \@minus -.2ex}%
  {1.9ex \@plus.2ex}%
  {\normalfont\large\bfseries}}
\def\subsection{\@startsection{subsection}{2}{\z@}%
  {-2.68ex\@plus -1ex \@minus -.2ex}%
  {1.24ex \@plus .2ex}%
  {\normalfont\bfseries}}
\def\subsubsection{\@startsection{subsubsection}{3}{\z@}%
  {-2.68ex\@plus -1ex \@minus -.2ex}%
  {1.24ex \@plus .2ex}%
  {\normalfont\normalsize\slshape}}
\def\paragraph{\@startsection{paragraph}{4}{\z@}%
  {2.68ex \@plus1ex \@minus.2ex}%
  {-1em}%
  {\normalfont\normalsize\slshape}}
\def\subparagraph{\@startsection{subparagraph}{5}{\parindent}%
  {2.68ex \@plus1ex \@minus .2ex}%
  {-1em}%
  {\normalfont\normalsize\slshape}}

\def\ps@myheadings{%
    \let\@oddfoot\@empty\let\@evenfoot\@empty
    \def\@evenhead{\thepage\hfil\slshape\leftmark}%
    \def\@oddhead{{\slshape\rightmark}\hfil\thepage}%
    \let\@mkboth\@gobbletwo
    \let\chaptermark\@gobble
    \let\sectionmark\@gobble
    }

\pagestyle{myheadings}

\newenvironment{acknowledgements}{%
  \titlepage
  \null\vfil
  \@beginparpenalty\@lowpenalty
  \begin{center}%
    \bfseries Acknowledgements
    \@endparpenalty\@M
  \end{center}}%
{\par\vfil\null\endtitlepage}

\if@proposal@
  \renewcommand\thesection{\@arabic\c@section}
\fi

\def\@makechapterhead#1{%
  \vspace*{50\p@}%
  {\parindent \z@ \raggedright \normalfont
    \interlinepenalty\@M
    \Large\bfseries
    \leavevmode\hbox to 40\p@{\thechapter\hss}\nobreak #1\par\nobreak
    \vskip 40\p@
  }}

\def\@makeschapterhead#1{%
  \vspace*{50\p@}%
  {\parindent \z@ \raggedright
    \normalfont
    \interlinepenalty\@M
    \Large\bfseries  #1\par\nobreak
    \vskip 40\p@
  }}
