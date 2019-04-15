\name{remove.punct}
\alias{remove.punct}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Tokenizing and Removing Punctuation Marks
}
\description{
It splits a given text into separated words and removes its punctuation marks. 
}
\usage{
remove.punct(text)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{text}{
an object.
}
}
\details{
This function also considers numbers as a separated word.

Note that This function removes "dot"" only if it is at the end of the sentence, separately. Meanwhile, it does not eliminate dash and hyper.Because it is assumed that words containing these punctuations are one word.
}
\value{
A vector of character string.

%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}

\author{
Neda Daneshgar and Majid Sarmad
}

\examples{
x = "This is an  example-based MT!"  
remove.punct (x)
}
