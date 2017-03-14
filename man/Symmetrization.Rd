\name{Symmetrization}
\alias{Symmetrization}
\alias{print.symmet}

\title{
Calculating Symmetric Word Alignment
}
\description{
It calculates source-to-target and target-to-source alignments using IBM Model 1, as well as symmetric word alignment models such as intersection, union, or grow-diag.
}
\usage{
Symmetrization(file_train1, file_train2, 
               method = c("union", "intersection", "grow-diag"), 
               nrec = -1, iter = 4, minlen = 5, maxlen = 40, 
               ul_s = TRUE, ul_t = TRUE, removePt = TRUE, 
               all = FALSE, f1 = "fa", e1 = "en")
               
\method{print}{symmet}(x, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file_train1}{
the name of  source language file in training set.
}
  \item{file_train2}{
the name of  target language file in training set.
}
  \item{method}{
character string specifying the symmetric word alignment method (union, intersection, or grow-diag alignment).
}
  \item{nrec}{
the number of sentences to be read.If  -1, it considers all sentences.
}
  \item{iter}{
the number of  iterations for IBM Model 1.
}
  \item{minlen}{
a minimum length of sentences.
}
  \item{maxlen}{
a maximum length of sentences.
}
  \item{ul_s}{
logical. If \code{TRUE}, it will convert the first character of the source language's  sentences. When the source language is an Arabic script, it can be \code{FALSE}.
}
  \item{ul_t}{
logical. If \code{TRUE}, it will convert the first character of the target language's  sentences. When the target language is an Arabic script, it can be \code{FALSE}.
}
  \item{removePt}{
logical. If \code{TRUE}, it removes all punctuation marks.
}
  \item{all}{
logical. If \code{TRUE}, it considers the third argument (\code{lower = TRUE}) in \code{culf} function.
}
  \item{f1}{
it is a notation for the source language (default = \code{'fa'}).
}
  \item{e1}{
it is a notation for the target language (default = \code{'en'}).
}
\item{x}{
an object of class \code{"symmet"}.
  }
   \item{\dots}{ further arguments passed to or from other methods. }
}

\details{
Here, word alignment is not only a map of the target language to the source language and it is considered as a symmetric alignment such as union, or intersection, or grow-diag alignment.
}
\value{
\code{Symmetrization} returns an object of class \code{"symmet"}.

An object of class \code{"symmet"} is a list containing the following components:

\item{time }{A number. (in second/minute/hour)}
\item{method }{symmetric word alignment method (union, intersection, or grow-diag alignment).}
\item{alignment }{A list of alignment for each sentence pair .}
\item{aa }{a vector of source sentences.}
}
\references{
Koehn P. (2010), "Statistical Machine Translation.",
Cambridge University, New York.

\url{http://statmt.org/europarl/v7/bg-en.tgz}
}
\author{
Neda Daneshgar and Majid Sarmad.
}
\note{
Note that we have a memory restriction and just special computers with high cpu and big ram can allocate the vectors of this function. Of course, it depends on corpus size.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{word_alignIBM1}}
}
\examples{
# Since the extraction of  bg-en.tgz in Europarl corpus is time consuming, 
# so the aforementioned unzip files have been temporarily exported to 
# http://www.um.ac.ir/~sarmad/... .

\dontrun{

S1 = Symmetrization ('http://www.um.ac.ir/~sarmad/word.a/euro.bg',
                     'http://www.um.ac.ir/~sarmad/word.a/euro.en',
                      nrec = 200, ul_s = TRUE)
                      
S2 = Symmetrization ('http://www.um.ac.ir/~sarmad/word.a/euro.bg',
                     'http://www.um.ac.ir/~sarmad/word.a/euro.en',
                      nrec = 200, ul_s = TRUE, method = 'grow-diag')
}
}