\name{evaluation}
\alias{evaluation}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Evaluation of Word Alignment Quality
}
\description{
It measures Precision, Recall, AER, and F_measurs metrics to evaluate the quality of word alignment.
}
\usage{
evaluation(file.gold = 'gold.RData', 
           file.align = 'alignment.-1.3.RData', 
           agn = c('my.agn','an.agn'), alpha = 0.3)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file.gold}{
the gold standarad file name.}
  
  \item{file.align}{
the alignment file name.
}
\item{agn}{
character string including two values. If \code{"my.agn"}, the user wants to evaluate one-to-many word alignment using the \code{align.ibm1} function in this package. If \code{"an.agn"}, the user wants to evaluate word alignment results which are obtained by another software.
}
  \item{alpha}{
is a parameter that sets the trade-off between Precision and Recall.
}
}
\details{
To evaluate word alignment quality, we need to a "reference alignment" (a gold standard for the word alignment) of a test set.
In order to read the gold into RData format and to compare it with the word alignment results, the gold standard file name must be set in \code{file.gold}. 
}
\value{
A list.
%%  If it is a LIST, use
 \item{Recall }{A decimal number.}
 \item{Precision}{A decimal number.}
 \item{AER }{A decimal number.}
 \item{F_measure.PS}{A decimal number.}
 \item{F_measure.S}{A decimal number.}
}
\references{
Fraser F., Marcu D. (2007), "MeasuringWord Alignment Quality for
Statistical Machine Translation.", Computational Linguistics, 33(3), 293-303.

Koehn P. (2010), "Statistical Machine Translation.",
Cambridge University, New York.

Och F., Ney H.(2003)."A Systematic Comparison Of Various Statistical Alignment Models.", 2003 Association for Computational Linguistics, J03-1002, 29(1).

Wang X. "Evaluation of Two Word Alignment Systems.", Final Thesis, Department of Computer and Information Science.
}
\author{
Neda Daneshgar and Majid Sarmad.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{cross.table}}, \code{\link{align.test}}, \code{\link{align.ibm1}}
}
