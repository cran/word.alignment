\name{bidictionary}
\alias{bidictionary}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Building an Automatic Bilingual Dictionary
}
\description{
It builds an automatic bilingual dictionary of two languages based on given sentence-aligned parallel corpus.
}
\usage{
bidictionary (..., n = -1L, iter = 15, prob = 0.8,  
              dtfile.path = NULL, name.sorc = 'f', name.trgt = 'e')
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{...}{Further arguments to be passed to \code{prepare.data}.}
  \item{n}{Number of sentences to be read.}
  \item{iter}{
the number of  iterations for IBM Model 1.
}
  \item{prob}{
the minimum word translation probanility.
}
\item{dtfile.path}{
if \code{NULL} (usually for the first time), a data.table will be created contaning cross words of all sentences with their matched probabilities. It saves into a file named as a combination of \code{name.sorc}, \code{name.trgt}, \code{n} and \code{iter} as "f.e.n.iter.RData".

If specific file name is set, it will be read and continue the rest of the function, i.e. : finding dictionary of two given languages.
}
  \item{name.sorc}{
source language's name in mydictionary.
}
  \item{name.trgt}{
traget language's name in mydictionary.
}
 
}
\details{
The results depend on the corpus. As an example, we have used English-Persian parallel corpus named Mizan which consists of more than 1,000,000 sentence pairs with a size of 170 Mb. For the 10,000 first sentences, we have a nice dictionary. It just takes 1.356784 mins using an ordinary computer. The results can be found at 

\url{http://www.um.ac.ir/~sarmad/word.a/bidictionary.pdf}
}
\value{
A list.
%%  If it is a LIST, use
\item{time }{A number. (in second/minute/hour)}
 \item{number_input }{An integer.}
 \item{Value_prob}{A decimal number between 0 and 1.}
 \item{iterIBM1 }{An integer.}
 \item{dictionary }{A matrix.}
}
\references{
Supreme Council of Information and Communication Technology. (2013), Mizan English-Persian Parallel Corpus. Tehran, I.R. Iran. Retrieved from http://dadegan.ir/catalog/mizan.

\url{http://statmt.org/europarl/v7/bg-en.tgz}
}
\author{
Neda Daneshgar and Majid Sarmad.
}
\note{
Note that we have a memory restriction and just special computers with high cpu and big ram can allocate the vectors of this function. Of course, it depends on corpus size.

In addition, if \code{dtfile.path = NULL}, the following question will be asked:

"Are you sure that you want to run the align.ibm1 function (It takes time)? (Yes/ No: if you want to specify word alignment path, please press 'No'.)

}

%% ~Make other sections like Warning with \section{Warning }{....} ~
\seealso{
\code{\link{scan}}
}
\examples{
# Since the extraction of  bg-en.tgz in Europarl corpus is time consuming, 
# so the aforementioned unzip files have been temporarily exported to 
# http://www.um.ac.ir/~sarmad/... .

\dontrun{

dic1 = bidictionary ('http://www.um.ac.ir/~sarmad/word.a/euro.bg',
                     'http://www.um.ac.ir/~sarmad/word.a/euro.en', 
                      n = 2000, encode.sorc = 'UTF-8', 
                      name.sorc = 'BULGARIAN', name.trgt = 'ENGLISH')
              
dic2 = bidictionary ('http://www.um.ac.ir/~sarmad/word.a/euro.bg',
                     'http://www.um.ac.ir/~sarmad/word.a/euro.en', 
                      n = 2000, encode.sorc = 'UTF-8', 
                      name.sorc = 'BULGARIAN', name.trgt = 'ENGLISH',
                      remove.pt = FALSE)
}              
}