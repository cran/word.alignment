\name{align.test}
\alias{align.test}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Computing One-to-Many Word Alignment Using a Parallel Corpus for a Given Test Set 
}
\description{
For a given parallel corpus based on IBM Model 1, it aligns the words of a given sentence-aligned test set.
}
\usage{
align.test(file.sorc, file.trgt, test.sorc, test.trgt, 
           n.train = -1, n.test = -1, minlen.train = 5, maxlen.train = 40, 
           minlen.test = 5, maxlen.test = 40, null.tokens = TRUE, 
           dtfile.path = NULL, file.align = 'alignment',
           name.sorc='f',name.trgt='e',iter = 3, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file.sorc}{
the name of source language file in training set.
}
  \item{file.trgt}{
the name of  target language file in training set.
}
  \item{test.sorc}{
the name of source language file in test set.
}
  \item{test.trgt}{
the name of target language file in test set.
}
\item{n.train}{
the number of sentences in the training set to be read. If  -1, it considers all sentences.
}
  \item{n.test}{
the number of sentences in the test set to be read. If  -1, it considers all sentences.
} 
 \item{minlen.train}{
a minimum length of sentences in training set.
}
  \item{maxlen.train}{
a maximum length of sentences in training set.
}
  \item{minlen.test}{
a minimum length of sentences in test set.
}
  \item{maxlen.test}{
a maximum length of sentences in test set.
}
  \item{null.tokens}{
logical. If \code{TRUE}, "null" is added at the first of each source sentence of the test set.
}
\item{dtfile.path}{
if \code{NULL} (usually for the first time), a data.table will be created contaning cross words of all sentences with their matched probabilities. It saves into a file named as a combination of \code{name.sorc}, \code{name.trgt}, \code{n} and \code{iter} as "f.e.n.iter.RData".

If specific file name is set, it will be read and continue the rest of the function, i.e. : finding the word alignments for the test set.
}
 \item{file.align}{
the output results file name.
}  
  \item{name.sorc}{
it is a notation for the source language (default = \code{'f'}).
}
  \item{name.trgt}{
it is a notation for the target language (default = \code{'e'}).
}
 \item{iter}{
the number of  iterations for IBM Model 1.
}
\item{...}{Further arguments to be passed to \code{prepare.data}.}
 
}
\details{
If \code{dtfile.path = NULL}, the following question will be asked:

"Are you sure that you want to run the align.ibm1 function (It takes time)? (Yes/ No: if you want to specify word alignment path, please press 'No'.)
}
\value{
an RData object as "file.align.n.iter.Rdata".
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
Koehn P. (2010), "Statistical Machine Translation.",
Cambridge University, New York.

Lopez A. (2008), "Statistical Machine Translation.", ACM Computing Surveys, 40(3).

Peter F., Brown J. (1990), "A Statistical
Approach to Machine Translation.", Computational Linguistics, 16(2), 79-85.

Supreme Council of Information and Communication Technology. (2013), Mizan English-Persian Parallel Corpus. Tehran, I.R. Iran. Retrieved from http://dadegan.ir/catalog/mizan.

\url{http://statmt.org/europarl/v7/bg-en.tgz}
}
\author{
Neda Daneshgar and Majid Sarmad.
}
\note{
Note that we have a memory restriction and so just special computers with a high
CPU and a big RAM can allocate the vectors of this function. Of course, it depends on the
corpus size. 
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{align.ibm1}}, \code{\link{evaluation}}, \code{\link{scan}}
}
\examples{
# Since the extraction of  bg-en.tgz in Europarl corpus is time consuming, 
# so the aforementioned unzip files have been temporarily exported to 
# http://www.um.ac.ir/~sarmad/... .
# In addition, in this example we use the first five sentence pairs of training set as the 
# test set.
\dontrun{

ats = align.test ('http://www.um.ac.ir/~sarmad/word.a/euro.bg',
                      'http://www.um.ac.ir/~sarmad/word.a/euro.en',  
                      'http://www.um.ac.ir/~sarmad/word.a/euro.bg',
                      'http://www.um.ac.ir/~sarmad/word.a/euro.en',
                       n.train = 100,n.test = 5, encode.sorc = 'UTF-8')               
}
}
