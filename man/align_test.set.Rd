\name{align_test.set}
\alias{align_test.set}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Computing One-to-Many Word Alignment Using a Parallel Corpus for a Given Test Set 
}
\description{
For a given parallel corpus based on IBM Model 1, it aligns the words of a given sentence-aligned test set.
}
\usage{
align_test.set(file_train1, file_train2, 
              tst.set_sorc, tst.set_trgt, 
              nrec = -1, nlen = -1, 
              encode.sorc = 'unknown', encode.trgt = 'unknown',
              minlen1 = 5, maxlen1 = 40, minlen2 = 5, maxlen2 = 40, 
              removePt = TRUE, all = FALSE, null.tokens = TRUE, 
              iter = 3, f1 = 'fa', e1 = 'en', 
              dtfile_path = NULL, file_align = 'alignment')
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file_train1}{
the name of source language file in training set.
}
  \item{file_train2}{
the name of  target language file in training set.
}
  \item{tst.set_sorc}{
the name of source language file in test set.
}
  \item{tst.set_trgt}{
the name of target language file in test set.
}
\item{nrec}{
the number of sentences in the training set to be read. If  -1, it considers all sentences.
}
  \item{nlen}{
the number of sentences in the test set to be read. If  -1, it considers all sentences.
}
\item{encode.sorc}{
encoding to be assumed for the source language. If the value is "latin1" or "UTF-8" it is used to mark character strings as known to be in Latin-1 or UTF-8. For more details please see \code{\link{scan}} function.	 
} 
\item{encode.trgt}{
encoding to be assumed for the target language. If the value is "latin1" or "UTF-8" it is used to mark character strings as known to be in Latin-1 or UTF-8. For more details please see \code{\link{scan}} function.	 
} 
 \item{minlen1}{
a minimum length of sentences in training set.
}
  \item{maxlen1}{
a maximum length of sentences in training set.
}
  \item{minlen2}{
a minimum length of sentences in test set.
}
  \item{maxlen2}{
a maximum length of sentences in test set.
}
  \item{removePt}{
logical. If \code{TRUE}, it removes all punctuation marks.
}
  \item{all}{
logical. If \code{TRUE}, it considers the third argument (\code{lower = TRUE}) in \code{\link{culf}} function.
}
  \item{null.tokens}{
logical. If \code{TRUE}, "null" is added at the first of each source sentence of the test set.
}
  \item{iter}{
the number of  iterations for IBM Model 1.
}
  \item{f1}{
it is a notation for the source language (default = \code{'fa'}).
}
  \item{e1}{
it is a notation for the target language (default = \code{'en'}).
}
  \item{dtfile_path}{
if \code{NULL} (usually for the first time), a data.table will be created contaning cross words of all sentences with their matched probabilities. It saves into a file named as a combination of \code{f1}, \code{e1}, \code{nrec} and \code{iter} as "f1.e1.nrec.iter.RData".

If specific file name is set, it will be read and continue the rest of the function, i.e. : finding the word alignments for the test set.
}
  \item{file_align}{
the output results file name.
}
}
\details{
If \code{dtfile_path = NULL}, the following question will be asked:

"Are you sure that you want to run the word_alignIBM1 function (It takes time)? (Yes/ No: if you want to specify word alignment path, please press 'No'.)
}
\value{
an RData object as "file_align.nrec.iter.Rdata".
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
\code{\link{word_alignIBM1}}, \code{\link{Evaluation1}}, \code{\link{scan}}
}
\examples{
# Since the extraction of  bg-en.tgz in Europarl corpus is time consuming, 
# so the aforementioned unzip files have been temporarily exported to 
# http://www.um.ac.ir/~sarmad/... .
# In addition, in this example we use the first five sentence pairs of training set as the 
# test set.
\dontrun{

ats = align_test.set ('http://www.um.ac.ir/~sarmad/word.a/euro.bg',
                      'http://www.um.ac.ir/~sarmad/word.a/euro.en',  
                      'http://www.um.ac.ir/~sarmad/word.a/euro.bg',
                      'http://www.um.ac.ir/~sarmad/word.a/euro.en',
                       nrec = 100,nlen = 5, encode.sorc = 'UTF-8',)               
}
}
