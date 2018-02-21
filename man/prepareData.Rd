\name{prepareData}
\alias{prepareData}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Initial Preparations of Bitext before the Word Alignment and the Evaluation of Word Alignment Quality
}
\description{
For a given Sentence-Aligned Parallel Corpus, it prepars sentence pairs as an input for \code{\link{word_alignIBM1}} and \code{\link{Evaluation1}} functions in this package.
}
\usage{
prepareData(file1, file2, nrec = -1, 
	   encode.sorc = 'unknown', encode.trgt = 'unknown',
           minlen = 5, maxlen = 40, all = FALSE, 
           removePt = TRUE, word_align = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file1}{
the name of source language file.
}
  \item{file2}{
the name of target language file.
}
  \item{nrec}{
the number of sentences to be read.If  -1, it considers all sentences.
}
\item{encode.sorc}{
encoding to be assumed for the source language. If the value is "latin1" or "UTF-8" it is used to mark character strings as known to be in Latin-1 or UTF-8. For more details please see \code{\link{scan}} function.	 
} 
\item{encode.trgt}{
encoding to be assumed for the target language. If the value is "latin1" or "UTF-8" it is used to mark character strings as known to be in Latin-1 or UTF-8. For more details please see \code{\link{scan}} function.	 
} 
  \item{minlen}{
a minimum length of sentences.
}
  \item{maxlen}{
a maximum length of sentences.
}
 \item{all}{
logical. If \samp{TRUE}, it considers the third argument (\samp{lower = TRUE}) in \code{\link{culf}} function.
}
  \item{removePt}{
logical. If \samp{TRUE}, it removes all punctuation marks.
}   
  \item{word_align}{
logical. If \samp{FALSE}, it divides each sentence into its words. Results can be used in \code{\link{Symmetrization}}, \code{\link{cons.agn}}, \code{\link{align_test.set}} and \code{\link{Evaluation1}} functions. 
}
}
\details{
It balances between source and target language as much as possible. For example, it removes extra blank sentences and equalization sentence pairs. Also, using \code{\link{culf}} function, it converts the first letter of each sentence into lowercase. Moreover, it removes  short and long sentences.
}
\value{
A list.
  
 if  \code{word_align = TRUE}
   \item{len1}{An integer.}
   \item{aa}{A matrix (n*2), where \samp{n} is the number of remained sentence pairs after preprocessing.}
 
 otherwise,
   \item{initial }{An integer.}
   \item{used }{An integer.}
   \item{source.tok }{A list of words for each the source sentence.}
   \item{target.tok }{A list of words for each the target sentence.}
}
\references{
Koehn P. (2010), "Statistical Machine Translation.",
Cambridge University, New York.
}
\author{
Neda Daneshgar and Majid Sarmad.
}
\note{
Note that if there is a few proper nouns in the parallel corpus, we suggest you to set \code{all=TRUE} to convert all text into lowercase.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{Evaluation1}}, \code{\link{culf}}, \code{\link{word_alignIBM1}}, \code{\link{scan}}
}
\examples{
# Since the extraction of  bg-en.tgz in Europarl corpus is time consuming, 
# so the aforementioned unzip files have been temporarily exported to 
# http://www.um.ac.ir/~sarmad/... .
\dontrun{

aa1 = prepareData ('http://www.um.ac.ir/~sarmad/word.a/euro.bg',
                   'http://www.um.ac.ir/~sarmad/word.a/euro.en', 
                    nrec = 20, encode.sorc = 'UTF-8')
 
aa2 = prepareData ('http://www.um.ac.ir/~sarmad/word.a/euro.bg',
                   'http://www.um.ac.ir/~sarmad/word.a/euro.en', 
                    nrec = 20, encode.sorc = 'UTF-8', word_align = FALSE)
                   
aa3 = prepareData ('http://www.um.ac.ir/~sarmad/word.a/euro.bg',
                   'http://www.um.ac.ir/~sarmad/word.a/euro.en', 
                    nrec = 20, encode.sorc = 'UTF-8', removePt = FALSE)
}
}