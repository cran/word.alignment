\name{prepare.data}
\alias{prepare.data}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Initial Preparations of Bitext before the Word Alignment and the Evaluation of Word Alignment Quality
}
\description{
For a given Sentence-Aligned Parallel Corpus, it prepars sentence pairs as an input for \code{\link{align.ibm1}} and \code{\link{evaluation}} functions in this package.
}
\usage{
prepare.data(file.sorc, file.trgt, n = -1L, 
             encode.sorc = 'unknown' , encode.trgt = 'unknown', 
             min.len = 5, max.len = 40, remove.pt = TRUE, word.align = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file.sorc}{
the name of source language file.
}
  \item{file.trgt}{
the name of target language file.
}
  \item{n}{
the number of sentences to be read.If  -1, it considers all sentences.
}
\item{encode.sorc}{
encoding to be assumed for the source language. If the value is "latin1" or "UTF-8" it is used to mark character strings as known to be in Latin-1 or UTF-8. For more details please see \code{\link{scan}} function.	 
} 
\item{encode.trgt}{
encoding to be assumed for the target language. If the value is "latin1" or "UTF-8" it is used to mark character strings as known to be in Latin-1 or UTF-8. For more details please see \code{\link{scan}} function.	 
} 
  \item{min.len}{
a minimum length of sentences.
}
  \item{max.len}{
a maximum length of sentences.
}
  \item{remove.pt}{
logical. If \samp{TRUE}, it removes all punctuation marks.
}   
  \item{word.align}{
logical. If \samp{FALSE}, it divides each sentence into its words. Results can be used in \code{\link{align.symmet}}, \code{\link{cross.table}}, \code{\link{align.test}} and \code{\link{evaluation}} functions. 
}
}
\details{
It balances between source and target language as much as possible. For example, it removes extra blank sentences and equalization sentence pairs. Also, using \code{\link{nfirst2lower}} function, it converts the first letter of each sentence into lowercase. Moreover, it removes  short and long sentences.
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
\code{\link{evaluation}}, \code{\link{nfirst2lower}}, \code{\link{align.ibm1}}, \code{\link{scan}}
}
\examples{
# Since the extraction of  bg-en.tgz in Europarl corpus is time consuming, 
# so the aforementioned unzip files have been temporarily exported to 
# http://www.um.ac.ir/~sarmad/... .
\dontrun{

aa1 = prepare.data ('http://www.um.ac.ir/~sarmad/word.a/euro.bg',
                   'http://www.um.ac.ir/~sarmad/word.a/euro.en', 
                    n = 20, encode.sorc = 'UTF-8')
 
aa2 = prepare.data ('http://www.um.ac.ir/~sarmad/word.a/euro.bg',
                   'http://www.um.ac.ir/~sarmad/word.a/euro.en', 
                    n = 20, encode.sorc = 'UTF-8', word.align = FALSE)
                   
aa3 = prepare.data ('http://www.um.ac.ir/~sarmad/word.a/euro.bg',
                   'http://www.um.ac.ir/~sarmad/word.a/euro.en', 
                    n = 20, encode.sorc = 'UTF-8', remove.pt = FALSE)
}
}