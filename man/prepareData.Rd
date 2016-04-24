\name{prepareData}
\alias{prepareData}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Initial Preparations of Bitext before Word Alignment and Evaluation
}
\description{
For a given Sentence-Aligned Parallel Corpus, it prepars sentence pairs as an input for \samp{word_alignIBM1} and \samp{Evaluation1} functions in this package.
}
\usage{
prepareData(file1, file2, 
           nrec = -1, minlen = 5, maxlen = 40, 
           ul_s = FALSE, ul_t = TRUE, all = FALSE, 
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
number of sentences to be read.If  -1, it considers all sentences.
}
  \item{minlen}{
a minimum length of sentences.
}
  \item{maxlen}{
a maximum length of sentences.
}
  \item{ul_s}{
logical. If \samp{TRUE}, it will convert the first character of source language's  sentences. When source language is an Arabic script, it can be \samp{FALSE}.
}
  \item{ul_t}{
logical. If \samp{TRUE}, it will convert the first character of target language's  sentences. When target language is a right-to-left, it can be \samp{FALSE}.
}
 \item{all}{
logical. If \samp{TRUE}, it considers the third argument (\samp{lower = TRUE}) in \samp{culf} function.
}
  \item{removePt}{
logical. If \samp{TRUE}, it removes all punctuation marks.
}   
  \item{word_align}{
logical. If \samp{FALSE}, it divides each sentence into its words. Results can be used in \code{Symmetrization}, \code{fix.gold}, \code{consExcel} and \code{Evaluation1} functions. 
}
}
\details{
It balances between source and target language as much as possible. For example, it removes extra blank sentences and equalization sentence pairs. Also, using \samp{culf} function, it converts the first letter of each sentence into lowercase. Moreover, it removes  short and long sentences.
}
\value{
A list.
  
 if  \samp{word_align = TRUE}
   \item{len1}{An integer.}
   \item{aa}{A matrix (n*2), where \samp{n} is the number of remained sentence pairs after preprocessing.}
 
 if  \samp{word_align = TRUE} 
   \item{initial }{An integer.}
   \item{used }{An integer.}
   \item{source.tok }{A list of words for each source sentence.}
   \item{target.tok }{A list of words for each target sentence.}
}
\references{
Koehn P. (2010), "Statistical Machine Translation.",
Cambridge University, New York.
}
\author{
Neda Daneshgar and Majid Sarmad.
}
\note{
Note that if there is a few proper nouns in the parallel corpus, we suggest you to set \samp{all=TRUE} to convert all text into lowercase.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{Evaluation1}, \code{culf}, \code{RmTokenizer}, \code{word_alignIBM1}
}
\examples{
\dontrun{

aa1 = prepareData ('http://www.um.ac.ir/~sarmad/word.a/euro.bg',
                   'http://www.um.ac.ir/~sarmad/word.a/euro.en', 
                    nrec = 20, ul_s = TRUE)

aa2 = prepareData ('http://www.um.ac.ir/~sarmad/word.a/euro.bg',
                   'http://www.um.ac.ir/~sarmad/word.a/euro.en', 
                    nrec = 20, ul_s = TRUE, word_align = FALSE)
                   
aa3 = prepareData ('http://www.um.ac.ir/~sarmad/word.a/euro.bg',
                   'http://www.um.ac.ir/~sarmad/word.a/euro.en', 
                    nrec = 20, ul_s = TRUE, removePt = FALSE)
}
}