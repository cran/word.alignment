\name{word_alignIBM1}
\alias{word_alignIBM1}
\alias{print.alignment}
\title{
Computing One-to-Many Word Alignment Using IBM Model 1 for a Given Parallel Corpus
}
\description{
For a given sentence-aligned parallel corpus, it aligns words in each sentence pair. Moreover, it calculates the expected length and vocabulary size of each language (source and taget language) and also shows word translation probability as a data.table.
}
\usage{
word_alignIBM1(file_train1, file_train2, 
              nrec = -1, iter = 4, minlen = 5, maxlen = 40, 
              ul_s = FALSE, ul_t = TRUE, removePt = TRUE, all = FALSE, 
              dtfile_path = NULL, f1 = "fa", e1 = "en", 
              result_file = "myResultIBM1", input = FALSE)

\method{print}{alignment}(x, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file_train1}{
the name of source language file in training set.
}
  \item{file_train2}{
the name of  target language file in training set.
}
  \item{nrec}{
the number of sentences to be read. If  -1, it considers all sentences.
}
  \item{iter}{
the number of iterations for IBM Model 1.
}
  \item{minlen}{
a minimum length of sentences.
}
  \item{maxlen}{
a maximum length of sentences.
}
  \item{ul_s}{
logical. If \code{TRUE}, it will convert the first character of the source language's  sentences. When the source language is an Arabic script, it should be \code{FALSE}.
}
  \item{ul_t}{
logical. If \code{TRUE}, it will convert the first character of the target language's  sentences. When the target language is an Arabic script, it should be \code{FALSE}.
}
  \item{removePt}{
logical. If \code{TRUE}, it removes all punctuation marks.
}
  \item{all}{
logical. If \code{TRUE}, it considers the third argument (\code{lower = TRUE}) in \code{\link{culf}} function.
}
  \item{dtfile_path}{
if \code{NULL} (usually for the first time), a data.table will be created contaning cross words of all sentences with their matched probabilities. It saves into a file named as a combination of \code{f1}, \code{e1}, \code{nrec} and \code{iter} as "f1.e1.nrec.iter.RData".

If specific file name is set, it will be read and continue the rest of the function, i.e. : finding the word alignments.
}
  \item{f1}{
it is a notation for the source language (default = \code{'fa'}).
}
  \item{e1}{
it is a notation for the target language (default = \code{'en'}).
}
  \item{result_file}{
the output results file name.
}
  \item{input}{
logical. If \code{TRUE}, the output can be used by \code{\link{mydictionary}} and \code{\link{align_test.set}} functions.
}
\item{x}{
an object of class \code{"alignment"}.
  }
  \item{\dots}{ further arguments passed to or from other methods. }
}
\details{
Here, word alignment is a map of the target language to the source language. 

The results depend on the corpus. As an example, we have used English-Persian parallel corpus named Mizan which consists of more than 1,000,000 sentence pairs with a size of 170 Mb. If all sentences are considered, it takes about 50.96671 mins using a computer with cpu: intel Xeon X5570 2.93GHZ and Ram: 8*8 G = 64 G and word alignment is good. But for the 10,000 first sentences, the word alignment might not be good. In fact, it is sensitive to the original translation type (lexical or conceptual). The results can be found at 

\url{http://www.um.ac.ir/~sarmad/word.a/example_wordalignIBM1.pdf}
}
\value{
\code{word_alignIBM1} returns an object of class \code{"alignment"}.

An object of class \code{"alignment"} is a list containing the following components:

If  \samp{input = TRUE}
 \item{dd1}{A data.table.}
Otherwise, 

  \item{n1}{An integer.}
  \item{n2}{An integer.}
  \item{time }{A number. (in second/minute/hour)}
  \item{iterIBM1 }{An integer.}
  \item{expended_l_source }{A non-negative real number.}
  \item{expended_l_target }{A non-negative real number.}
  \item{VocabularySize_source }{An integer.}
  \item{VocabularySize_target }{An integer.}
  \item{word_translation_prob }{A data.table.}
  \item{word_align }{A list of one-to-many word alignment for each sentence pair (it is as word by word).}
  \item{number_align }{A list of one-to-many word alignment for each sentence pair (it is as numbers).}
  \item{aa}{A matrix (n*2), where \code{n} is the number of remained sentence pairs after preprocessing.}
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
\code{\link{align_test.set}}, \code{\link{Symmetrization}}, \code{\link{mydictionary}}
}
\examples{
# Since the extraction of  bg-en.tgz in Europarl corpus is time consuming, 
# so the aforementioned unzip files have been temporarily exported to 
# http://www.um.ac.ir/~sarmad/... .
\dontrun{

w1 = word_alignIBM1 ('http://www.um.ac.ir/~sarmad/word.a/euro.bg',
                     'http://www.um.ac.ir/~sarmad/word.a/euro.en',
                      nrec = 30, ul_s = TRUE)
                 
w2 = word_alignIBM1 ('http://www.um.ac.ir/~sarmad/word.a/euro.bg',
                     'http://www.um.ac.ir/~sarmad/word.a/euro.en',
                      nrec = 30, ul_s = TRUE, removePt = FALSE)
}
}