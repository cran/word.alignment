\name{word_alignIBM1}
\alias{word_alignIBM1}

\alias{print.alignment}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Computing One-to-Many Word Alignment Using IBM Model 1 for a Given Parallel Corpus
}
\description{
For a given sentence-aligned parallel corpus, it aligns words in each sentence pair. Moreover, it calculates expected length and vocabulary size of each language (source and taget language) and also shows word translation probability as a data.table.
}
\usage{
word_alignIBM1(file_train1, file_train2, 
              nrec = -1, iter = 4, minlen = 5, 
              maxlen = 40, ul_s = FALSE, ul_t = TRUE, 
              removePt = TRUE, all = FALSE, display = c("word1", "number"), 
              dtfile = NULL, f1 = "fa", e1 = "en", sym = FALSE, input = FALSE)

\method{print}{alignment}(x, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file_train1}{
the name of source language file in training set.
}
  \item{file_train2}{
the name of the target language file in training set.
}
  \item{nrec}{
number of sentences to be read. If  -1, it considers all sentences.
}
  \item{iter}{
number of iterations for IBM Model 1.
}
  \item{minlen}{
a minimum length of sentences.
}
  \item{maxlen}{
a maximum length of sentences.
}
  \item{ul_s}{
logical. If \samp{TRUE}, it will convert the first character of source language's  sentences. When source language is an Arabic script, it should be \samp{FALSE}.
}
  \item{ul_t}{
logical. If \samp{TRUE}, it will convert the first character of target language's  sentences. When target language is an Arabic script, it should be \samp{FALSE}.
}
  \item{removePt}{
logical. If \samp{TRUE}, it removes all punctuation marks.
}
  \item{all}{
logical. If \samp{TRUE}, it considers the third argument (\samp{lower = TRUE}) in \samp{culf} function.
}
  \item{display}{
character string including two values. If \samp{'word1'}, alignments are exhibited as words. If \samp{'number'}, alignments exhibits as numbers.
}
  \item{dtfile}{
to run this function for the first time, it must be assigned to \samp{NULL}. In this case, the function will automatically save required data.table (it is necessary for obtaining MLE of IBM Model 1's parameters.) with a name which is a combination of \samp{f1}, \samp{e1}, \samp{nrec} and \samp{iter} as "f1.e1.nrec.iter.RData".
Note that, the abovementioned name must not be changed. For next times, it is sufficient to set \samp{dtfile} to any character, e.g. "a", "textfile" or "myproject".  
}
  \item{f1}{
it is a notation for the source language (default = \samp{'fa'}).
}
  \item{e1}{
it is a notation for the target language (default = \samp{'en'}).
}
  \item{sym}{
logical. If \samp{TRUE}, the output can be used by \samp{Symmetrization} function.
}
  \item{input}{
logical. If \samp{TRUE}, the output can be used by \samp{mydictionary} and \samp{Evaluation1} functions.
}
\item{x}{
an object of class \code{"alignment"}.
  }
  \item{\dots}{ further arguments passed to or from other methods. }
}
\details{
Here, word alignment is a map of target language to source language. 

The results depend on the corpus. As an example, we have used English-Persian parallel corpus named Mizan which consists of more than 1,000,000 sentence pairs with a size of 170 Mb. If all sentences are considered, it takes about 50.96671 mins using a computer with cpu: intel Xeon X5570 2.93GHZ and Ram: 8*8 G = 64 G and word alignment is good. But for the 10,000 first sentences, the word alignment might not be good. In fact, it is sensitive to the original translation type (lexical or conceptual). The results can be found at 

\code{http://www.um.ac.ir/~sarmad/word.a/example_wordalignIBM1.pdf}
}
\value{
\code{word_alignIBM1} returns an object of class \code{"alignment"}.

 if  \samp{sym = TRUE}
  \item{ef}{A list of integer vectors.} 
 
 if  \samp{input = TRUE}
  \item{dd1}{A data.table}
 
 if \samp{sym = FALSE}  and  \samp{input = FALSE}
  \item{n1}{An integer.}
  \item{n2}{An integer.}
  \item{time }{A number. (in second/minute/hour)}
  \item{iterIBM1 }{An integer.}
  \item{expended_l_source }{A non-negative real number.}
  \item{expended_l_target }{A non-negative real number.}
  \item{VocabularySize_source }{An integer.}
  \item{VocabularySize_target }{An integer.}
  \item{word_translation_prob }{A data.table.}
  \item{word_align }{A list of one-to-many word alignment for each sentence pair.}
  \item{aa}{A matrix (n*2), where \samp{n} is the number of remained sentence pairs after preprocessing.}
}
\references{
Koehn P. (2010), "Statistical Machine Translation.",
Cambridge University, New York.

Lopez A. (2008), "Statistical Machine Translation.", ACM Computing Surveys, 40(3).

Peter F., Brown J. (1990), "A Statistical
Approach to Machine Translation.", Computational Linguistics, 16(2), 79-85.

Supreme Council of Information and Communication Technology. (2013), Mizan English-Persian Parallel Corpus. Tehran, I.R. Iran. Retrieved from http://dadegan.ir/catalog/mizan.

http://statmt.org/europarl/v7/bg-en.tgz
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
\code{Evaluation1}, \code{Symmetrization}, \code{mydictionary}
}
\examples{
#Since the extraction of  bg-en.tgz in Europarl corpus is time consuming, 
#so the aforementioned unzip files have been exported to http://www.um.ac.ir/~sarmad/... .
\dontrun{

w1 = word_alignIBM1 ('http://www.um.ac.ir/~sarmad/word.a/euro.bg',
                     'http://www.um.ac.ir/~sarmad/word.a/euro.en',
                      nrec = 3, ul_s = TRUE)
                 
w2 = word_alignIBM1 ('http://www.um.ac.ir/~sarmad/word.a/euro.bg',
                     'http://www.um.ac.ir/~sarmad/word.a/euro.en',
                      nrec = 3, ul_s = TRUE, removePt = FALSE)
}
}