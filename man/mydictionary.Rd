\name{mydictionary}
\alias{mydictionary}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Building an Automatic Bilingual Dictionary
}
\description{
It builds an automatic bilingual dictionary of two languages based on given sentence-aligned parallel corpus.
}
\usage{
mydictionary(file_train1, file_train2, nrec = -1, 
             encode.sorc = 'unknown', encode.trgt = 'unknown', 
	         iter = 15, prob = 0.8, minlen = 5, maxlen = 40,
             lang1 = 'Farsi', lang2 = 'English', removePt = TRUE, 
             dtfile_path = NULL, f1 = 'fa', e1 = 'en', 
             result_file = 'mydictionaryResults')
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file_train1}{
the name of source language file in training set.
}
  \item{file_train2}{
the name of target language file in training set.
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
  \item{iter}{
the number of  iterations for IBM Model 1.
}
  \item{prob}{
the minimum word translation probanility.
}
  \item{minlen}{
a minimum length of sentences.
}
  \item{maxlen}{
a maximum length of sentences.
}
  \item{lang1}{
source language's name in mydictionary.
}
  \item{lang2}{
traget language's name in mydictionary.
}
  \item{removePt}{
logical. If \code{TRUE}, it removes all punctuation marks.
}
  \item{dtfile_path}{
if \code{NULL} (usually for the first time), a data.table will be created contaning cross words of all sentences with their matched probabilities. It saves into a file named as a combination of \code{f1}, \code{e1}, \code{nrec} and \code{iter} as "f1.e1.nrec.iter.RData".

If specific file name is set, it will be read and continue the rest of the function, i.e. : finding dictionary of two given languages.
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
}
\details{
The results depend on the corpus. As an example, we have used English-Persian parallel corpus named Mizan which consists of more than 1,000,000 sentence pairs with a size of 170 Mb. For the 10,000 first sentences, we have a nice dictionary. It just takes 1.356784 mins using an ordinary computer. The results can be found at 

\url{http://www.um.ac.ir/~sarmad/word.a/mydictionary.pdf}
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

In addition, if \code{dtfile_path = NULL}, the following question will be asked:

"Are you sure that you want to run the word_alignIBM1 function (It takes time)? (Yes/ No: if you want to specify word alignment path, please press 'No'.)

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

dic1 = mydictionary ('http://www.um.ac.ir/~sarmad/word.a/euro.bg',
                     'http://www.um.ac.ir/~sarmad/word.a/euro.en', 
                      nrec = 2000, encode.sorc = 'UTF-8', lang1 = 'BULGARIAN')
              
dic2 = mydictionary ('http://www.um.ac.ir/~sarmad/word.a/euro.bg',
                     'http://www.um.ac.ir/~sarmad/word.a/euro.en', 
                      nrec = 2000, encode.sorc = 'UTF-8', lang1 = 'BULGARIAN',
                      removePt = FALSE)
}              
}