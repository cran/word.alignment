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
mydictionary (file_train1,file_train2, 
              nrec = -1, iter = 15, prob = 0.8, 
              minlen=5, maxlen = 40, ul_s = FALSE, ul_t = TRUE, 
              lang1 = 'Farsi', lang2 = 'English', removePt = TRUE, 
              dtfile = NULL, f1 = 'fa', e1 = 'en')
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
number of sentences to be read.If  -1, it considers all sentences.
}
  \item{iter}{
number of  iterations for IBM Model 1.
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
  \item{ul_s}{
logical. If \samp{TRUE}, it will convert the first character of target language's  sentences. When source language is an Arabic script, it can be \samp{FALSE}.
}
  \item{ul_t}{
logical. If \samp{TRUE}, it will convert the first character of source language's sentences. When target language is an Arabic script, it can be \samp{FALSE}.
}
  \item{lang1}{
source language's name in mydictionary.
}
  \item{lang2}{
traget language's name in mydictionary.
}
  \item{removePt}{
logical. If \samp{TRUE}, it removes all punctuation marks.
}
  \item{dtfile}{
if \samp{NULL}, we  already did not save data.table (dd1) and it has to built. If an address exists,  means that data.table (dd1)  has already saved  and we want to use it.
}
  \item{f1}{
it is a notation for the source language (default = \samp{'fa'}).
}
  \item{e1}{
it is a notation for the target language (default = \samp{'en'}).
}
}
\details{
The results depend on the corpus. As an example, we used English-Persian parallel corpus named Mizan which consists of more than 1,000,000 sentence pairs with a size of 170 Mb. For the 10,000 first sentences, we have a nice dictionary. It just takes 1.356784 mins using an usual computer. The results can be found at 

\code{http://www.um.ac.ir/~sarmad/word.a/mydictionary.pdf}
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

http://statmt.org/europarl/v7/bg-en.tgz
}
\author{
Neda Daneshgar and Majid Sarmad.
}
\note{
Note that we have a memory restriction and just special computers with high cpu and big ram can allocate the vectors of this function. Of course, it depends on corpus size.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
#Since the extraction of  bg-en.tgz in Europarl corpus is time consuming, 
#so the aforementioned unzip files have been exported to http://www.um.ac.ir/~sarmad/... .

\dontrun{

dic1 = mydictionary ('http://www.um.ac.ir/~sarmad/word.a/euro.bg',
                     'http://www.um.ac.ir/~sarmad/word.a/euro.en', 
                      nrec = 2000, ul_s = TRUE, lang1 = 'BULGARIAN')
              
dic2 = mydictionary ('http://www.um.ac.ir/~sarmad/word.a/euro.bg',
                     'http://www.um.ac.ir/~sarmad/word.a/euro.en', 
                      nrec = 2000, ul_s = TRUE, lang1 = 'BULGARIAN',
                      removePt = FALSE)
}              
}
