\name{align.ibm1}
\alias{align.ibm1}
\alias{align.symmet}
\alias{print.align}
\title{
Computing One-to-Many and Symmetric Word Alignment Using IBM Model 1 for a Given Sentence-Aligned Parallel Corpus
}
\description{
For a given sentence-aligned parallel corpus, it calculates source-to-target and target-to-source alignments using IBM Model 1, as well as symmetric word alignment models such as intersection, union, or grow-diag in each sentence pair. Moreover, it calculates the expected length and vocabulary size of each language (source and taget language) and also shows word translation probability as a data.table.
}
\usage{
align.ibm1(...,
              iter = 5, dtfile.path = NULL, 
              name.sorc = 'f',name.trgt = 'e', 
              result.file = 'result', input = FALSE)
align.symmet(file.sorc, file.trgt, 
             n = -1L, iter = 4, 
             method = c ('union', 'intersection', 'grow-diag'), 
             encode.sorc = 'unknown', encode.trgt = 'unknown', 
             name.sorc = 'f', name.trgt = 'e', ...)              
              
\method{print}{align}(x,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{

\item{file.sorc}{
the name of  source language file.
}
  \item{file.trgt}{
the name of  target language file.
}
\item{n}{
the number of sentences to be read.If  -1, it considers all sentences.
}
\item{iter}{
the number of iterations for IBM Model 1.
}

  \item{method}{
character string specifying the symmetric word alignment method (union, intersection, or grow-diag alignment).
}
  
\item{encode.sorc}{
encoding to be assumed for the source language. If the value is "latin1" or "UTF-8" it is used to mark character strings as known to be in Latin-1 or UTF-8. For more details please see \code{\link{scan}} function.   
} 
\item{encode.trgt}{
encoding to be assumed for the target language. If the value is "latin1" or "UTF-8" it is used to mark character strings as known to be in Latin-1 or UTF-8. For more details please see \code{\link{scan}} function.	 
} 
\item{name.sorc}{
it is a notation for the source language (default = \code{'f'}).
}
\item{name.trgt}{
it is a notation for the target language (default = \code{'e'}).   
} 
  
  \item{dtfile.path}{
if \code{NULL} (usually for the first time), a data.table will be created contaning cross words of all sentences with their matched probabilities. It saves into a file named as a combination of \code{name.sorc}, \code{name.trgt}, \code{nrec} and \code{iter} as 'name.sorc.name.trgt.n.iter.RData'.
If specific file name is set, it will be read and continue the rest of the function, i.e. : finding the word alignments.
}
  
\item{result.file}{
the output results file name.	 
} 
  \item{input}{
logical. If \code{TRUE}, the output can be used by \code{\link{bidictionary}} and \code{\link{align.test}} functions.
}
 
\item{x}{
an object of class \code{'align'}.
  }
\item{\dots}{further arguments passed to or from other methods and further arguments of function prepare.data.}
  
}
\details{
Here, word alignment is a map of the target language to the source language. 

The results depend on the corpus. As an example, we have used English-Persian parallel corpus named Mizan which consists of more than 1,000,000 sentence pairs with a size of 170 Mb. If all sentences are considered, it takes about 50.96671 mins using a computer with cpu: intel Xeon X5570 2.93GHZ and Ram: 8*8 G = 64 G and word alignment is good. But for the 10,000 first sentences, the word alignment might not be good. In fact, it is sensitive to the original translation type (lexical or conceptual). The results can be found at 

\url{http://www.um.ac.ir/~sarmad/word.a/example.align.ibm1.pdf}
}
\value{
\code{align.ibm1} and \code{align.symmet} returns an object of class \code{'align'}.

An object of class \code{'align'} is a list containing the following components:

If  \code{input = TRUE}
 \item{dd1}{A data.table.}
Otherwise, 
  \item{model}{'IBM1'}  
  \item{initial_n}{An integer.}
  \item{used_n}{An integer.}
  \item{time }{A number. (in second/minute/hour)}
  \item{iterIBM1 }{An integer.}
  \item{expended_l_source }{A non-negative real number.}
  \item{expended_l_target }{A non-negative real number.}
  \item{VocabularySize_source }{An integer.}
  \item{VocabularySize_target }{An integer.}
  \item{word_translation_prob }{A data.table.}
  \item{word_align }{A list of one-to-many word alignment for each sentence pair (it is as word by word).}
  \item{align_init}{One-to-many word alignment for the first three sentences.}
  \item{align_end}{One-to-many word alignment for the last three sentences.}
  \item{number_align }{A list of one-to-many word alignment for each sentence pair (it is as numbers).}
  \item{aa}{A matrix (n*2), where \code{n} is the number of remained sentence pairs after preprocessing.}
  \item{method }{symmetric word alignment method (union, intersection or grow-diag alignment).}
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
\code{\link{align.test}}, \code{\link{align.symmet}}, \code{\link{bidictionary}}, \code{\link{scan}} 
}
\examples{
# Since the extraction of  bg-en.tgz in Europarl corpus is time consuming, 
# so the aforementioned unzip files have been temporarily exported to 
# http://www.um.ac.ir/~sarmad/... .
\dontrun{

w1 = align.ibm1 ('http://www.um.ac.ir/~sarmad/word.a/euro.bg',
                     'http://www.um.ac.ir/~sarmad/word.a/euro.en',
                      n = 30, encode.sorc = 'UTF-8')
                 
w2 = align.ibm1 ('http://www.um.ac.ir/~sarmad/word.a/euro.bg',
                     'http://www.um.ac.ir/~sarmad/word.a/euro.en',
                      n = 30, encode.sorc = 'UTF-8', remove.pt = FALSE)

S1 = align.symmet ('http://www.um.ac.ir/~sarmad/word.a/euro.bg',
                     'http://www.um.ac.ir/~sarmad/word.a/euro.en',
                      n = 200, encode.sorc = 'UTF-8')
                      
S2 = align.symmet ('http://www.um.ac.ir/~sarmad/word.a/euro.bg',
                     'http://www.um.ac.ir/~sarmad/word.a/euro.en',
                      n = 200, encode.sorc = 'UTF-8', method = 'grow-diag')

}
}