\name{cons.agn}
\alias{cons.agn}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Constructing  Cross Tables of the Source Language Words vs the Target Language Words of Sentence Pairs
}
\description{
It is a function to create the cross tables of the source language words vs the target language words of sentence pairs  as the gold standard or as the alignment matrix of another software. For the gold standard, the created cross table is filled by an expert. He/she sets '1' for Sure alignments and '2' for Possible alignments in cross between the source and the target words. For alignment results of another software, '1' in cross between each aligned source and target words is set by the user.

It works with two formats: 

For the first format, it constructs a cross table  of the source language words vs the target language words of a given sentence pair.  Then, after filling as mentioned above sentence by sentence, it builds a list of cross tables and finally, it saves the created list as "file_align.RData". 

In the second format, it creates an excel file with \code{nrec} sheets. Each sheet includes a cross table of the two language words related each sentence pair.  The file is as "file_align.xlsx". The created file  to be filled as mentioned above.
}
\usage{
cons.agn(tst.set_sorc, tst.set_trgt, nrec = -1, 
        encode.sorc = 'unknown', encode.trgt = 'unknown', 
        minlen = 5, maxlen = 40, removePt = TRUE, 
        all = FALSE, null.tokens = TRUE, Format = c('R', 'Excel'), 
        file_align = 'alignment')
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tst.set_sorc}{
the name of source language file in test set.
}
  \item{tst.set_trgt}{
the name of target language file in test set.
}
  \item{nrec}{
the number of sentences to be read. If  -1, it considers all sentences.
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
  \item{removePt}{
logical. If \code{TRUE}, it removes all punctuation marks.
}
  \item{all}{
logical. If \code{TRUE}, it considers the third argument (\code{lower = TRUE}) in \code{\link{culf}} function.
}
  \item{null.tokens}{
logical. If \code{TRUE},  "null" is added at the first of each source and target sentence, when we use R format.
}
  \item{Format}{
character string including two values. If \code{R}, it creates a cross table of the source language words vs the target language words of a given sentence pair. Then, it constructs a list of them. If \code{Excel}, it makes an excel file with \code{nrec} sheets of a test set including the source and the target languages. Each sheet  includes the words of the source sentence in its first rows and the words of the target sentence in its first columns.
}
  \item{file_align}{
the output file name. %%     ~~Describe \code{file_align} here~~
}
}
\value{
an RData object as "file_align.RData" or an excel file as "file_align.xlsx".
%%  If it is a LI, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
Holmqvist M., Ahrenberg L. (2011), "A Gold Standard for English-Swedish Word Alignment.", NODALIDA 2011 Conference Proceedings, 106 - 113.

Och F., Ney H.(2003), "A Systematic Comparison Of Various Statistical Alignment Models.", 2003 Association for Computational Linguistics, J03-1002, 29(1).
}
\author{
Neda Daneshgar and Majid Sarmad.
}
\note{
If you have not the non-ascii problem, you can set \code{Format} as \code{'R'}. 

If ypu assign \code{Format} to \code{'Excel'}, it is necessary to bring two notes into consideration. The first note is that in order to use the created excel file for \code{\link{Evaluation1}} function, don't forget to use \code{\link{ExcelToR}} function to convert the excel file into required R format. The second note focouses on this:
ocassionally, there is a problem with 'openxlsx' package which is used in the function and it might be solved by  'installr::install.rtools() on Windows'.
}
\seealso{
\code{\link{Evaluation1}}, \code{\link{ExcelToR}}, \code{\link{scan}}
} 
%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
\dontrun{

cons.agn('http://www.um.ac.ir/~sarmad/word.a/source1.txt',
          'http://www.um.ac.ir/~sarmad/word.a/target1.txt',
           nrec = 5, encode.sorc = 'UTF-8')

cons.agn('http://www.um.ac.ir/~sarmad/word.a/source1.txt',
          'http://www.um.ac.ir/~sarmad/word.a/target1.txt', 
           nrec = 5, encode.sorc = 'UTF-8', Format = 'Excel')
}
}