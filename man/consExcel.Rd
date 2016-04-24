\name{consExcel}
\alias{consExcel}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Create an Excel File from Two Sentences
}
\description{
It creates an excel file from two sentences of two languages to help the user for constructing a gold standard such that he/she can set 1 or 2 for Sure or Possible alignments.
}
\usage{
consExcel(tst.set_sorc, tst.set_trgt, 
          method = c("gold", "aligns"), 
          out1 = "gold.xlsx", out2 = "align.xlsx", 
          nrec = -1, minlen = 5, maxlen = 40, ul_s = FALSE, 
          ul_t = TRUE, removePt = TRUE, all = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tst.set_sorc}{
the name of source language file in test set.
}
  \item{tst.set_trgt}{
the name of target language file in test set.
}
  \item{method}{
character string including two values. If \samp{"gold"}, it creates a separated excel file of test set to fill up its sheets with 1|2 for Sure|Possible alignment. If \samp{"aligns"}, it creates a separated excel file of test set to fill up its sheets with '3' as an alignment.
}
  \item{out1}{
the name of the excel file for gold standard.
}
  \item{out2}{
the name of the excel file for alignment.
}
  \item{nrec}{
number of sentences to be read. If  -1, it considers all sentences.
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
logical. If \samp{TRUE}, it will convert the first character of target language's  sentences. When target language is an Arabic script, it can be \samp{FALSE}.
}
  \item{removePt}{
logical. If \samp{TRUE}, it removes all punctuation marks.
}
  \item{all}{
logical. If \samp{TRUE}, it considers the third argument (\samp{lower = TRUE}) in \samp{culf} function.
}
}
\details{
The first step for evaluation of word alignment quality is creating a gold standard. This function makes an excel file with \samp{nrec} sheets of a test set including source and target languages. Each sheet  includes the words of the source sentence in its first rows and the words of the target sentence in its first columns. To create a gold standard, it can be filled by Sure|Possible alignments (Sure = 1, Possible = 2).

Sometimes, the user calculates word alignments using some other software or  method and he/she wants to evaluate such alignment with this package. So, this function can help him/her in this way, it creates a separated excel file in "out2.xlsx" (as a default: "align.xlsx") and it can be filled by number 3 for alignments.
}
\value{
One or two excel file in "out1" or "out2" file.
%%  If it is a LIST, use
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
If you have not the non-ascii problem, you can use \samp{fix.gold} function instead. 

Ocassionally, there is a problem with "openxlsx" package which is used in the function and it might solved by  "installr::install.rtools() on Windows".
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{fix.gold}
}
\examples{
\dontrun{

consExcel("http://www.um.ac.ir/~sarmad/word.a/source1.txt",
          "http://www.um.ac.ir/~sarmad/word.a/target1.txt",
           nrec = 5)

consExcel("http://www.um.ac.ir/~sarmad/word.a/source1.txt",
          "http://www.um.ac.ir/~sarmad/word.a/target1.txt", 
           nrec = 5, method = "aligns")
}
}