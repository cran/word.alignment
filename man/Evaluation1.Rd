\name{Evaluation1}
\alias{Evaluation1}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Evaluation of Word Alignment Quality
}
\description{
It measures Precision, Recall, AER, and F_measurs metrics to evaluate word alignment quality.
}
\usage{
Evaluation1(file_train1, file_train2, nrec = -1, 
            tst.set_sorc, tst.set_trgt, nlen = -1, 
            minlen1 = 5, maxlen1 = 40, minlen2 = 5, 
            maxlen2 = 40, ul_s = FALSE, ul_t = TRUE, 
            removePt = TRUE, all = FALSE, iter = 3, 
            method = c("fix", "Excel"), agn = c("an.agn", "my.agn"), 
            null.tokens = TRUE, excel1 = "gold.xlsx", excel2 = "align.xlsx",
            fixfile_gld = NULL, fixfile_agn = NULL, dtfile = NULL, 
            f1 = "fa", e1 = "en", alpha = 0.5)
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
number of sentences in the training set to be read. If  -1, it considers all sentences.
}
  \item{tst.set_sorc}{
the name of source language file in test set.
}
  \item{tst.set_trgt}{
the name of target language file in test set.
}
  \item{nlen}{
number of sentences in the test set to be read. If  -1, it considers all sentences.
}
  \item{minlen1}{
a minimum length of sentences in training set.
}
  \item{maxlen1}{
a maximum length of sentences in training set.
}
  \item{minlen2}{
a minimum length of sentences in test set.
}
  \item{maxlen2}{
a maximum length of sentences in test set.
}
  \item{ul_s}{
logical. If \samp{TRUE}, it will convert the first character of source language's  sentences. When source language is an Arabic script, it can be \samp{FALSE}.
}
  \item{ul_t}{
logical. If \samp{TRUE}, it will convert the first character of target language's  sentences. When target language is an arabic script, it can be \samp{FALSE}.
}
  \item{removePt}{
logical. If \samp{TRUE}, it removes all punctuation marks.
}
  \item{all}{
logical. If \samp{TRUE}, it considers the third argument (\samp{lower = TRUE}) in \samp{culf} function.
}
  \item{iter}{
number of  iterations for IBM Model 1.
}
  \item{method}{
character string including two values. If \samp{"fix"}, it uses \samp{fix.gold} function to achieve gold standard. If \samp{"Excel"}, it uses \samp{consExcel} function to achieve gold standard.
}
  \item{agn}{
character string including two values. If \samp{"my.agn"}, the user wants to evaluate one-to-many word alignment using the \samp{word_alignIBM1} function in this package. If \samp{"an.agn"}, the user applies another software or even another method to word alignment.
}
  \item{null.tokens}{
logical. \samp{TRUE} means that the gold standard alignment is constructed based on "null tokens". 
}
  \item{excel1}{
the name of the excel file for gold standarad.
}
  \item{excel2}{
the name of the excel file for alignment.
}
  \item{fixfile_gld}{
if \samp{NULL},  it is the first time to build a gold standard using \samp{fix.gold} function. In this case, the function will automatically save the created matrices of gold standard with an informative name which is a combination of the  sentence number and 'RData'. As an example "1.RData", "2.RData" and ... . Note that, the aforementioned name must not be changed. For  next times, it is sufficient to set \samp{fixfile_gld} to any character, e.g. "a", "textfile" or "myproject".
}
  \item{fixfile_agn}{
it is similar to \samp{fixfile_gld}, but when we use some other software or method to create alignment  instead of creating gold standard.
}
  \item{dtfile}{
to run this function for the first time, it must be assigned to \samp{NULL}. In this case, the function will automatically save required codes of \samp{word_alignIBM1} function with a name which is a combination of \samp{f1}, \samp{e1}, \samp{nrec} and \samp{iter} as "f1.e1.nrec.iter.RData".
Note that, the abovementioned name must not been changed. For  next times, it is sufficient to set \samp{dtfile} to any character, e.g. "a", "textfile" or "myproject".
}
  \item{f1}{
it is an abbreviation of source language (default = \samp{'fa'}).
}
  \item{e1}{
t is an abbreviation of target language (default = \samp{'en'})
}
  \item{alpha}{
is a parameter that sets the trade-off between Precision and Recall.
}
}
\details{
To evaluate word alignment quality, we need to a "reference alignment" (a gold standard for the word alignment) of a test set. Two methods to enter this gold standard is considered. When \samp{method = "fix"}, means that \samp{fix.gold} function is called and the user should press  'Enter' to continue and edit the matrix to enter Sure/Possible alignments (Sure=1,Possible=2). Furthermore, when the user applies another software or even another method to word alignment, he/she should set \samp{agn = "an.agn"}, and he/she based on another word alignment's  results should press'Enter' to continue and edit the matrix to enter 3 for alignment. (Note that for each sentence pair, one matrix is created.) 

If  \samp{method = "Excel"}, means that the created excel file of \samp{consExcel} has been used. In this method, the aforementioned excel file should be completed by an expert with codes 1 or 2 for Sure or Possible alignments first and then this excel file named excel1 (as a default: "gold.xlsx") is set as an input argument. Moreover, to evaluate word alignment quality using another software or even another method, the user can be used excel2 file (as a default: "align.xlsx") that had been completed by 3 for alignments. 
}
\value{
A list.
%%  If it is a LIST, use
 \item{Recall }{A decimal number.}
 \item{Precision}{A decimal number.}
 \item{AER }{A decimal number.}
 \item{F_measure1}{A decimal number.}
 \item{F_measure2}{A decimal number.}
}
\references{
Fraser F., Marcu D. (2007), "MeasuringWord Alignment Quality for
Statistical Machine Translation.", Computational Linguistics, 33(3), 293-303.

Koehn P. (2010), "Statistical Machine Translation.",
Cambridge University, New York.

Och F., Ney H.(2003)."A Systematic Comparison Of Various Statistical Alignment Models.", 2003 Association for Computational Linguistics, J03-1002, 29(1).

Wang X. "Evaluation of Two Word Alignment Systems.", Final Thesis, Department of Computer and Information Science.
}
\author{
Neda Daneshgar and Majid Sarmad.
}
\note{
Note that we have a memory restriction and just special computers with high cpu and big ram can allocate the vectors of this function. Of course, it depends on corpus size.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{word_alignIBM1}, \code{fix.gold}, \code{consExcel}
}