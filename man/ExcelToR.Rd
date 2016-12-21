\name{ExcelToR}
\alias{ExcelToR}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Converting Excel Files Into Required R Format 
}
\description{
This function converts the excel files into required R format.
}
\usage{
ExcelToR(file_align = "alignment.xlsx", null.tokens = TRUE, len = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file_align}{
the excel file name which we want to convert it into required R format. 
}
  \item{null.tokens}{
logical. If \samp{TRUE}, "null" is added at the first of each source sentence of the test set.
}
 \item{len}{
it is related to  the  number of sheets of the excel file. It is \samp{TRUE} or an integer. If \samp{TRUE}, the function is automatically compute the number of sheets. Otherwise, it must be assigned by the user.
}
}

\value{
an RData object as "file_align.RData".
}

\author{
Neda Daneshgar and Majid Sarmad.
}
\note{
Note that in order to use the created excel file for the function \code{\link{Evaluation1}}, don't forget to use \code{\link{ExcelToR}} function to convert the excel file into required \code{R} format.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{cons.agn}}, \code{\link{Evaluation1}}
}
\examples{
\dontrun{

ExcelToR("http://www.um.ac.ir/~sarmad/word.a/final.gold.xlsx")         
}
}