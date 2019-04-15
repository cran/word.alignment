\name{cross.table}
\alias{cross.table}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Constructing  Cross Tables of the Source Language Words vs the Target Language Words of Sentence Pairs
}
\description{
It is a function to create the cross tables of the source language words vs the target language words of sentence pairs  as the gold standard or as the alignment matrix of another software. For the gold standard, the created cross table is filled by an expert. He/she sets '1' for Sure alignments and '2' for Possible alignments in cross between the source and the target words. For alignment results of another software, '1' in cross between each aligned source and target words is set by the user.

It works with two formats: 

For the first format, it constructs a cross table  of the source language words vs the target language words of a given sentence pair.  Then, after filling as mentioned above sentence by sentence, it builds a list of cross tables and finally, it saves the created list as "file.align.RData". 

In the second format, it creates an excel file with \code{n} sheets. Each sheet includes a cross table of the two language words related each sentence pair.  The file is as "file.align.xlsx". The created file  to be filled as mentioned above.
}
\usage{
cross.table( ..., 
             null.tokens = TRUE, 
             out.format = c('rdata','excel'), 
             file.align = 'alignment')
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{...}{
Further agguments to be passed to \code{prepare.data} and \code{align.test}
}
 
  \item{null.tokens}{
logical. If \code{TRUE},  "null" is added at the first of each source and target sentence, when we use RData format.
}
  \item{out.format}{
a character string including two options.For \code{"rdata"} format, it constructs a cross table of the source language words vs the target language words of a given
sentence pair. Then, after filling it as mentioned in the description sentence
by sentence, it builds a list of cross tables and finally, it saves the created list as "file.align.RData". In the \code{"excel"} format, it creates
an excel file with n sheets. Each sheet includes a cross table of the two language
words related to each sentence pair. The file is as "file.align.xlsx". The
created file to be filled as mentioned in description.
}
  \item{file.align}{
the output file name. %%     ~~Describe \code{file_align} here~~
}
}
\value{
an RData object as "file.align.RData" or an excel file as "file.align.xlsx".
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
If you have not the non-ascii problem, you can set \code{out.format} as \code{'rdata'}. 

If ypu assign \code{out.format} to \code{'excel'}, it is necessary to bring two notes into consideration. The first note is that in order to use the created excel file for \code{\link{evaluation}} function, don't forget to use \code{\link{excel2rdata}} function to convert the excel file into required R format. The second note focouses on this:
ocassionally, there is a problem with 'openxlsx' package which is used in the function and it might be solved by  'installr::install.rtools() on Windows'.
}
\seealso{
\code{\link{evaluation}}, \code{\link{excel2rdata}}, \code{\link{scan}}
} 
%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
\dontrun{

cross.table('http://www.um.ac.ir/~sarmad/word.a/euro.bg',
           'http://www.um.ac.ir/~sarmad/word.a/euro.en',
           n = 10, encode.sorc = 'UTF-8')

cross.table('http://www.um.ac.ir/~sarmad/word.a/euro.bg',
           'http://www.um.ac.ir/~sarmad/word.a/euro.en', 
           n = 5, encode.sorc = 'UTF-8', out.format = 'excel')
}
}