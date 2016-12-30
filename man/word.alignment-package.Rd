\name{word.alignment-package}
\alias{word.alignment-package}
\alias{word.alignment}
\docType{package}
\title{
Computing Word Alignment Using IBM Model 1 (and Symmetrization) for a Given Parallel Corpus and Its Evaluation
}
\description{
For a given Sentence-Aligned Parallel Corpus, it aligns words for each sentence pair. It considers one-to-many alignment in the function \code{\link{word_alignIBM1}}  and symmetric word alignment in the function \code{\link{Symmetrization}}. Moreover, it evaluates the quality of word alignment from \code{\link{word_alignIBM1}} function or from some other software in the function \code{\link{Evaluation1}}. It also builds an automatic bilingual dictionary of two languages using the given corpus in the function \code{\link{mydictionary}}.
}
\details{
\tabular{ll}{
Package: \tab word.alignment\cr
Type: \tab Package\cr
Version: \tab 1.0.4\cr
Date: \tab 2016-12-28\cr
License: \tab GPL (>= 2)\cr
}
}
\author{
Neda Daneshgar and Majid Sarmad.

Maintainer: Neda Daneshgar <ne_da978@stu-mail.um.ac.ir>
}
\references{
Fraser F., Marcu D. (2007), "Measuring Word Alignment Quality for
Statistical Machine Translation.", Computational Linguistics, 33(3), 293-303.

Koehn P. (2010), "Statistical Machine Translation.",
Cambridge University, New York.

Lopez A. (2008), "Statistical Machine Translation.", ACM Computing Surveys, 40(3).

Peter F., Brown J., (1990), "A Statistical Approach to Machine Translation.", Computational Linguistics, 16(2), 79-85.

Supreme Council of Information and Communication Technology. (2013), Mizan English-Persian Parallel Corpus. Tehran, I.R. Iran. Retrieved from http://dadegan.ir/catalog/mizan.

\url{http://statmt.org/europarl/v7/bg-en.tgz}

Och F., Ney H. (2003), "A Systematic Comparison Of Various Statistical Alignment Models.", 2003 Association for Computational Linguistics, J03-1002, 29(1).

Wang X. "Evaluation of Two Word Alignment Systems.", Final Thesis, Department of Computer and Information Science.
}

\keyword{ package }

\examples{
# Since the extraction of  bg-en.tgz in Europarl corpus is time consuming, 
# so the aforementioned unzip files have been temporarily exported to 
# http://www.um.ac.ir/~sarmad/... .

\dontrun{

ww = word_alignIBM1 ('http://www.um.ac.ir/~sarmad/word.a/euro.bg',
                     'http://www.um.ac.ir/~sarmad/word.a/euro.en',
                      nrec=2000, ul_s = TRUE)

ss = Symmetrization ('http://www.um.ac.ir/~sarmad/word.a/euro.bg',
                     'http://www.um.ac.ir/~sarmad/word.a/euro.en',
                      nrec = 50, ul_s = TRUE)
}
}
