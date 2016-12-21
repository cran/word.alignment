\name{culf}
\alias{culf}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Make a String's First n Characters Lowercase
}
\description{
Converts uppercase to lowercase letters for the first n characters  of a character string.
}
\usage{
culf(x, n = 1, first = TRUE, second = FALSE, lower = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
a character string.
}
  \item{n}{
an integer. Number of characters that we want to convert.
}
  \item{first}{
logical. If \code{TRUE}, it converts the n first characters into lowercase.
}
  \item{second}{
logical. If \code{TRUE}, it checks if the second letter of \code{x} is uppercase, the whole word will be converted to lower.
}
  \item{lower}{
logical. If \code{TRUE}, it works similar to \code{tolower} in base R.
}
}
\details{
It is a function to convert some uppercase letters  into lowercase for which words with uppercase second letter. If \code{\link{tolower}} in base R is used, it will be sometimes created a problem for proper nouns. Because, as we know, a name or proper noun starts with capital letter and we do not want to convert them into lowercase. But sometimes there are some words which are not a name or proper noun and displayed in capital letters. These words are the target of this function.

If we have a text of several sentences and we want to convert  the first n letters of every sentence to lowercase, separately. We have to split text to sentences, furthermore we should consider \code{first=TRUE} and apply the function for each sentence (see the examples below).

If we have a list, it works fine.
}
\value{
A character string.
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\author{
Neda Daneshgar and Majid Sarmad.
}
\note{
Because of all sentences begin with uppercase letters, \code{first=TRUE} is considered  as a default. But, if the second character of a word be capital, it is usually
concluded that all its characters are capital. In this case, you can consider \code{second=TRUE}. Of course, there are some exceptations in these cases that they can be ignored (see the examples below).

In general, if there are not a lot of proper nouns in your text string, we suggest you to use \code{\link{tolower}} in base R. As an ability of this function, \code{lower} is considered as a third argument.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{tolower}}
}
\examples{
# x is a list

x=list('W-A for an English-Persian Parallel Corpus (Mizan).','ALIGNMENT is a link between words.')

culf(x, n=8) ## culf(x, n=8) is not a list

y='MT is the automatic translation. SMT is one of the methods of MT.'

culf(y) # only run for the first sentence

u1=unlist(strsplit(y, ". ", fixed = TRUE))
sapply(1:length(u1),function(x)culf(u1[x])) ## run for all sentences

h = 'It is a METHOD for this function.'
culf (h, second = TRUE) #only run for the first word

h1 = strsplit(h, ' ')[[1]]
culf(h1, second = TRUE) # run for all words
}
