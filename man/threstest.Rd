\name{threstest}
\alias{threstest}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
Generate files input for CIBERSORTx mixture file. This function can be used following the TPM function to generate input files with different filtering threshold of row sums in case a preliminary filtering is wanted to be done to filter out low count genes before running the CIBERSORTx deconvolution algorihtm.

This function take two inputs.
The first one is the dataframe you generated from the previous steps. (You many use TPM() or your own method.)
The second one is a vector, containing the filtering threshold you want to test.
(e.g. 10 means you want to drop rows for which row sum < 10)
}
\usage{
thresholds <- c(10,50,100,200,300,500)
for (threshold in thresholds) {
  threstest(TPM_count_test, threshold)
}
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x)
{
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
