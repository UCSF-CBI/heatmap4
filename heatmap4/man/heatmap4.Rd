\name{heatmap4}
\alias{generate_heatmap4}
\title{Simpler Heatmap function for datasets}
\description{This package takes the original heatmap function and reduces the number of arguements while modifing arguement defaults. This makes it quicker to analyze data.}
\usage{heatmap4(x, Rowv = NULL, Colv = NULL, distfun = dist, hclustfun = hclust, symm = FALSE, ColSideColors, RowSideColors, labCol, labRow, scale = c("row", "column", "none"), na.rm = TRUE, margins = c(5, 5)), main = NULL, xlab = NULL, ylab = NULL, zlm, high, low, mid, addamps, colamps, addText}
\arguments{
  \item{x}{numeric matrix of values to be plotted}
  \item{Rowv}{determines if and how the row dendrogram should be computed and reordered. Either a dendrogram or a vector of values used to reorder the row dendrogram or NA to suppress any row dendrogram (and reordering) or by default, NULL, see ‘Details’ below.}
  \item{Colv}{determines if and how the column dendrogram should be reordered. Has the same options as the Rowv argument above and additionally when x is a square matrix, Colv = "Rowv" means that columns should be treated identically to the rows (and so if there is to be no row dendrogram there will not be a column one either).}
  \item{distfun}{function used to compute the distance (dissimilarity) between both rows and columns. Defaults to dist}
  \item{hclustfun}{function used to compute the hierarchical clustering when Rowv or Colv are not dendrograms. Defaults to hclust. Should take as argument a result of distfun and return an object to which as.dendrogram can be applied.}
  \item{symm}{logical indicating if x should be treated symmetrically; can only be true when x is a square matrix.}
  \item{ColSideColors}{(optional) character vector of length ncol(x) containing the color names for a horizontal side bar that may be used to annotate the columns of x.}
  \item{RowSideColors}{(optional) character vector of length nrow(x) containing the color names for a vertical side bar that may be used to annotate the rows of x.}
  \item{labCol, labRow}{character vectors with row and column labels to use; these default to rownames(x) or colnames(x), respectively.}
  \item{scale}{character indicating if the values should be centered and scaled in either the row direction or the column direction, or none. The default is "row" if symm false, and "none" otherwise.}
  \item{na.rm}{logical indicating whether NA's should be removed.}
  \item{margins}{numeric vector of length 2 containing the margins (see par(mar = *)) for column and row names, respectively.}
  \item{main, xlab, ylab}{main, x- and y-axis titles; defaults to none.}
  \item{zlm}{all the values greater or equal than zlm are set to zlm - 0.01. a;; value less or equal to -zlm are set to -zlm + 0.01.}
  \item{high, low, mid}{Colors to use for the Lowest, middle, and highest values. mid may be omitted.}
  \item{addamps}{}
  \item{colamps}{}
  \item{addText}{}
\details{
If either Rowv or Colv are dendrograms they are honored (and not reordered). Otherwise, dendrograms are computed as dd <- as.dendrogram(hclustfun(distfun(X))) where X is either x or t(x).

If either is a vector (of ‘weights’) then the appropriate dendrogram is reordered according to the supplied values subject to the constraints imposed by the dendrogram, by reorder(dd, Rowv), in the row case. If either is missing, as by default, then the ordering of the corresponding dendrogram is by the mean value of the rows/columns, i.e., in the case of rows, Rowv <- rowMeans(x, na.rm = na.rm). If either is NA, no reordering will be done for the corresponding side.

By default (scale = "row") the rows are scaled to have mean zero and standard deviation one. There is some empirical evidence from genomic plotting that this is useful.

The default colors are not pretty therefore the RColorBrewer package is used to enhance the colors.

The "marray", micro array, package is used and installed through bioconductor. Full vignette and info found by entering: "browseVignettes("marray")" in RStudio.}
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
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
