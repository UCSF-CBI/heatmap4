\name{heatmap4}
\alias{generateheatmap}
\title{Generate a Heat Map}
\description{This package takes the original heatmap function and reduces the arguement complexity. A heat map is a false color image (basically image(t(x))) with a dendrogram added to the left side and to the top. Typically, reordering of the rows and columns according to some set of values (row or column means) within the restrictions imposed by the dendrogram is carried out.}
\usage{generate_heatmap <- function(x, col_lab = c(FALSE, TRUE), row_lab = c(FALSE, TRUE), col_lab_vtr = NULL, row_lab_vtr =
NULL, col_anno = c(FALSE, TRUE), row_anno = c(FALSE, TRUE), col_info = NULL, row_info = NULL, col_anno_var = NULL, row_anno_var
= NULL, col_var_info = NULL, row_var_info = NULL, col_dend = c(FALSE, TRUE), row_dend = c(FALSE, TRUE), col_clust = c(FALSE,
TRUE), row_clust = c(FALSE, TRUE), plot_info = c("sideLabCol" = NULL, "sideLabRow" = NULL, "cexColSide" = NULL, "cexRowSide" =
NULL), file_name = NULL, ...)}
\arguments{
  \item{x}{numeric matrix; values to be plotted in the heatmap}
  \item{col_lab}{boolean (T, F); to display column labels; default is FALSE}
  \item{row_lab}{boolean (T, F); to display row labels; default is FALSE}
  \item{col_lab_vtr}{character vector; with column labels to use; defaults to colnames(x)}
  \item{row_lab_vtr}{character vector; with row labels to use; defaults to rownames(x)}
  \item{col_anno}{boolean (T, F); to display column annotations; defaults to FALSE}
  \item{row_anno}{boolean (T, F); to display row annotations; defaults to FALSE}
  \item{col_info}{data frame; data to be used in column annotations; default is NULL}
  \item{row_info}{data frame; data to be used in row annotations; default is NULL}
  \item{col_anno_var}{character vector; with specific column annotation variables to see in column annotations; defaults to colnames(col_info)}
  \item{row_anno_var}{character vector; with specific row annotation variables to see in row annotations; defaults to colnames(row_info)}
  \item{col_var_info}{nested list; of column annotation variables that need specific colors and placement; default is NULL}
  \item{row_var_info}{nested list; of row annotation variables that need specific colors and placement; default is NULL}
  \item{col_dend}{boolean (T, F); to display column dendrogram; default in FALSE}
  \item{row_dend}{boolean (T, F); to display row dendrogram; default in FALSE}
  \item{col_clust}{variable of hclust or dendrogram class; to cluster by the column; default NULL when col_dend is FALSE}
  \item{row_clust}{variable of hclust or dendrogram class; to cluster by the row; default NULL when row_dend is FALSE}
  \item{plot_info}{list; of 4 variables to control heatmap and annotation column/row label size (in that order); default is list("cexCol" = NULL, "cexRow" = NULL, "cexColSide" = NULL, "cexRowSide" = NULL); 'cex': heatmap labels and 'cexSide': annotation labels}
  \item{file_name}{string; denotes the name of the file, must contain one of the following file types: .pdf, .jpeg, .png, .tiff; default is NULL}
  \item{h_title}{string; gives the heatmap output a title; default is NULL}
  \item{input_legend}{boolean (T, F); to display annotations legend; row, column or both will be added if selected; default is FALSE}
  \item{legend_title}{string; denotes the title of the legend; default is NULL}
  \item{...}{additional arguments}
}
\details{
If either Rowv or Colv are dendrograms they are honored (and not reordered). Otherwise, dendrograms are computed as dd <- as.dendrogram(hclustfun(distfun(X))) where X is either x or t(x).

If either is a vector (of ‘weights’) then the appropriate dendrogram is reordered according to the supplied values subject to the constraints imposed by the dendrogram, by reorder(dd, Rowv), in the row case. If either is missing, as by default, then the ordering of the corresponding dendrogram is by the mean value of the rows/columns, i.e., in the case of rows, Rowv <- rowMeans(x, na.rm = na.rm). If either is NA, no reordering will be done for the corresponding side.

By default (scale = "row") the rows are scaled to have mean zero and standard deviation one. There is some empirical evidence from genomic plotting that this is useful.

The default colors are not used, instead the RColorBrewer package is used to enhance the colors. When adding personal colors to annotations, default colors or RColorBrewer colors are supported.

The "marray", micro array, package is used and installed through bioconductor. Full vignette and info found by entering: "browseVignettes("marray")" in RStudio.}
\value{
Invisibly, a list with components

rowInd
row index permutation vector as returned by order.dendrogram.

colInd
column index permutation vector.

Rowv
the row dendrogram; only if input Rowv was not NA and keep.dendro is true.

Colv
the column dendrogram; only if input Colv was not NA and keep.dendro is true.
}
\note{
Unless Rowv = NA (or Colw = NA), the original rows and columns are reordered in any case to match the dendrogram, e.g., the rows by order.dendrogram(Rowv) where Rowv is the (possibly reorder()ed) row dendrogram.

heatmap() uses layout and draws the image in the lower right corner of a 2x2 layout. Consequentially, it can not be used in a multi column/row layout, i.e., when par(mfrow = *) or (mfcol = *) has been called.
}
\references{
Original heatmap function and authors.
}
\author{
Rohan Bhanot and Ritu Roy.
}
%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
# Main numeric matrix to heatmap
x <- as.matrix(mtcars)

# Column annotation df
set.seed(827)

anno_rows <- ncol(mtcars)
anno_columns <- 3

random_data <- rnorm(anno_rows * anno_columns)
annotation_df <- matrix(data = random_data, nrow = anno_rows, ncol = anno_columns)
colnames(annotation_df) <- c('C1', 'C2', 'C3')
annotation_df <- as.data.frame(annotation_df)

# Basic heatmap4 example with column annotations only
generate_heatmap(x, col_info = annotation_df, col_anno = TRUE, row_lab = TRUE, col_lab = TRUE,
                 col_dend = TRUE, file_name = "Heatmap4 Example.pdf")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
