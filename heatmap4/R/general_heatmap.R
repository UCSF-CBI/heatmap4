
## Calling Bioconductor
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("marray")
load("data.RData")

## Calls Libraries
library(RColorBrewer)
library(marray)

## Calls Files
source("heatmap4.R")
source("heatmapRelated.R")

## Wrapped heatmap function
generate_heatmap <- function(x, row_info = NULL, col_info = NULL, row_anno = c(TRUE, FALSE), col_anno = c(TRUE, FALSE),
                             row_lab = c(TRUE, FALSE), col_lab = c(TRUE, FALSE), row_clust = c(TRUE, FALSE),
                             col_clust = c(TRUE, FALSE), row_dend = c(TRUE, FALSE), col_dend = c(TRUE, FALSE),
                             col_var_info = list(NULL), row_var_info = list(NULL), test_scenario = c(TRUE, FALSE))
  {

  #NEED TO FIND PURPOSE, or delete
  ## Replacing Labels
  row_lab_vtr <- NULL
  col_lab_vtr <- NULL

  ## Row and Column Names
  row_new_lab <- rownames(x) ## Name of main matrix
  col_new_lab <- colnames(x) ## Name of main matrix

  if(!(is.null(row_lab_vtr) & is.null(col_lab_vtr))) {
    row_lab_vtr <- row_new_lab
    col_lab_vtr <- col_new_lab
  }


  ##Test Scenario
  test_scenario <- test_scenario[1]
  if (test_scenario) {
  ## Reducing Matrices
  i <- 1:20
  j <- 1:10

  ## Simplifying Data Matrices
  x <- x[i, j]
  row_info <- row_info[i, ]
  col_info <- col_info[, j]
  }

  ## Annotations
  row_anno <- row_anno[2]

  if (row_anno) {
    row_var <- colnames(row_info)

    color_vec <- c("skyblue", "blue", "yellow", "purple", "black", "red", "orange", "green", "cyan", "darkgreen")
    if (is.null(row_var)) {
      row_color <- color_vec
    } else {
      for (anno in row_var) {
        a <- 1
        row_color <- c(color_vec[a])
        a <- a + 1
      }
    }
    RowSideColors <- row_color
  }
  else {
    RowSideColors <- NA
  }

  col_anno <- col_anno[2]

  if (col_anno) {
    col_var <- as.vector(col_info)

    color_vec <- c("skyblue", "blue", "yellow", "purple", "black", "red", "orange", "green", "cyan", "darkgreen")
    if (is.null(col_var)) {
      col_color <- color_vec
    } else {
      for (anno in col_var) {
        a <- 1
        col_color <- c(color_vec[a])
        a <- a + 1
      }
    }

    ColSideColors <- col_color
  }
  else {
    ColSideColors <- NA
  }

  ## Labels
  row_lab <- row_lab[1]

  if (row_lab) {
    labRow <- row_lab_vtr
  }
  else {
    labRow <- NA
  }

  col_lab <- col_lab[1]

  if (col_lab) {
    labCol <- col_lab_vtr
  }
  else {
    labCol <- NA
  }

  ## Clusters
  row_clust <- row_clust[1]

  if (row_clust) {
    cluster <- hclust(dist(as.matrix(x)), method = "complete", members = NULL)
  }
  else {
    cluster <-  NA
  }

  col_clust <- col_clust[1]

  if (col_clust) {
    cluster <- hclust(dist(as.matrix(x)), method = "complete", members = NULL)
  }
  else {
    cluster <- NA
  }

  ## Dendograms
  row_dend <- row_dend[1]

  if (row_dend) {
    Rowv <- NULL
  }
  else {
    Rowv <- NA
  }

  col_dend <- col_dend[1]

  if (col_dend) {
    Colv <- NULL
  }
  else {
    Colv <- NA
  }

  heatmap4(x = x, Rowv = Rowv, Colv = Colv, distfun = dist, hclustfun = cluster,  symm = FALSE,
            ColSideColors = ColSideColors, RowSideColors = RowSideColors, labCol = labCol, labRow = labRow,
            scale = "none", na.rm = FALSE, margins = c(5, 5), main = NULL, xlab = NULL, ylab = NULL, zlm = limit,
            high = cols[1], low = cols[2], mid = cols[3])
  }

generate_heatmap(genomDat, row_info = chrInfo, col_info = phen, row_anno = FALSE, col_anno = TRUE, row_lab = TRUE,
                 col_lab = TRUE, row_dend = FALSE, test_scenario = TRUE)
