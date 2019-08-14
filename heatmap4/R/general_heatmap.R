
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
                             col_var_info = list(NULL), row_var_info = list(NULL), file_name = NULL,
                             test_scenario = c(TRUE, FALSE), ...)
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
  i <- 1:10
  j <- 1:5

  ## Simplifying Data Matrices
  x <- x[i, j]
  row_info <- row_info[i, ]
  col_info <- col_info[, j]
  }

  ## Annotations
  row_anno <- row_anno[1]

  if (row_anno) {
    r <- 1

    while (r <= length(row_info)) {

      if (nrow(unique(row_info[r])) == nrow(row_info)) {
        #print(colnames(row_info[r]))
        r = r + 1
      }
      else {
        #print(colnames(row_info[r]))
        row_var <- append(row_var, c(colnames(row_info[r])))
        r = r + 1
      }
    }

    w <- 1
    color_vec <- c("skyblue", "blue", "yellow", "purple", "black", "red", "orange", "green", "cyan", "darkgreen")

    for (v in row_var) {

      if (length(unique(row_info[ ,v])) > 3) {
        #print("Needs to be scales")
      }
      else if (length(unique(row_info[ ,v])) == 2){
        if (w > 10) {
          w <- 1
        }
        row_color <- append(row_color, color_vec[w:(w + 1)])
        w = w + 2
      }
      else if (length(unique(row_info[ ,v])) == 1) {
        if (w > 10) {
          w <- 1
        }
        row_color <- append(row_color, color_vec[w])
        w = w + 1
      }

    }
    RowSideColors <- row_color
  }
  else {
    RowSideColors <- NULL
  }

  col_anno <- col_anno[1]

  if (col_anno) {
    r <- 1

    while (r <= length(col_info)) {

      if (nrow(unique(col_info[r])) == nrow(col_info)) {
        #print(colnames(col_info[r]))
        r = r + 1
      }
      else {
        #print(colnames(col_info[r]))
        col_var <- append(col_var, c(colnames(col_info[r])))
        r = r + 1
      }

    }
    w <- 1
    color_vec <- c("skyblue", "blue", "yellow", "purple", "black", "red", "orange", "green", "cyan", "darkgreen")

    for (v in col_var) {

      if (length(unique(col_info[ ,v])) > 3) {
        #print("Needs to be scales")

      }
      else if (length(unique(col_info[ ,v])) == 2){
        if (w > 10) {
          w <- 1
        }
        col_color <- append(col_color, color_vec[w:(w + 1)])
        w = w + 2
      }
      else if (length(unique(col_info[ ,v])) == 1) {
        if (w > 10) {
          w <- 1
        }
        col_color <- append(col_color, color_vec[w])
        w = w + 1
      }

    }
    ColSideColors <- col_color
  }
  else {
    ColSideColors <- NULL
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

  ## Output Files
  element = strsplit(file_name, ".", fixed = TRUE)

  if (element[[1]][2] == "pdf") {
    pdf(file_name, paper = "letter")
    cat(file_name)
    print("pdf")
  }
  else if (element[[1]][2] == "jpeg") {
    jpeg(filename = file_name)
    cat(file_name)
    print("jpeg")
  }
  else if (element[[1]][2] == "png") {
    png(filename = file_name)
    cat(file_name)
    print("png")
  }
  else if (element[[1]][2] == "tiff") {
    tiff(filename = file_name)
    cat(file_name)
    print("tiff")
  }
  else {
    stop("File name not in valid format: filename.type")
    print("why you no work")
  }

  ## Heatmap Output
  heatmap4(x = x, Rowv = Rowv, Colv = Colv, distfun = dist, hclustfun = hclust,  symm = FALSE,
            ColSideColors = ColSideColors, RowSideColors = RowSideColors, labCol = labCol, labRow = labRow,
            scale = "none", na.rm = FALSE, margins = c(5, 5), main = NULL, xlab = NULL, ylab = NULL, zlm = limit,
            high = cols[1], low = cols[2], mid = cols[3])

  dev.off()
}


generate_heatmap(genomDat, row_info = chrInfo, col_info = phen, row_anno = FALSE, col_anno = TRUE, row_lab = TRUE,
                 col_lab = TRUE, row_dend = FALSE, file_name = "heatmaptest.jpeg", test_scenario = FALSE)

dev.off()

