if (F) {
## Calling Bioconductor
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("marray")
load("data.RData")
}

## Calls Libraries
library(RColorBrewer)
library(marray)

## Calls Files
#source("heatmap4.R")
#source("heatmapRelated.R")

## Wrapped heatmap function
generate_heatmap <- function(x, row_info = NULL, col_info = NULL, row_anno = c(TRUE, FALSE), col_anno = c(TRUE, FALSE),
                             row_lab = c(TRUE, FALSE), col_lab = c(TRUE, FALSE), row_lab_vtr = NULL, col_lab_vtr = NULL,
                             row_clust = c(TRUE, FALSE), col_clust = c(TRUE, FALSE), row_dend = c(TRUE, FALSE),
                             col_dend = c(TRUE, FALSE), col_var_info = NULL, row_var_info = NULL,
                             file_name = NULL, ...)
  {

  # ## Row and Column Names
  # row_new_lab <- rownames(x)
  # col_new_lab <- colnames(x)

  #--------------------------------------------------------------------------------------------
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

  # Checking row annotation variable list for valid data
  if (length(row_var) == 0) {
    stop("Row annotation matrix did not identify any unique annotations,\n
         row annotions will be turned off until issue is resolved")
    row_anno <- row_anno[2]
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
  #------------------------------------------
  col_anno <- col_anno[1]

  if (col_anno) {
    # Defining column annotation variable selection
    col_var <- c()
    # Looping through column annotation data frame
    if (is.null(col_var_info)) {
      for (r in 1:ncol(col_info)) {
        if (sum(!duplicated(col_info[,r])) < nrow(col_info)) {
          col_var <- c(col_var, colnames(col_info)[r])
        }
      }
    } else {
      col_var=names(col_var_info)
    }
    # Checking column annotation variable list for valid data
    if (length(col_var) == 0) {
      stop("Column annotation matrix did not identify any unique annotations,\n
         column annotions will be turned off until issue is resolved")
      col_anno <- col_anno[2]
    }

    # Colors we want to use
    color_vec <- c("skyblue", "blue", "yellow", "purple", "black", "red", "orange", "green", "cyan", "darkgreen")

    # Foundation for the color matrix
    col_color = matrix(nrow = length(col_var), ncol = nrow(col_info))
    rownames(col_color) = col_var

    # Filling the color matrix
    for (v in 1:length(col_var)) {

      if (length(unique(col_info[ ,v])) > 10) {
        # Needs to be scales

      } else {
        dat <- col_info[ ,col_var[v]]
        datUniq <- sort(unique(dat))

        for (v2 in 1:length(datUniq)) {
          j = which(dat == datUniq[v2])
          col_color[v, j] <- color_vec[v2]

        }
      }

    }
    ColSideColors <- col_color
  }
  else {
    ColSideColors <- NULL
  }
  #--------------------------------------------------------------------------------------------
  ## Labels
  row_lab <- row_lab[1]

  if (row_lab) {
    if(!(is.null(row_lab_vtr))) {
      labRow <- row_lab_vtr
    } else {
      labRow <- rownames(x)
    }
  }
  else {
    labRow <- NA
  }
  #------------------------------------------
  col_lab <- col_lab[1]

  if (col_lab) {
    if(!(is.null(col_lab_vtr))){
      labCol = col_lab_vtr
    } else {
      labCol <- colnames(x)
    }
  }
  else {
    labCol <- NA
  }
  #--------------------------------------------------------------------------------------------
  ## Clusters
  row_clust <- row_clust[1]

  if (row_clust) {
    cluster <- hclust(dist(as.matrix(x)), method = "complete", members = NULL)
  }
  else {
    cluster <-  NA
  }
  #------------------------------------------
  col_clust <- col_clust[1]

  if (col_clust) {
    cluster <- hclust(dist(as.matrix(x)), method = "complete", members = NULL)
  }
  else {
    cluster <- NA
  }
  #--------------------------------------------------------------------------------------------
  ## Dendograms
  row_dend <- row_dend[1]

  if (row_dend) {
    Rowv <- NULL
  }
  else {
    Rowv <- NA
  }
  #------------------------------------------
  col_dend <- col_dend[1]

  if (col_dend) {
    Colv <- NULL
  }
  else {
    Colv <- NA
  }
  #--------------------------------------------------------------------------------------------
  ## Output Files
  if (!is.null(file_name)){

    element <- strsplit(file_name, ".", fixed = TRUE)

    if (element[[1]][2] == "pdf") {
      pdf(file_name, paper = "letter")
    }
    else if (element[[1]][2] == "jpeg") {
    jpeg(filename = file_name)
    }
    else if (element[[1]][2] == "png") {
      png(filename = file_name)
    }
    else if (element[[1]][2] == "tiff") {
      tiff(filename = file_name)
    }
    else {
      stop("File name not in valid format: filename.type")
    }
  }
  #--------------------------------------------------------------------------------------------
  ## Heatmap Output
  heatmap4(x = x, Rowv = Rowv, Colv = Colv, distfun = dist, hclustfun = hclust,  symm = FALSE,
            ColSideColors = ColSideColors, RowSideColors = RowSideColors, labCol = labCol, labRow = labRow,
            scale = "none", na.rm = FALSE, margins = c(5, 5), main = NULL, xlab = NULL, ylab = NULL, zlm = limit,
            high = cols[1], low = cols[2], mid = cols[3])

  #--------------------------------------------------------------------------------------------
  ## Clears graphics on device
  dev.off()
}


generate_heatmap(genomDat, row_info = phen, col_info = phen, row_anno = FALSE, col_anno = TRUE, row_lab = TRUE,
                 col_lab = TRUE, row_dend = FALSE, file_name = "test2.pdf")
