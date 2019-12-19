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
generate_heatmap <- function(x, col_lab = c(TRUE, FALSE), row_lab = c(TRUE, FALSE), col_lab_vtr = NULL,
                             row_lab_vtr = NULL, col_anno = c(TRUE, FALSE), row_anno = c(TRUE, FALSE), col_info = NULL,
                             row_info = NULL, col_anno_var = NULL, row_anno_var = NULL, col_var_info = NULL,
                             row_var_info = NULL, col_dend = c(TRUE, FALSE), row_dend = c(TRUE, FALSE),
                             col_clust = c(TRUE, FALSE), row_clust = c(TRUE, FALSE), file_name = NULL, ...)
  {

  # ## Row and Column Names
  # row_new_lab <- rownames(x)
  # col_new_lab <- colnames(x)

  #--------------------------------------------------------------------------------------------
  ## Annotations
  row_anno <- row_anno[1]

  if (row_anno) {

    # Checks to see if row_info is a valid input
    if (class(row_info) != 'data.frame'){
      stop("row_info is meant to be a data.frame")
    }
    # Defining row annotation variable selection
    row_var <- c()

    # Looping through row annotation data frame
    if (is.null(row_anno_var)) {
      for (o in 1:ncol(row_info)) {
        if (sum(!duplicated(row_info[ , o])) < nrow(row_info)) {
          row_var <- c(row_var, colnames(row_info)[o])
        }
      }
    } else {
      row_var <- row_anno_var
      row_check <- 0
      # Stop function to check to see if variables in list exist in the original anno matrix
      for (h in colnames(row_info)) {
        for (hh in row_var) {
          if (h == hh) {
            row_check = row_check + 1
          }
        }
      }
     }
      if (row_check != length(row_var)){
        stop("Selection of row annotation variables does not match column names in original data frame \n
             for row annotations.")
      }

      color_vec_default <- c("skyblue", "blue", "yellow", "purple", "black", "red", "orange", "green", "cyan", "darkgreen")

      row_color <- matrix(nrow = length(row_var), ncol = nrow(row_info))
      rownames(row_color) <- row_var

      for (v in 1:length(row_var)) {
        if (is.null(row_var_info)) {
          color_vec <- color_vec_default
        } else {
          if (row_var[v] %in% names(row_var_info)) {
            if ("color" %in% names(row_var_info[[row_var[v]]])){
              color_vec <-  row_var_info[[row_var[v]]]$color
            }
          }
        }

        if (length(unique(row_info[ ,v])) > 10) {
          #print("Needs to be scales")

        } else {
          dat <- row_info[ , row_var[v]]
          datUniq <- sort(unique(dat))
          for (v2 in 1:length(datUniq)) {
            j <- which(dat == datUniq[v2])
            row_color[v, j] <- color_vec[v2]
          }
        }
    }
    RowSideColors <- row_color
  } else {
    RowSideColors <- NULL
  }
  #------------------------------------------
  col_anno <- col_anno[1]

  if (col_anno) {

    # Checks to see if col_info is a valid input
    if (class(col_info) != 'data.frame') {
      stop("col_info is meant to be a data.frame")
    }

    # Defining column annotation variable selection
    col_var <- c()

    # Looping through column annotation data frame
    if (is.null(col_anno_var)) {
      for (r in 1:ncol(col_info)) {
        if (sum(!duplicated(col_info[ ,r])) < nrow(col_info)) {
          col_var <- c(col_var, colnames(col_info)[r])
        }
      }
    } else {
      col_var <- col_anno_var
      col_check <- 0
      # Stop function to check to see if variables in list exist in the original anno matrix
      for (n in colnames(col_info)) {
        for (nn in col_var) {
          if (n == nn) {
            col_check = col_check + 1
          }
        }
      }

      if (col_check != length(col_var)){
        stop("Selection of column annotation variables does not match column names in original data frame.")
      }

    }

    if (length(col_var) == 0) {
      stop("Column annotation matrix did not identify any unique annotations,\n
         column annotions will be turned off until issue is resolved")
      col_anno <- col_anno[2]
    }


    color_vec_default <- c("skyblue", "blue", "yellow", "purple", "black", "red", "orange", "green", "cyan", "darkgreen")

    col_color <- matrix(nrow = length(col_var), ncol = nrow(col_info))
    rownames(col_color) <- col_var

    for (v in 1:length(col_var)) {
      if (is.null(col_var_info)) {
        color_vec <- color_vec_default
      } else {
        if (col_var[v] %in% names(col_var_info)) {
          if ("color" %in% names(col_var_info[[col_var[v]]])){
            color_vec <-  col_var_info[[col_var[v]]]$color
          }
        }
      }

      if (length(unique(col_info[ ,v])) > 10) {
        #print("Needs to be scales")

      } else {
        dat <- col_info[ ,col_var[v]]
        datUniq <- sort(unique(dat))
        for (v2 in 1:length(datUniq)) {
          j <-  which(dat == datUniq[v2])
          col_color[v,j] <- color_vec[v2]

        }
      }
    }
    ColSideColors <- col_color
  } else {
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

test_list <-  list(STAGE = list(color = c("red", "blue", "green")))

row_test <- NULL

generate_heatmap(test_x, col_anno = FALSE, row_info = row_df, col_dend = FALSE)

# working example
# generate_heatmap(genomDat, row_info = phen, col_info = phen, row_anno = FALSE,
#                  col_anno = TRUE, row_lab = TRUE, col_lab = TRUE, row_dend = FALSE,
#                  file_name = "test2.pdf", col_anno_var = c("sex", "STAGE"),
#                  col_var_info = test_list)

# col_anno_var error
#generate_heatmap(genomDat, row_info = phen, col_info = phen, row_anno = FALSE,
#                 col_anno = TRUE, row_lab = TRUE, col_lab = TRUE, row_dend = FALSE,
#                 file_name = "test2.pdf", col_anno_var = c("sex", "stage"),
#                 col_var_info = test_list)

# file name error
# generate_heatmap(genomDat, row_info = phen, col_info = phen, row_anno = FALSE,
#                  col_anno = TRUE, row_lab = TRUE, col_lab = TRUE, row_dend = FALSE,
#                  file_name = "test2.pd", col_anno_var = c("sex", "STAGE"),
#                  col_var_info = test_list)
