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
                             row_lab_vtr = NULL,
                             # Annotation varaiables
                             col_anno = c(TRUE, FALSE), row_anno = c(TRUE, FALSE), col_info = NULL,
                             row_info = NULL, col_anno_var = NULL, row_anno_var = NULL, col_var_info = NULL,
                             row_var_info = NULL,
                             # ---
                             col_dend = c(TRUE, FALSE), row_dend = c(TRUE, FALSE),
                             # ---
                             col_clust = c(TRUE, FALSE), row_clust = c(TRUE, FALSE),
                             plot_info = c("sideLabCol" = NULL, "sideLabRow" = NULL, "cexCol" = NULL, "cexRow" = NULL),
                             file_name = NULL, ...)
{

  # ## Row and Column Names
  # row_new_lab <- rownames(x)
  # col_new_lab <- colnames(x)

  #--------------------------------------------------------------------------------------------
  ## Annotations
  row_anno <- row_anno[1]

  if (row_anno) {

    # Checks to see if row_info is a valid input
    if (class(row_info) != 'data.frame') {
      stop("row_info is meant to be a data.frame")
    }

    # Defining row annotation variable selection
    row_var <- c()

    # Looping through row annotation data frame
    if (is.null(row_anno_var)) {
      for (r in 1:ncol(row_info)) {
        if (sum(!duplicated(row_info[ ,r])) < nrow(row_info)) {
          row_var <- c(row_var, colnames(row_info)[r])
        }
      }
    } else {
      k <- match(colnames(row_info), row_anno_var)
      k <- k[!is.na(k)]
      if (length(k) == 0) {
        row_var <- colnames(row_info)
        cat("Selection of row annotation variables does not match column names in original data frame.\n")
      } else {
        row_var <- row_anno_var[k]
        if (length(k) != length(row_anno_var)) cat("Selection of column annotation variables does not match column names in original data frame.\n")
      }


    }

    if (length(row_var) == 0) {
      stop("Column annotation matrix did not identify any unique annotations,\n
         column annotations will be turned off until issue is resolved")
      row_anno <- row_anno[2]
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

    if (is.numeric(row_info[ ,row_var[v]]) & length(unique(row_info[ ,row_var[v]])) > 5) {

        varib <- row_info[ ,row_var[v]]
        varib <- varib - min(varib, na.rm=T) + 1
        varib <- varib / min(varib, na.rm=T)

        varib <- round(varib)
        lim <- range(varib,na.rm=T)

        grpUniq <- lim[1]:lim[2]
        rowColUniq <- maPalette(high=colList2[2], low=colList2[1], k=length(grpUniq))


        row_color[v, ] <- rowColUniq[varib]

      } else {
        dat <- row_info[ ,row_var[v]]
        datUniq <- sort(unique(dat))
        for (v2 in 1:length(datUniq)) {
          j <-  which(dat == datUniq[v2])
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
      k=match(colnames(col_info),col_anno_var)
      k=k[!is.na(k)]
      if (length(k)==0) {
        col_var=colnames(col_info)
        cat("Selection of column annotation variables does not match column names in original data frame.\n")
      } else {
        col_var=col_anno_var[k]
        if (length(k)!=length(col_anno_var)) cat("Selection of column annotation variables does not match column names in original data frame.\n")
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
        if (is.numeric(col_info[ ,col_var[v]]) & length(unique(col_info[ ,col_var[v]])) > 5) {
          color_vec <- c("white","black")
        } else {
          color_vec <- color_vec_default
        }
      } else {
        if (col_var[v] %in% names(col_var_info)) {
          if ("color" %in% names(col_var_info[[col_var[v]]])) {
            color_vec <-  col_var_info[[col_var[v]]]$color
          }
        }
      }

      if (is.numeric(col_info[ ,col_var[v]]) & length(unique(col_info[ ,col_var[v]])) > 5) {

        #varib=c(0,sample(0:100,size=50),0)
        #x=varib+1

        varib <- col_info[ ,col_var[v]]
        varib = varib - min(varib, na.rm=T) + 1
        varib = varib / min(varib, na.rm=T)

        varib = round(varib)
        lim = range(varib,na.rm=T)

        grpUniq=lim[1]:lim[2]
        colColUniq=maPalette(high=colList2[2],low=colList2[1],k=length(grpUniq))


        col_color[v, ] <- colColUniq[varib]

      } else {
        dat <- col_info[ ,col_var[v]]
        datUniq <- sort(unique(dat))
        if (length(datUniq) > length(color_vec)) {
          color_vec <- rainbow(length(datUniq))
          cat("Not enough colors for ,",col_var[v],"; They have been assigned random colors in the meantime.\n")
        }
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
  # Global Plot Info
  nc <- ncol(x)
  nr <- nrow(x)

  cexColSide <-  1
  cexRowSide <-  1
  cexCol <- 0.2 + 1 / log10(nc)
  cexRow <- 0.2 + 1 / log10(nr)

  if (!(is.null(plot_info))) {

    if ("cexRowSide" %in% names(plot_info)) {
      cexRowSide <- plot_info$cexRowSide
    }
    if ("cexColSide" %in% names(plot_info)) {
      cexColSide <- plot_info$cexColSide
    }
    if ("cexCol" %in% names(plot_info)) {
      cexCol <- plot_info$cexCol
    }
    if ("cexRow" %in% names(plot_info)){
      cexRow <- plot_info$cexRow
    }
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
  ## Dendograms
  row_dend <- row_dend[1]

  if (row_dend) {
    Rowv <- row_clust
  }
  else {
    Rowv <- NA
  }
  #------------------------------------------
  col_dend <- col_dend[1]

  if (col_dend) {
    Colv <- col_clust
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
  heatmap4(x = x, Rowv = Rowv, Colv = Colv, distfun = dist,  symm = FALSE,
           ColSideColors = ColSideColors, RowSideColors = RowSideColors, labCol = labCol, labRow = labRow,
           scale = "none", na.rm = FALSE, margins = c(5, 5), main = "Plot Info Test - Anno Large", xlab = NULL, ylab = NULL, zlm = limit,
           high = cols[1], low = cols[2], mid = cols[3], cexRowSide = cexRowSide, cexColSide = cexColSide, cexRow = cexRow,
           cexCol = cexCol, ...)

  #--------------------------------------------------------------------------------------------
  ## Clears graphics on device
  dev.off()
}

test_list <-  list(STAGE = list(color = c("red", "blue", "green"), type = "categorical"),
                   age = list(color = c("yellow", "green")))

row_test <- NULL

phen$age = phen$age..yrs. + 0.5


set.seed(20430)
rbz_rows <-  20
rbz_columns <- 3
dat <- rnorm(rbz_rows * rbz_columns)
rbz <- matrix(data = dat, nrow = rbz_rows, ncol = rbz_columns)
colnames(rbz) <- c('R1', 'R2', 'R3')
rbz <- as.data.frame(rbz)

global_test = list("cexRowSide" = 3, "cexColSide" = 3)
#

# working example - all dend and row
generate_heatmap(genomDat, row_info = rbz, col_info = phen, row_anno = TRUE,
                 col_anno = TRUE, row_lab = TRUE, col_lab = TRUE, row_dend = TRUE, col_dend = TRUE,
                 file_name = "plot_info Testing 2.pdf", col_anno_var = c("sex", "STAGE", "age"),
                 row_anno_var = c("R1"), col_var_info = test_list, col_clust = TRUE, plot_info = global_test)

# working example - only row dend and annos
# generate_heatmap(genomDat, row_info = rbz, col_info = phen, row_anno = TRUE,
#                  col_anno = FALSE, row_lab = TRUE, col_lab = TRUE, row_dend = TRUE, col_dend = FALSE,
#                  file_name = "test2.pdf", col_anno_var = c("sex", "STAGE", "age"),
#                  row_anno_var = c("R1"), col_var_info = test_list, col_clust = TRUE)

# col_anno_var error
# generate_heatmap(genomDat, row_info = phen, col_info = phen, row_anno = FALSE,
#                  col_anno = TRUE, row_lab = TRUE, col_lab = TRUE, row_dend = FALSE,
#                  file_name = "test2.pdf", col_anno_var = c("sex", "stage"),
#                  col_var_info = test_list)

# file name error
# generate_heatmap(genomDat, row_info = phen, col_info = phen, row_anno = FALSE,
#                  col_anno = TRUE, row_lab = TRUE, col_lab = TRUE, row_dend = FALSE,
#                  file_name = "test2.pd", col_anno_var = c("sex", "STAGE"),
#                  col_var_info = test_list)
#
# Decimal Anno Range Testing
# generate_heatmap(genomDat, row_info = rbz, col_info = phen, row_anno = TRUE,
#                  col_anno = TRUE, row_lab = TRUE, col_lab = TRUE, row_dend = TRUE, col_dend = TRUE,
#                  file_name = "Decimal Anno Range Testing.pdf", col_anno_var = c("sex", "STAGE", "age"),
#                  row_anno_var = c("R1"), col_var_info = test_list, col_clust = TRUE)

