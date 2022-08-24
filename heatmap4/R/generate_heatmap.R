## 7. Ritu 04/13/22 - Fix continuous row color bar
## 8. Ritu 05/13/22 - row_var not col_var for row color bar
## 9. Ritu 06/09/22 - Set no. of axis labels to 5 (k=5) in heatmapColorBar

## Wrapped heatmap function
generate_heatmap <- function(x, col_lab = c(FALSE,TRUE), row_lab = c(FALSE,TRUE), col_lab_vtr = NULL,
                             row_lab_vtr = NULL, col_anno = c(FALSE, TRUE), row_anno = c(FALSE,TRUE), col_info = NULL,
                             row_info = NULL, col_anno_var = NULL, row_anno_var = NULL, col_var_info = NULL,
                             row_var_info = NULL, col_dend = c(FALSE,TRUE), row_dend = c(FALSE,TRUE),
                             col_anno_name=NULL, row_anno_name=NULL,
                             # review col/row_clust and _dend
                             col_clust = NULL, row_clust = NULL,
                             plot_info = list(margins=c(5,5),cexCol=NULL,cexRow=NULL,cexColSide=NULL,cexRowSide=NULL,colorCatCol=NULL,colorCatRow=NULL,colorContCol=NULL,colorContRow=NULL),
                             file_name = NULL, h_title = NULL,input_legend = c(FALSE, TRUE), legend_title = NULL, heatmap_color=c("red", "blue", "grey"), zlm=c(-0.5, 0.5), ...)
{

  #--------------------------------------------------------------------------------------------
  ## Annotations
  input_legend <- input_legend[1]
  row_anno <- row_anno[1]
  col_anno <- col_anno[1]

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
        if (is.numeric(row_info[ ,r]) | sum(!duplicated(row_info[ ,r])) < nrow(row_info)) {
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

    color_vec_cat_default <- c("skyblue", "blue", "yellow", "purple", "black", "red", "orange", "green", "cyan", "darkgreen")
    color_vec_cont_default <- c("white","black")
    if ("colorCatRow"%in%names(plot_info) && !is.null(plot_info$colorCatRow)) color_vec_cat_default <- plot_info$colorCatRow
    if ("colorContRow"%in%names(plot_info) && !is.null(plot_info$colorContRow)) color_vec_cont_default <- plot_info$colorContRow

    row_color <- matrix(nrow = length(row_var), ncol = nrow(row_info))
    if (is.null(row_anno_name)) {
        #rownames(row_color) <- paste(rownames(row_var)," ",sep="")
        rownames(row_color) <- paste(row_var," ",sep="")
    } else {
     k <- match(colnames(row_info), row_anno_var)
     k <- k[!is.na(k)]
     rownames(row_color) <- row_anno_name[k]
    }
    
    for (v in 1:length(row_var)) {
      if (is.numeric(row_info[ ,row_var[v]]) & length(unique(row_info[ ,row_var[v]])) > 5){
        color_vec <- color_vec_cont_default
      } else {
          color_vec <- color_vec_cat_default
      }
      if (!is.null(row_var_info)) {
        if (row_var[v] %in% names(row_var_info)) {
          if ("color" %in% names(row_var_info[[row_var[v]]])){
            color_vec <-  row_var_info[[row_var[v]]]$color
          }
        }
      }

    if (is.numeric(row_info[ ,row_var[v]]) & length(unique(row_info[ ,row_var[v]])) > 5) {

        varib <- row_info[ ,row_var[v]]
        if ("limit" %in% names(row_var_info[[row_var[v]]])){
            lim <-  row_var_info[[row_var[v]]]$limit
            varib=round(varib); varib[varib<lim[1]]=lim[1]; varib[varib>lim[2]]=lim[2]; varib=varib+lim[2]+1
            ## 7. Ritu
            lim=lim+lim[2]+1
        } else {
            if (F) {
                varib = varib - min(varib, na.rm=T) + 1
                varib = varib / min(varib, na.rm=T)
            }
            if (T) {
                x1=max(abs(varib-min(varib,na.rm=T)),na.rm=T)
                varib=(100*(varib-min(varib,na.rm=T))/x1)+1
            }

            varib <- round(varib)
            lim <- range(varib,na.rm=T)
        }

        grpUniq <- lim[1]:lim[2]
        rowColUniq <- maPalette(high=color_vec[2], low=color_vec[1], k=length(grpUniq))

        ## 7. Ritu
        #row_color[v, ] <- rowColUniq[varib]
        j=match(varib,grpUniq); j1=which(!is.na(j)); j2=j[j1]
        row_color[v,j1] <- rowColUniq[j2]

      } else {
        dat <- row_info[ ,row_var[v]]
        datUniq <- sort(unique(dat))
        if (length(datUniq) > length(color_vec)) {
          color_vec <- rainbow(length(datUniq))
          ## 8. Ritu
          #cat("Not enough colors for ,",col_var[v],"; They have been assigned random colors in the meantime.\n")
          cat("Not enough colors for ,",row_var[v],"; They have been assigned random colors in the meantime.\n")
        }
        for (v2 in 1:length(datUniq)) {
          j <-  which(dat == datUniq[v2])
          row_color[v, j] <- color_vec[v2]

        }
      }
    }
    RowSideColors <- row_color
      if (!is.null(row_anno_name)) {
          k=match(row_anno_name,rownames(RowSideColors))
          k=k[!is.na(k)]
          nm=rownames(RowSideColors)
          RowSideColors=RowSideColors[k,]
          if (!is.matrix(RowSideColors)) {
              RowSideColors=matrix(RowSideColors,nrow=1)
              rownames(RowSideColors)=nm
          }
      }
  } else {
    RowSideColors <- NULL
  }
  #------------------------------------------

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
        if (is.numeric(col_info[ ,r]) | sum(!duplicated(col_info[ ,r])) < nrow(col_info)) {
          col_var <- c(col_var, colnames(col_info)[r])
        }
      }
    } else {
        #k=match(colnames(col_info),col_anno_var)
        #k=k[!is.na(k)]
      k=which(col_anno_var%in%colnames(col_info))
      if (length(k)==0) {
        col_var=colnames(col_info)
        cat("Selection of column annotation variables does not match column names in original data frame.\n")
      } else {
        col_var=col_anno_var[k]
        if (length(k)!=length(col_anno_var)) cat("Selection of column annotation variables does not match column names in original data frame.\n")
      }

    }

    color_vec_cat_default <- c("skyblue", "blue", "yellow", "purple", "black", "red", "orange", "green", "cyan", "darkgreen")
    color_vec_cont_default <- c("white","black")
    if ("colorCatCol"%in%names(plot_info) && !is.null(plot_info$colorCatCol)) color_vec_cat_default <- plot_info$colorCatCol
    if ("colorContCol"%in%names(plot_info) && !is.null(plot_info$colorContCol)) color_vec_cont_default <- plot_info$colorContCol

    col_color <- matrix(nrow = length(col_var), ncol = nrow(col_info))
    if (is.null(col_anno_name)) {
        #rownames(col_color) <- paste(rownames(col_color)," ",sep="")
        rownames(col_color) <- paste(col_var," ",sep="")
    } else {
        #k=match(colnames(col_info),col_anno_var)
        #if (any(is.na(k))) cat("Mismatched column variables!!!")
        #k=k[!is.na(k)]
        if (length(col_anno_var)!=length(col_anno_name)) cat("Mismatched column variables!!!")
        k=which(col_anno_var%in%colnames(col_info))
        rownames(col_color) <- col_anno_name[k]
    }
    
    for (v in 1:length(col_var)) {
      if (is.numeric(col_info[ ,col_var[v]]) & length(unique(col_info[ ,col_var[v]])) > 5) {
        color_vec <- color_vec_cont_default
      } else {
          color_vec <- color_vec_cat_default
      }
      if (!is.null(col_var_info)) {
        if (col_var[v] %in% names(col_var_info)) {
          if ("color" %in% names(col_var_info[[col_var[v]]])) {
            color_vec <-  col_var_info[[col_var[v]]]$color
          }
        }
      }

      if (is.numeric(col_info[ ,col_var[v]]) & length(unique(col_info[ ,col_var[v]])) > 5) {

        varib <- col_info[ ,col_var[v]]
        if ("limit" %in% names(col_var_info[[col_var[v]]])){
            if (F) {
                x1=max(abs(varib-min(varib,na.rm=T)),na.rm=T)
                varib=((varib-min(varib,na.rm=T))/x1)+1
                lim <-  col_var_info[[col_var[v]]]$limit+1
            } else {
                lim <-  col_var_info[[col_var[v]]]$limit
                varib=round(varib); varib[varib<lim[1]]=lim[1]; varib[varib>lim[2]]=lim[2]; varib=varib+lim[2]+1
                lim=lim+lim[2]+1
            }
        } else {
            if (F) {
                varib = varib - min(varib, na.rm=T) + 1
                varib = varib / min(varib, na.rm=T)
            }
            if (T) {
                x1=max(abs(varib-min(varib,na.rm=T)),na.rm=T)
                varib=(100*(varib-min(varib,na.rm=T))/x1)+1
            }
            varib <- round(varib)
            lim <- range(varib,na.rm=T)
        }

        grpUniq=lim[1]:lim[2]
        colColUniq=maPalette(high=color_vec[2],low=color_vec[1],k=length(grpUniq))

        j=match(varib,grpUniq);j1=which(!is.na(j)); j2=j[j1]
        col_color[v,j1] <- colColUniq[j2]
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
    if (!is.null(col_anno_name)) {
        k=match(col_anno_name,rownames(ColSideColors))
        k=k[!is.na(k)]
        nm=rownames(ColSideColors)
        ColSideColors=ColSideColors[k,]
        if (!is.matrix(ColSideColors)) {
            ColSideColors=matrix(ColSideColors,nrow=1)
            rownames(ColSideColors)=nm
        }
    }
  } else {
    ColSideColors <- NULL
  }
  #--------------------------------------------------------------------------------------------
  # Global Plot Info
  nc <- ncol(x)
  nr <- nrow(x)

  margins=c(5,5)
  cexColSide <-  1
  cexRowSide <-  1
  cexCol <- 0.2 + 1 / log10(nc)
  cexRow <- 0.2 + 1 / log10(nr)

  if (!(is.null(plot_info))) {
    if ("margins" %in% names(plot_info)){
      margins <- plot_info$margins
    }
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
    if (is.null(row_clust) || class(row_clust) %in% c("dendogram", "hclust")) {
      Rowv <- row_clust
    }
    else {
      stop("Error in column clustering class.")
    }
  }
  else {
    Rowv <- NA
  }
  #------------------------------------------
  col_dend <- col_dend[1]

  if (col_dend) {
    if (is.null(col_clust) || class(col_clust)%in%c("dendogram", "hclust")) {
      Colv <- col_clust
    }
    else {
      stop("Error in column clustering class.")
    }
  }
  else {
    Colv <- NA
  }
  #--------------------------------------------------------------------------------------------
  ## Legend
  heatmapColorBar.my <- function(zlm=c(-.5,.5),cols=c("green","red","black"),main=NULL) {
      if (length(cols)==3) {
          try <- maPalette(high=cols[1], low=cols[2], mid=cols[3])
      } else {
          ## 5. Ritu
          try <- maPalette(high=cols[1], low=cols[2])
      }
      maColorBar(try, scale=zlm,main=main)
  }
  sampleColorLegend.my <- function(tls,col=NULL,lty=NULL,legendTitle=NULL,cex=NULL,density=NULL) {
    nTypes <- length(tls)
    if (is.null(col)) {
      cl <- brewer.pal(8, "Accent")
      cl <- cl[1:min(nTypes,length(cl))]
      if (length(cl)<nTypes) {
        cl=c(brewer.pal(nTypes,"Set3"),brewer.pal(nTypes,"Set2"))[1:nTypes]
        cl[1] <- "#1F78B4"
        if (length(cl)>8) {
          cl[9] <- "#999999"
        }
      }
      cl <- cl[1:nTypes]
      cl[cl=="#FFFF99"]="#FFFF60"
    } else {
      cl <- col
    }
    fill=col=NULL
    if (is.null(lty)) {
      fill=cl
    } else {
      col=cl
    }
    n <- length(tls)
    if (n!=0) {
        ii <- 1:length(tls)
        if (is.null(cex)) {
          cex=ifelse(max(nchar(tls))>13,1.5,3)
          if (nTypes>6) {cex=1.5}
        }
        plot(0:length(tls),0:length(tls),type="n",axes=F,xlab="",ylab="")
        if (is.null(lty)) {
          if (is.null(density)) {
            legend(0,length(tls),tls,fill=fill,col=col,lty=lty,cex=cex,title=legendTitle)
          } else {
            ## 6. Ritu
            legend(0,length(tls),tls,col=fill,lty=lty,cex=cex,density=density,title=legendTitle)
            if (F) {
              text(1,k-1,legendTitle)
              for (k in 1:length(tls)) {
                rect(0,k-0.5,1,k+0.5,col=fill[k],density=density)
                text(2,k,tls[k])
              }
            }
          }
        } else {
          legend(0,length(tls),tls,col=col,lty=lty,cex=cex,title=legendTitle)
        }
    }
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
  cols <- heatmap_color
  
  ## Heatmap Output
  clusterObj=heatmap4(x = x, Rowv = Rowv, Colv = Colv, symm = FALSE,
           ColSideColors = ColSideColors, RowSideColors = RowSideColors, labCol = labCol, labRow = labRow,
           scale = "none", na.rm = FALSE, margins = margins, main = h_title, xlab = NULL, ylab = NULL,
           high = cols[1], low = cols[2], mid = cols[3], cexRowSide = cexRowSide, cexColSide = cexColSide, cexRow = cexRow,
           cexCol = cexCol, zlm=zlm, ...)
  ## Legend Output
  if (input_legend) {
      #cat("Legends not yet implemented ....")
      if (T) {
          cat("zlm:",exists("zlm"),"\n")
          if (!exists("zlm")) zlm=c(-.5,.5)
          ## 9. Ritu
          #heatmapColorBar(cols=cols,limit=zlm)
          heatmapColorBar(cols=cols,limit=zlm,k=5)
          if (input_legend & row_anno) {
              for (vId in 1:length(row_var))
            sampleColorLegend(tls = row_var, col = row_color, lty = NULL, legendTitle = legend_title, cex = NULL)
          }
          if (input_legend & row_anno) {
            cat("Legends ....")
            for (vId in 1:length(row_var)) {
                nm=sub("^ +","",sub(" +$","",rownames(row_color)[vId]))
                x=sort(unique(row_info[,row_var[vId]]))
                color_vec <- color_vec_cat_default
                if (!is.null(row_var_info)) {
                  if (row_var[vId] %in% names(row_var_info)) {
                    if ("color" %in% names(row_var_info[[row_var[vId]]])) {
                      color_vec <-  row_var_info[[row_var[vId]]]$color
                    }
                  if ("level" %in% names(row_var_info[[row_var[vId]]])) {
                    x <-  row_var_info[[row_var[vId]]]$level
                  }
                  }
                }
                sampleColorLegend(tls=x,col=color_vec,lty=NULL,legendTitle=nm,cex=NULL)
            }
          }
          if (input_legend & col_anno) {
            cat("Legends ....")
            for (vId in 1:length(col_var)) {
                nm=sub("^ +","",sub(" +$","",rownames(col_color)[vId]))
                if (is.numeric(col_info[ ,col_var[vId]]) & length(unique(col_info[ ,col_var[vId]])) > 5) {
                    varib <- col_info[ ,col_var[vId]]
                    color_vec <- color_vec_cont_default
                    if ("limit" %in% names(col_var_info[[col_var[vId]]])){
                        lim <-  col_var_info[[col_var[vId]]]$limit
                        varib=round(varib); varib[varib<lim[1]]=lim[1]; varib[varib>lim[2]]=lim[2]; varib=varib+lim[2]+1
                    } else {
                        x1=max(abs(varib-min(varib,na.rm=T)),na.rm=T)
                        varib=(100*(varib-min(varib,na.rm=T))/x1)+1
                        varib <- round(varib)
                        lim <- range(varib,na.rm=T)
                        lim <- round(range(col_info[ ,col_var[vId]],na.rm=T),2)
                    }
                    color_vec=rev(color_vec)
                    heatmapColorBar(cols=color_vec,limit=lim)

                } else {
                    x=sort(unique(col_info[,col_var[vId]]))
                    color_vec <- color_vec_cat_default
                    if (!is.null(col_var_info)) {
                      if (col_var[vId] %in% names(col_var_info)) {
                        if ("color" %in% names(col_var_info[[col_var[vId]]])) {
                          color_vec <-  col_var_info[[col_var[vId]]]$color
                        }
                      if ("level" %in% names(col_var_info[[col_var[vId]]])) {
                        x <-  col_var_info[[col_var[vId]]]$level
                      }
                      }
                    }
                    sampleColorLegend(tls=x,col=color_vec,lty=NULL,legendTitle=nm,cex=NULL)
                }
            }
          }
          # sampleColorLegend(tls = c("N0", "N+"), col = samColUniq[1:2], lty = NULL, legendTitle = "Node", cex = NULL)
          # tls = title, each annotation variable, default: used annotation variables, for loop
          # col = color, default: annotation default or specified by user
          # lty = line type, deafault = NULL
          # legendTitle, default = NULL
          # cex = text size, default = NULL
      }
  }

  #--------------------------------------------------------------------------------------------
  ## Clears graphics on device
  if (!is.null(file_name)){
    dev.off()
  }
  
  invisible(clusterObj)

}
