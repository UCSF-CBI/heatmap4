#' Heatmap internal function.
#'
#' Called by generate_heatmap to create heatmap.
#'
#' @param x numeric matrix of the values to be plotted.
#' @param imp logical impute missing values if TRUE.
#' @param Rowv determines if and how the row dendrogram should be computed and reordered. Either a dendrogram or a vector of values used to reorder the row dendrogram or NA to suppress any row dendrogram (and reordering) or by default, NULL, see ‘Details’ below.
#' @param Colv determines if and how the column dendrogram should be reordered. Has the same options as the Rowv argument above and additionally when x is a square matrix, Colv = "Rowv" means that columns should be treated identically to the rows (and so if there is to be no row dendrogram there will not be a column one either).
#' @param distfun function used to compute the distance (dissimilarity) between both rows and columns. Defaults to dist.
#' @param hclustfun function used to compute the hierarchical clustering when Rowv or Colv are not dendrograms. Defaults to hclust. Should take as argument a result of distfun and return an object to which as.dendrogram can be applied.
#' @param add.expr expression that will be evaluated after the call to image. Can be used to add components to the plot.
#' @param symm logical indicating if x should be treated symmetrically; can only be true when x is a square matrix.
#' @param revC logical indicating if the column order should be reversed for plotting, such that e.g., for the symmetric case, the symmetry axis is as usual.
#' @param scale character indicating if the values should be centered and scaled in either the row direction or the column direction, or none. The default is "row" if symm false, and "none" otherwise.
#' @param na.rm logical indicating whether NA's should be removed.
#' @param margins numeric vector of length 2 containing the margins (see graphics::par(mar = *)) for column and row names, respectively.
#' @param ColSideColors (optional) character vector of length ncol(x) containing the color names for a horizontal side bar that may be used to annotate the columns of x.
#' @param RowSideColors (optional) character vector of length nrow(x) containing the color names for a vertical side bar that may be used to annotate the rows of x.
#' @param cexRow, positive number, used as cex.axis in for the row axis labeling. The default currently only uses number of rows.
#' @param cexCol positive number, used as cex.axis in for the column axis labeling. The default currently only uses number of columns.
#' @param fontRow positive number, used as font.axis in for the row axis labeling. The default is 1.
#' @param fontCol positive number, used as font.axis in for the column axis labeling. The default is 1.
#' @param labRow character vector with row labels to use; defaults to rownames(x).
#' @param labCol character vector with column labels to use; these defaults to colnames(x).
#' @param lineRow (optional) positive number vector, used to draw horiontal lines. The default is NULL.
#' @param lineCol (optional) positive number vector, used to draw vertical lines. The default is NULL.
#' @param lineColor (optional) Color of lines drawn with lineRow and lineCol.
#' @param totalR .
#' @param totalC .
#' @param ncr .
#' @param ncc .
#' @param main main title; defaults to none.
#' @param xlab, x-axis title; defaults to none.
#' @param ylab y-axis title; defaults to none.
#' @param verbose logical indicating if information should be printed.
#' @param methodR Agglomeration method for clustering rows.
#' @param methodC Agglomeration method for clustering columns.
#' @param zlm the minimum and maximum x values for which colors should be plotted.
#' @param high color for high values.
#' @param low color for low values.
#' @param mid color for middle values.
#' @param addAmp (optional) matrix 0s and 1s of the same size as x to be added to the heatmap
#' @param colAmp (optional) color of addAmp
#' @param cexAmp (optional) size of addAmp
#' @param addText (optional) character matrix of the same size as x to be added to the heatmap
#' @param cexText (optional) color of addText
#' @param lwidHeatmap .
#' @param lheiHeatmap .
#' @param lwidRowSide .
#' @param lheiColSide .
#' @param cexRowSide .
#' @param cexColSide .
#' @param densColor .
#' @param sideLabRow .
#' @param sideLabCol .
#' @param layoutRespect .
#' @param sideColSide .
#' @param sideRowSide .
#' @param ... additional arguments passed on to image, e.g., col specifying the colors.
#' @return A List.
heatmap4 <- function (x, imp = TRUE, Rowv = NA, Colv = NULL, distfun = stats::dist,
hclustfun = stats::hclust, add.expr, symm = FALSE, revC = identical(Colv, "Rowv"),
scale = "none", na.rm = TRUE, margins = c(5,5), ColSideColors, RowSideColors,
cexRow = 0.2 + 1/log10(nr), cexCol = 0.2 + 1/log10(nc), fontRow=1, fontCol=1, labRow = NULL, labCol = NULL, lineRow = NULL, lineCol = NULL, lineColor="black",
totalR=nr, totalC=nc, ncr=NA, ncc=NA, main = NULL, xlab = NULL, ylab = NULL, verbose = getOption("verbose"),
methodR = "ward.D", methodC = "ward.D", zlm = c(-0.5, 0.5), high="green", low="red", mid="black",
addAmp=NULL, colAmp=NULL, cexAmp=.25, addText=NULL, cexText=1,
lwidHeatmap=4, lheiHeatmap=4, lwidRowSide=0.2, lheiColSide=0.2, cexRowSide=1,cexColSide=1, densColor=NULL, sideLabRow=4, sideLabCol=1, layoutRespect=T, sideColSide=c("left","right"), sideRowSide=c("bottom","top"), ...)
{
    if (!is.matrix(x)) {
        x <- matrix(x,ncol=1)
        if (!is.null(addAmp)) {
            addAmp <- matrix(addAmp,ncol=1)
        }
        ## 19. Ritu
        if (!is.null(addText)) {
            addText <- matrix(addText,ncol=1)
        }
    }
    scale <- if (symm && missing(scale)) "none" else match.arg(scale)
    if (length(di <- dim(x)) != 2 || !is.numeric(x)) stop("`x' must be a numeric matrix")
    nr <- di[1]
    nc <- di[2]
    ## 27. Ritu
    sideRowSide=sideRowSide[1]
    sideColSide=sideColSide[1]
    ## 4. Ritu
    #if (nr <= 1 || nc <= 1) stop("`x' must have at least 2 rows and 2 columns")
    if (nr <= 1 || nc < 1) stop("`x' must have at least 2 rows and 1 column")
    if (!is.numeric(margins) || length(margins) != 2) stop("`margins' must be a numeric vector of length 2")
    doRdend <- !identical(Rowv, NA)
    doCdend <- !identical(Colv, NA)
    if (is.null(Rowv)) Rowv <- rowMeans(x, na.rm = na.rm)
    if (is.null(Colv)) Colv <- colMeans(x, na.rm = na.rm)
    if (doRdend) {
        if (inherits(Rowv, "hclust")) {
            ## 16. Ritu
            hcr <- Rowv
            ddr <- stats::as.dendrogram(hcr)
        } else if (inherits(Rowv, "dendrogram"))
            ddr <- Rowv
        else {
            hcr <- hclustfun(distfun(x), method = methodR)
            ddr <- stats::as.dendrogram(hcr)
            if (!is.logical(Rowv) || Rowv) {
                ddr <- stats::reorder(ddr, Rowv)
                ## 30. Ritu
                hcr <- stats::as.hclust(ddr)
            }
        }
        if (nr != length(rowInd <- stats::order.dendrogram(ddr))) stop("row dendrogram ordering gave index of wrong length")
    } else rowInd <- 1:nr
    if (doCdend) {
        if (inherits(Colv, "hclust")) {
            ## 16. Ritu
            hcc <- Colv
            ddc <- stats::as.dendrogram(hcc)
        } else if (inherits(Colv, "dendrogram"))
            ddc <- Colv
        else if (identical(Colv, "Rowv")) {
            if (nr != nc) stop("Colv = \"Rowv\" but nrow(x) != ncol(x)")
            ddc <- ddr
        }
        else {
            hcc <- hclustfun(distfun(if (symm) x else t(x)), method = methodC)
            ddc <- stats::as.dendrogram(hcc)
            if (!is.logical(Colv) || Colv) {
                ddc <- stats::reorder(ddc, Colv)
                ## 30. Ritu
                hcc <- stats::as.hclust(ddc)
            }
        }
        if (nc != length(colInd <- stats::order.dendrogram(ddc))) stop("column dendrogram ordering gave index of wrong\nlength")
    } else colInd <- 1:nc
    ## 28. Ritu
    if (is.null(labRow)) labRow <- if (is.null(rownames(x))) (1:nr) else rownames(x)
    if (is.null(labCol)) labCol <- if (is.null(colnames(x))) (1:nc) else colnames(x)
    x <- x[rowInd, colInd]
    ## 28. Ritu
    #if (is.null(labRow)) labRow <- if (is.null(rownames(x))) (1:nr)[rowInd] else rownames(x)
    #if (is.null(labCol)) labCol <- if (is.null(colnames(x))) (1:nc)[colInd] else colnames(x)
    if (scale == "row") {
        x <- sweep(x, 1, rowMeans(x, na.rm = na.rm))
        sx <- apply(x, 1, stats::sd, na.rm = na.rm)
        x <- sweep(x, 1, sx, "/")
    } else if (scale == "column") {
        x <- sweep(x, 2, colMeans(x, na.rm = na.rm))
        sx <- apply(x, 2, stats::sd, na.rm = na.rm)
        x <- sweep(x, 2, sx, "/")
    }
    lmat <- rbind(c(NA, 3), 2:1)
    #lwid <- c(if (doRdend) 1 else 0.05, 4)
    #lhei <- c((if (doCdend) 1 else 0.05) + if (!is.null(main)) 0.2 else 0, 4)

    ## 27. Ritu
    ### 7. Ritu
    ##lwid <- c(1, 4)
    ##lhei <- c(1 + if (!is.null(main)) 0.2 else 0, 4)
    #lwid <- c(ifelse(doRdend,1,0.01), 4)
    #lhei <- c(ifelse(doCdend,1,0.01) + ifelse(is.null(main),0,0.2), 4)
    lwid <- c(ifelse(doRdend,1,0.01), lwidHeatmap)
    lhei <- c(ifelse(doCdend,1,0.01) + ifelse(is.null(main),0,0.2), lheiHeatmap)

    ## 2. Ritu
    if (!is.null(ColSideColors)) {
        #if (!missing(ColSideColors)) {
           # if (!is.character(ColSideColors) || length(ColSideColors) !=
            #    nc)
              #  stop("'ColSideColors' must be a character vector of\nlength ncol(x)")

            ## 6. Ritu
            if (!is.matrix(ColSideColors)) {
                ColSideColors <- matrix(ColSideColors,nrow=1)
                rownames(ColSideColors) <- ""
            }

        ## 11. Ritu
        #lmat <- rbind(lmat[1, ] + 1, c(NA, 1), lmat[2, ] + 1)
        lmat <- rbind(lmat[1, ] + 1, c(rep(NA, ncol(lmat) - 1),1), lmat[2, ] + 1)

        ## 24. Ritu
        lhei <- c(lhei[1], 0.2, lhei[2])
    }
    ## 3. Ritu
    if (!is.null(RowSideColors)) {
        #if (!missing(RowSideColors)) {
            #if (!is.character(RowSideColors) || length(RowSideColors) != nr)
            #    stop("'RowSideColors' must be a character vector of\nlength nrow(x)")
            #lmat <- cbind(lmat[, 1] + 1, c(rep(NA, nrow(lmat) - 1), 1), lmat[, 2] + 1)
            #lwid <- c(lwid[1], 0.2, lwid[2])

        ## 11. Ritu
        if (!is.matrix(RowSideColors)) {
            RowSideColors <- matrix(RowSideColors,nrow=1)
            rownames(RowSideColors) <- ""
        }
        lmat <- cbind(lmat[, 1] + 1, c(rep(NA, nrow(lmat) - 1), 1), lmat[, 2] + 1)
        ## 24. Ritu
        #lwid <- c(lwid[1], 0.2, lwid[2])
        lwid <- c(lwid[1], lwidRowSide, lwid[2])
    }
    lmat[is.na(lmat)] <- 0
    if (verbose) {
        cat("layout: widths = ", lwid, ", heights = ", lhei, "; lmat=\n")
        print(lmat)
    }

################################
#redo lmat:

    ## 1. Ritu
    #if (!is.matrix(ColSideColors)) {
        #ColSideColors <- matrix(ColSideColors,nrow=1,dimnames=list(names(ColSideColors),1:nc))
    #}

    ## 2. Ritu
    if (!is.null(ColSideColors)) {
        nr.my=nrow(ColSideColors)+2

        ## 11. Ritu
        #nc.my=3
        #nfig=nr.my+2
        #lmat=matrix(0, nrow=nr.my, ncol=nc.my)
        #lmat[nr.my,]=c(nfig-1,1,nfig-2)
        #lmat[1, nc.my]=nfig
        #lmat[2:(nr.my-1),nc.my]=(nfig-3):2
        nc.my=2+ifelse(is.null(RowSideColors),0,nrow(RowSideColors))
        nfig=nr.my+nc.my-1
        lmat=matrix(0, nrow=nr.my, ncol=nc.my)
        if (is.null(RowSideColors)) {
            lmat[nr.my,]=c(nfig-1,nfig-2)
        } else {
            lmat[nr.my,]=c(nfig-1,1:nrow(RowSideColors),nfig-2)
            ## 27. Ritu
            ## 24. Ritu
            ##lwid <- c(ifelse(doRdend,1,0.01), rep(.2, nrow(RowSideColors)), 4)
            #lwid <- c(ifelse(doRdend,1,0.01), rep(lwidRowSide, nrow(RowSideColors)), 4)
            lwid <- c(ifelse(doRdend,1,0.01), rep(lwidRowSide, nrow(RowSideColors)), lwidHeatmap)
        }
        lmat[1, nc.my]=nfig
        lmat[2:(nr.my-1),nc.my]=(nfig-3):(nc.my-1)

        ## 11. Ritu
        ### 10. Ritu
        #if (is.null(RowSideColors)) {
        #    lmat=lmat[,c(1,3:ncol(lmat))]
        #    lmat=lmat-1
        #    lmat[lmat==(-1)]=0
        #}

        ## 27. Ritu
        ### 24. Ritu
        #### 7. Ritu
        ###lhei=c(1, rep(.2, nrow(ColSideColors)),4)
        ##lhei <- c(ifelse(doCdend,1,0.01) + ifelse(is.null(main),0,0.2), rep(.2, nrow(ColSideColors)), 4)
        #lhei <- c(ifelse(doCdend,1,0.01) + ifelse(is.null(main),0,0.2), rep(lheiColSide, nrow(ColSideColors)), 4)
        lhei <- c(ifelse(doCdend,1,0.01) + ifelse(is.null(main),0,lheiColSide), rep(lheiColSide, nrow(ColSideColors)), lheiHeatmap)
    }
    
    if (F) {
        ## 11. Ritu
        if (!is.null(RowSideColors)) {
            nr.my=nrow(RowSideColors)+2
            nc.my=nrow(ColSideColors)+2
            nfig=nr.my+2
            lmat=matrix(0, nrow=nr.my, ncol=nc.my)
            lmat[nr.my,]=c(nfig-1,1,nfig-2)
            lmat[1, nc.my]=nfig
            lmat[2:(nr.my-1),nc.my]=(nfig-3):2
            if (is.null(ColSideColors)) {
                lmat=lmat[,c(1,3:ncol(lmat))]
                lmat=lmat-1
                lmat[lmat==(-1)]=0
            }
            lhei <- c(ifelse(doCdend,1,0.01) + ifelse(is.null(main),0,0.2), rep(.2, nrow(RowSideColors)), 4)
        }
    }

###################################
    op <- graphics::par(no.readonly = TRUE)
    on.exit(graphics::par(op))
    if (verbose) {
        cat("Final layout: widths = ", lwid, ", heights = ", lhei, "; lmat=\n")
        print(lmat)
    }

    ## 26. Ritu
    ##graphics::layout(lmat, widths = lwid, heights = lhei, respect = TRUE)
    #layoutThis=graphics::layout(lmat, widths = lwid, heights = lhei, respect = TRUE)
    layoutThis=graphics::layout(lmat, widths = lwid, heights = lhei, respect = layoutRespect)
    #layout.show(layoutThis)
    #layout.show(n=nfig)

    ## 3. Ritu
    if (!is.null(RowSideColors)) {
    #if (!missing(RowSideColors)) {
        graphics::par(mar = c(margins[1], 0, 0, 0.5))

        ## 11. Ritu
        #if (revC) {
        #    graphics::image(rbind(1:nr), col = RowSideColors[rev(rowInd)], axes = FALSE)
        #} else {
        #    graphics::image(rbind(1:nr), col = RowSideColors[rowInd], axes = FALSE)
        #}
        if (revC) {
            j=rev(rowInd)
        } else {
            j=rowInd
        }
        for (i in 1:nrow(RowSideColors)) {
            ## 27. Ritu
            #graphics::image(rbind(1:nr), col = RowSideColors[i,j], axes = FALSE)
            ##graphics::image(rbind(1:nr), z=rbind(1:nr),col = RowSideColors[i,j], axes = FALSE)
            ##graphics::image(cbind(1:nr), z=cbind(1:nr),col = RowSideColors[i,j], axes = FALSE)
            #graphics::image(rbind(1:nr), col = RowSideColors[i,j], axes = FALSE, ylim = 0.5 + c(0, totalR))
            graphics::image(rbind(1:nr), col = RowSideColors[i,j], axes = FALSE)
            #graphics::image(rbind(1:nr), z=cbind(1:nr),col = RowSideColors[i,j], axes = FALSE, ylim = 0.5 + c(0, totalR))
            
            ## 24. Ritu
            if (!is.null(densColor)) {
                if (any(is.na(RowSideColors[i,j]))) {
                    for (jj in which(is.na(RowSideColors[i,j]))) {
                        graphics::rect(-1,jj-0.5,1,jj+0.5,density=densColor)
                    }
                }
            }
            ## 26. Ritu
            ### 24. Ritu
            ##graphics::mtext(side=1, text=as.character(rownames(RowSideColors)[i]), las=3, cex=1)
            #graphics::mtext(side=1, text=as.character(rownames(RowSideColors)[i]), las=3, cex=cexRowSide)
            graphics::mtext(side=ifelse(sideRowSide=="bottom",1,3), text=as.character(rownames(RowSideColors)[i]), las=3, cex=cexRowSide)
        }
    }
    ## 2. Ritu
    if (!is.null(ColSideColors)) {
        #if (!missing(ColSideColors)) {
        graphics::par(mar = c(0.5, 0, 0, margins[2]))
        for (i in 1:nrow(ColSideColors)) {
            ## 9. Ritu
            #graphics::image(cbind(1:nc), col = ColSideColors[i,colInd], axes = FALSE)
            graphics::image(cbind(1:nc), z=cbind(1:nc),col = ColSideColors[i,colInd], axes = FALSE, xlim = 0.5 + c(0, totalC))
            ## 24. Ritu
            if (!is.null(densColor)) {
                if (any(is.na(ColSideColors[i,colInd]))) {
                    for (jj in which(is.na(ColSideColors[i,colInd]))) {
                        graphics::rect(jj-0.5,-1,jj+0.5,1,density=densColor)
                    }
                }
            }
            ## 26. Ritu
            ## 24. Ritu
            #graphics::mtext(side=2, text=as.character(rownames(ColSideColors)[i]), las=1, cex=1)
            graphics::mtext(side=ifelse(sideColSide=="left",2,4), text=as.character(rownames(ColSideColors)[i]), las=1, cex=cexColSide)
        }
    }
    graphics::par(mar = c(margins[1], 0, 0, margins[2]))
    if (!symm || scale != "none")
        x <- t(x)
    if (revC) {
        iy <- nr:1
        ddr <- rev(ddr)
        x <- x[, iy]
    }
    else iy <- 1:nr
    x.floor <- x
    for (i in 1:ncol(x)) {
        ind1 <- (1:length(x[, i]))[x[, i] >= zlm[2] & !is.na(x[,i])]
        ind2 <- (1:length(x[, i]))[x[, i] <= zlm[1] & !is.na(x[,i])]
        x.floor[, i][ind1] <- rep((zlm[2] - 0.01), length(ind1))
        x.floor[, i][ind2] <- rep((zlm[1] + 0.01), length(ind2))
    }
    ## 5. Ritu
    #graphics::image(1:nc, 1:nr, x.floor, xlim = 0.5 + c(0, nc), ylim = 0.5 + c(0, nr), axes = FALSE, xlab = "", ylab = "", col = maPalette(high = high, low = low, mid = mid), zlim = zlm, ...)
    if (length(high)>1) {
        ## 20. Ritu
        colThis=high
    } else {
        colThis=marray::maPalette(high = high, low = low, mid = mid)
    }
    if (nc==1) {
        ## 27. Ritu
        ### 20. Ritu
        ##graphics::image(1:(2*nc), 1:nr, rbind(x.floor,x.floor), xlim = 0.5 + c(0, 2*totalC), ylim = 0.5 + c(0, nr), axes = FALSE, xlab = "", ylab = "", col = maPalette(high = high, low = low, mid = mid), zlim = zlm, ...)
        #graphics::image(1:(2*nc), 1:nr, rbind(x.floor,x.floor), xlim = 0.5 + c(0, 2*totalC), ylim = 0.5 + c(0, nr), axes = FALSE, xlab = "", ylab = "", col = colThis, zlim = zlm, ...)
        graphics::image(1:(2*nc), 1:nr, rbind(x.floor,x.floor), xlim = 0.5 + c(0, 2*totalC), ylim = 0.5 + c(0, totalR), axes = FALSE, xlab = "", ylab = "", col = colThis, zlim = zlm, ...)
    } else {
        ## 27. Ritu
        ### 20. Ritu
        ##graphics::image(1:nc, 1:nr, x.floor, xlim = 0.5 + c(0, totalC), ylim = 0.5 + c(0, nr), axes = FALSE, xlab = "", ylab = "", col = maPalette(high = high, low = low, mid = mid), zlim = zlm, ...)
        #graphics::image(1:nc, 1:nr, x.floor, xlim = 0.5 + c(0, totalC), ylim = 0.5 + c(0, nr), axes = FALSE, xlab = "", ylab = "", col = colThis, zlim = zlm, ...)
        graphics::image(1:nc, 1:nr, x.floor, xlim = 0.5 + c(0, totalC), ylim = 0.5 + c(0, totalR), axes = FALSE, xlab = "", ylab = "", col = colThis, zlim = zlm, ...)
    }
    ## 15. Ritu
    if (!is.null(lineCol)) {
        ## 22. Ritu
        #graphics::abline(v=lineCol)
        graphics::abline(v=lineCol,col=lineColor)
    }
    ## 15. Ritu
    if (!is.null(lineRow)) {
        ## 27. Ritu
        ### 22. Ritu
        ##graphics::abline(h=lineRow)
        #graphics::abline(h=lineRow,col=lineColor)
        for (i in lineRow) graphics::lines(x=c(1-0.5,nc+0.5),y=rep(i,2), col=lineColor)
    }

##################

## 19. Ritu
if (!is.null(addText)) {
    addText=addText[rowInd, colInd]
    if (!is.matrix(addText)) {
        addText <- matrix(addText,ncol=1)
    }
    for (i in 1:ncol(addText)) {
        j=which(!is.na(addText[,i]))
        ## 23. Ritu
        #graphics::points(rep(i, length(j)), j, pch=addText[j,i])
        if (length(j)!=0) graphics::text(i, j, labels=addText[j,i], cex=cexText)
    }
}

if (!is.null(addAmp)) {
    addAmp=addAmp[rowInd, colInd]
    ## 4. Ritu
    if (!is.matrix(addAmp)) {
        addAmp <- matrix(addAmp,ncol=1)
    }
    for (i in 1:ncol(addAmp)) {
        amp=which(addAmp[,i]>0)
        ## 12. Ritu
        ### 8. Ritu
        ##graphics::points(rep(i, length(amp)), amp, col=colAmp, cex=.75, pch=20)
        #graphics::points(rep(i, length(amp)), amp, col=colAmp, cex=.25, pch=20)
        graphics::points(rep(i, length(amp)), amp, col=colAmp, cex=cexAmp, pch=20)
    }
 }
    
################

    ## 25. Ritu
    ### 17. Ritu
    ##graphics::axis(1, 1:nc, labels = labCol[colInd], las = 2, line = -0.5, tick = 0, cex.axis = cexCol)
    #graphics::axis(1, 1:nc, labels = labCol[colInd], las = 2, line = -0.5, tick = 0, cex.axis = cexCol, font.axis=fontCol)
    graphics::axis(sideLabCol, 1:nc, labels = labCol[colInd], las = 2, line = -0.5, tick = 0, cex.axis = cexCol, font.axis=fontCol)
    if (!is.null(xlab))
        graphics::mtext(xlab, side = 1, line = margins[1] - 1.25)
    ## 25. Ritu
    ### 17. Ritu
    ##graphics::axis(4, iy, labels = labRow[rowInd], las = 2, line = -0.5, tick = 0, cex.axis = cexRow)
    #graphics::axis(4, iy, labels = labRow[rowInd], las = 2, line = -0.5, tick = 0, cex.axis = cexRow, font.axis=fontRow)
    graphics::axis(sideLabRow, iy, labels = labRow[rowInd], las = 2, line = -0.5, tick = 0, cex.axis = cexRow, font.axis=fontRow)
    if (!is.null(ylab))
        graphics::mtext(ylab, side = 4, line = margins[2] - 1.25)
    if (!missing(add.expr))
        eval(substitute(add.expr))
    ## 27. Ritu
    #graphics::par(mar = c(margins[1], 0, 0, 0))
    xr=ifelse(totalR==nr,margins[1],2*(totalR-nr)+1+margins[1])
    xc=0
    graphics::par(mar = c(xr, 0, 0, xc))
    if (doRdend) {
        plot(ddr, horiz = TRUE, axes = FALSE, yaxs = "i", leaflab = "none")
        ## 16. Ritu
        if (exists("hcr") & !is.na(ncr)) {
            ## 18. Ritu
            #if (inherits(hcc,"hclust")) rect.hclust(hcr,k=ncr) else stop("Must be of class hclust to delineate clusters")
            if (inherits(hcr,"hclust")) rect.hclust.my(hcr,k=ncr,horiz=TRUE) else stop("Must be of class hclust to delineate clusters")
        }
    } else {
        graphics::frame()
    }
    ## 27. Ritu
    ## 9. Ritu
    #if (totalC==nc) {
    #    graphics::par(mar = c(0, 0, if (!is.null(main)) 1 else 0, margins[2]))
    #} else {
    #    graphics::par(mar = c(0, 0, if (!is.null(main)) 1 else 0, 2*(totalC-nc)+1+margins[2]))
    #}
    xr=0
    xc=ifelse(totalC==nc,margins[2],2*(totalC-nc)+1+margins[2])
    graphics::par(mar = c(xr, 0, if (!is.null(main)) 1 else 0, xc))
    if (doCdend) {
        plot(ddc, axes = FALSE, xaxs = "i", leaflab = "none")
        ## 16. Ritu
        if (exists("hcc") & !is.na(ncc)) {
            ## 18. Ritu
            #if (inherits(hcc,"hclust")) rect.hclust(hcc,k=ncc) else stop("Must be of class hclust to delineate clusters")
            if (inherits(hcc,"hclust")) rect.hclust.my(hcc,k=ncc) else stop("Must be of class hclust to delineate clusters")
        }
    } else if (!is.null(main)) {
        graphics::frame()
    }
    if (!is.null(main)) {
        ## 27. Ritu
        if (doCdend) {
            graphics::title(main, cex.main=0.8 * op[["cex.main"]])
            #graphics::text(x=20,y=5,main,cex=1 * op[["cex.main"]],font=2)
        } else {
            graphics::text(x=0.5,y=0.5,main,cex=1.5 * op[["cex.main"]],font=2)
        }
        #graphics::title(main, cex.main = 1.5 * op[["cex.main"]])
        #graphics::title(main, cex.main = 1.5 * op[["cex.main"]],line = margins[1] - 1.25)
        #plot(1,type="n",xaxt="n",yaxt="n",xlab="",ylab="");
        #graphics::text(x=10+margins[1] - 1.25,y=1,"test",cex=1.5 * op[["cex.main"]],)
        #graphics::text(x=1,y=1,c("test","ok"),cex=1.5 * op[["cex.main"]])
        #graphics::text(x=0.5,y=1,c("tex"),cex=1.5 * op[["cex.main"]])
        #graphics::points(rep(0,10),1:10)
    }

    ## 13. Ritu
    #invisible(list(rowInd = rowInd, colInd = colInd))
    out=list(rowInd = rowInd, colInd = colInd, rowClust=NULL, colClust=NULL)

    ## 14. Ritu
    #if (doRdend) out$rowClust=hcr
    #if (doCdend) out$colClust=hcc
    if (exists("hcr")) out$rowClust=hcr
    if (exists("hcc")) out$colClust=hcc
    
    ## 29. Ritu
    if (exists("hcr") & !is.na(ncr)) {
        y=stats::cutree(hcr,ncr)
        y=y[out$rowInd]
        out$rowClustId=rep(NA,length(y))
        k1=which(!duplicated(y))
        for (k in 1:ncr) {
            out$rowClustId[which(y==y[k1[k]])]=k
        }
    }
    if (exists("hcc") & !is.na(ncc)) {
        y=stats::cutree(hcc,ncc)
        y=y[out$colInd]
        out$colClustId=rep(NA,length(y))
        k1=which(!duplicated(y))
        for (k in 1:ncc) {
            out$colClustId[which(y==y[k1[k]])]=k
        }
    }

    invisible(out)
}

#' rect.hclust.my.
#'
#' Draws rectangles around the branches of a dendrogram highlighting the corresponding clusters. First the dendrogram is cut at a certain level, then a rectangle is drawn around selected branches.
#'
#' @param tree an object of the type produced by hclust.
#' @param k Scalar. Cut the dendrogram such that exactly k clusters are produced.
#' @param h Scalar. Cut the dendrogram by cutting at height h.
#' @param which A vector selecting the clusters around which a rectangle should be drawn. which selects clusters by number (from left to right in the tree). Default is which = 1:k.
#' @param x A vector selecting the clusters around which a rectangle should be drawn. x selects clusters containing the respective horizontal coordinates.
#' @param border Vector with border colors for the rectangles.
#' @param cluster Optional vector with cluster memberships as returned by stats::cutree(hclust.obj, k = k), can be specified for efficiency if already computed.
#' @param horiz Horizontal or vertical.
#' @return (Invisibly) returns a list where each element contains a vector of data points contained in the respective cluster.
rect.hclust.my=function (tree, k = NULL, which = NULL, x = NULL, h = NULL, border = 2, cluster = NULL, horiz = FALSE) {
    if (length(h) > 1L | length(k) > 1L) stop("'k' and 'h' must be a scalar")
    if (!is.null(h)) {
        if (!is.null(k))
        stop("specify exactly one of 'k' and 'h'")
        k <- min(which(rev(tree$height) < h))
        k <- max(k, 2)
    } else if (is.null(k)) {
        stop("specify exactly one of 'k' and 'h'")
    }
    if (k < 2 | k > length(tree$height))
    stop(gettextf("k must be between 2 and %d", length(tree$height)), domain = NA)
    if (is.null(cluster))
    cluster <- stats::cutree(tree, k = k)
    clustab <- table(cluster)[unique(cluster[tree$order])]
    m <- c(0, cumsum(clustab))
    if (!is.null(x)) {
        if (!is.null(which))
        stop("specify exactly one of 'which' and 'x'")
        which <- x
        for (n in seq_along(x)) which[n] <- max(which(m < x[n]))
    }
    else if (is.null(which))
    which <- 1L:k
    if (any(which > k))
    stop(gettextf("all elements of 'which' must be between 1 and %d", k), domain = NA)
    border <- rep_len(border, length(which))
    retval <- list()
    for (n in seq_along(which)) {
        if (horiz) {
            ## 18. Ritu
            graphics::rect(mean(rev(tree$height)[(k - 1):k]), m[which[n]] + 0.66, graphics::par("usr")[2L], m[which[n] + 1] + 0.33, border = border[n])
        } else {
            graphics::rect(m[which[n]] + 0.66, graphics::par("usr")[3L], m[which[n] + 1] + 0.33, mean(rev(tree$height)[(k - 1):k]), border = border[n])
        }
#        graphics::rect(xleft, ybottom, xright, ytop, density = NULL, angle = 45,col = NA, border = NULL, lty = graphics::par("lty"), lwd = graphics::par("lwd"),
        retval[[n]] <- which(cluster == as.integer(names(clustab)[which[n]]))
    }
    invisible(retval)
}

######################################################################
######################################################################
######################################################################
