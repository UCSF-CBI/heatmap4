#' Calibration bar for color images.
#'
#' This function produces a color image (color bar)
#'
#' @param x .
#' @param horizontal .
#' @param col .
#' @param scale .
#' @param k .
#' @param cexAxis .
#' @param ... additional arguments.
#' @return A color vector.
colorBar=function (x, horizontal = TRUE, col = grDevices::heat.colors(50), scale = 1:length(x),k = 10, cexAxis=1, ...) {
    if (is.numeric(x)) {
        x <- x
        colmap <- col
    }
    else {
        colmap <- x
        low <- range(scale)[1]
        high <- range(scale)[2]
        x <- seq(low, high, length = length(x))
    }
    if (length(x) > k) {
        x.small <- seq(x[1], x[length(x)], length = k)
    } else {x.small <- x}
    if (horizontal) {
        graphics::image(x, 1, matrix(x, length(x), 1), axes = FALSE, xlab = "",ylab = "", col = colmap, ...)
        graphics::axis(1, at = rev(x.small), labels = signif(rev(x.small),2), srt = 270,cex.axis=cexAxis)
    }
    if (!horizontal) {
        graphics::image(1, x, matrix(x, 1, length(x)), axes = FALSE, xlab = "",ylab = "", col = colmap, ...)
        graphics::par(las = 1)
        graphics::axis(4, at = rev(x.small), labels = signif(rev(x.small),2),cex.axis=cexAxis)
        graphics::par(las = 0)
    }
    #box()
}

#' Returns a color bar.
#'
#' This function produces a color image (color bar) which can be used for the legend to another color image obtained from heatmap4.
#'
#' @param limit .
#' @param cols .
#' @param main .
#' @param marginHMCBar .
#' @param cexAxisHMCBar .
#' @param ... additional arguments.
#' @export
#' @return A color vector.
heatmapColorBar <- function(limit,cols=c("green","red","black"),main=NULL,marginHMCBar=NULL,cexAxisHMCBar=1,...) {
    if (!is.null(marginHMCBar)) {
        parOrig=graphics::par("mar")
        graphics::par(mar=c(marginHMCBar[1], 4, marginHMCBar[2], 2) + 0.1)
    }
    if (length(cols)==3) {
        try <- marray::maPalette(high=cols[1], low=cols[2], mid=cols[3])
    } else {
        try <- marray::maPalette(high=cols[1], low=cols[2])
    }
    #marray::maColorBar(try, scale=limit,main=main,...)
    colorBar(try, scale=limit,main=main,cex.lab=2,cexAxis=cexAxisHMCBar,...)
    if (!is.null(marginHMCBar)) {
        graphics::par(mar=parOrig)
    }
}
#' Returns a legend.
#'
#' This function can be used to add legends to row and column color bars.
#'
#' @param tls .
#' @param col .
#' @param lty .
#' @param border .
#' @param pch .
#' @param lwd .
#' @param legendTitle .
#' @param cex .
#' @param density .
#' @param ... additional arguments.
#' @export
sampleColorLegend <- function(tls,col=NULL,lty=NULL,border=NULL,pch=NULL,lwd=NULL,legendTitle=NULL,cex=NULL,density=NULL,...) {
	nTypes <- length(tls)
	if (is.null(col)) {
		cl <- RColorBrewer::brewer.pal(8, "Accent")
		cl <- cl[1:min(nTypes,length(cl))]
		if (length(cl)<nTypes) {
			cl=c(RColorBrewer::brewer.pal(nTypes,"Set3"),RColorBrewer::brewer.pal(nTypes,"Set2"))[1:nTypes]
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
    if (is.null(border)) border=fill
	n <- length(tls)
	ii <- 1:length(tls)
	if (is.null(cex)) {
		cex=ifelse(max(nchar(tls))>13,1.5,3)
		if (nTypes>6) {cex=1.5}
	}
	plot(0:length(tls),0:length(tls),type="n",axes=F,xlab="",ylab="")
	if (is.null(lty)) {
        if (is.null(density)) {
            graphics::legend(0,length(tls),tls,fill=fill,col=col,lty=lty,border=border,cex=cex,title=legendTitle,...)
        } else {
            graphics::legend(0,length(tls),tls,col=fill,lty=lty,border=border,cex=cex,density=density,title=legendTitle,...)
            if (F) {
                graphics::text(1,k-1,legendTitle)
                for (k in 1:length(tls)) {
                    graphics::rect(0,k-0.5,1,k+0.5,col=fill[k],density=density)
                    graphics::text(2,k,tls[k])
                }
            }
        }
	} else {
        graphics::legend(0,length(tls),tls,col=col,lty=lty,pch=pch,lwd=lwd,cex=cex,title=legendTitle,...)
	}
}

#' Returns a distance matrix.
#'
#' This function computes correlation coefficients and returns them as a distance matrix.
#'
#' @param dat a numeric matrix passed to function cor.
#' @param method a character string indicating which correlation coefficient is to be computed. One of "pearson" (default), "kendall", or "spearman": can be abbreviated.
#' @param absolute a logical value. TRUE means absolute value of correlation is to be returned.
#' @return an object of class "dist".
getDist <- function(dat,method="pearson",absolute=FALSE) {
	if (method%in%c("pearson","spearman","kendall")) {
		y=stats::cor(t(dat),use="complete.obs")
		if (absolute) y=abs(y)
		stats::as.dist(1-y)
	} else {
		stats::dist(dat, method=method)
	}
}

#' Cohen's kappa coefficient based distance matrix.
#'
#' This function computes Cohen's kappa coefficients and returns them as a distance matrix.
#'
#' @param x a numeric matrix passed to function cor.
#' @param type a character string indicating which correlation coefficient (or covariance) is to be computed. One of "pearson" (default), "kendall", or "spearman": can be abbreviated.
#' @param absolute a logical value. TRUE means absolute value of correlation is to be returned.
#' @return an object of class "dist".
getKappaDist=function(x,type="Cohen",absolute=FALSE) {
	res=matrix(nrow=ncol(x),ncol=ncol(x),dimnames=list(colnames(x),colnames(x)))
	for (k1 in 1:ncol(x)) {
		for (k2 in 1:ncol(x)) {
			res[k1,k2]=res[k2,k1]=psy::lkappa(x[,c(k1,k2)], type=type)
		}
	}
	if (absolute) res=abs(res)
	stats::as.dist(1-res)
}

#' Cosine distance based distance matrix.
#'
#' This function computes cosine distances and returns them as a distance matrix.
#'
#' @param x a numeric matrix.
#' @return an object of class "dist".
getCosineDist=function(x) {
    cos.sim <- function(ix) {
        A = x[ix[1],]
        B = x[ix[2],]
        return( sum(A*B)/sqrt(sum(A^2)*sum(B^2)) )
    }
    res=matrix(nrow=ncol(x),ncol=ncol(x),dimnames=list(colnames(x),colnames(x)))
    n <- nrow(x)
    cmb <- expand.grid(i=1:n, j=1:n)
    res <- matrix(apply(cmb,1,cos.sim),n,n,dimnames=list(rownames(x),rownames(x)))
    stats::as.dist(1-res)
}

#' Hierarchical cluster analysis based on various distance methods.
#'
#' This function computes distance matrix and then performs hierarchical clustering.
#'
#' @param dat a numeric matrix.
#' @param distMethod distance function.
#' @param linkMethod agglomeration method.
#' @param absolute a logical value. TRUE means absolute value of correlation is to be returned.
#' @return an object of class "hclust".
getCluster=function(dat,distMethod,linkMethod,absolute=F) {
    switch(distMethod,
        "cosine"={
            if (any(apply(dat,2,sum,na.rm=T)==0)) dat=dat+1
            distMat=getCosineDist(t(dat))
            clustThis=stats::hclust(distMat,method=linkMethod)},
        "pearson"={
            x=stats::cor(dat,method=distMethod,use="complete.obs")
            if (absolute) x=abs(x)
            distMat=stats::as.dist(1-x)
            clustThis=stats::hclust(distMat,method=linkMethod)},
        "spearman"={
            x=stats::cor(dat,method=distMethod,use="complete.obs")
            if (absolute) x=abs(x)
            distMat=stats::as.dist(1-x)
            clustThis=stats::hclust(distMat,method=linkMethod)},
        "kappa"={
            distMat=getKappaDist(dat,absolute=absolute)
            clustThis=stats::hclust(distMat,method=linkMethod)},
        "euclidean"={
            distMat=stats::dist(t(dat),method=distMethod)
            clustThis=stats::hclust(distMat,method=linkMethod)}
    )
    invisible(clustThis)
}


#' Cluster designation.
#'
#' This function cuts the tree into desired number of groups.
#'
#' @param clustObj a hclust object.
#' @param ann a data.frame containing annotation for each observation.
#' @param nClust number of groups desired.
#' @param rev reverse the order of observations.
#' @export
#' @return a data.frame.
cutCluster=function(clustObj,ann,nClust=2,rev=F) {
    if (is.na(nClust)) {
        if (inherits(clustObj,"hclust")) {
            tbl=cbind(ann[clustObj$order,],order=1:nrow(ann))
        } else {
            tbl=cbind(ann,order=1:nrow(ann))
        }
    } else {
        tbl=matrix(nrow=nrow(ann),ncol=nClust-1)
        colnames(tbl)=paste("clustId_",1:ncol(tbl)+1,sep="")
        #for (kk in kVec) {
        for (kk in 1:ncol(tbl)) {
            clustId=stats::cutree(clustObj,k=kk+1)[clustObj$order]
            k1=which(!duplicated(clustId))
            for (k in 1:length(k1)) {
                if (rev) kThis=length(k1)-k+1 else kThis=k
                clustId[which(clustId==clustId[k1[k]])]=paste("cluster",kThis,sep="")
            }
            tbl[,kk]=clustId
        }
        tbl=cbind(ann[clustObj$order,],tbl,order=1:nrow(ann))
    }
    if (rev) {
        tbl=tbl[rev(1:nrow(tbl)),]
        tbl$order=1:nrow(tbl)
    }
    invisible(tbl)
}
