## 1. Ritu 01/11/09 Fixed logic for "lty" option & removed param box.lty
## 2. Ritu 02/21/14 New function getKappaDist
##					New param absolute for getDist
## 3. Ritu 03/08/16 New function getCosineDist
## 4. Ritu 04/14/17 Set row & column names in getCosineDist
## 5. Ritu 05/29/17 Set row & column names in getCosineDist
## 6. Ritu 01/27/18 New option density in function sampleColorLegend

plotHeatmap <- function(dataAcgh, clInfo, distMethod="euclidean",clustCol=T,clustRow=F,numChrom=nAut,resp=NULL,samId=1:ncol(dataAcgh),samName=NULL,amplif=NULL,phenName="",phenColor=NULL, main=NULL, candidateClone=F,cloneInfo=NULL,margins=c(5,5),chrInfo.this=chrInfo,chromCol=c("skyblue","blue","yellowgreen","yellow"),cexCol=NULL) {
	if (!is.null(resp)) {
		if (!is.matrix(resp)) {
			resp <- matrix(resp,nrow=1)
		}
		nPhen <- nrow(resp)
	}

	## ------------------------------------------
	## Chroms
	ind.cl <- which(clInfo$Target%in%clones.info(acghNA)$Target)
	chromcols <- rep(NA, nrow(clInfo))
	for (i in 1:numChrom) {
		if (is.odd(i)) {
			chromcols[which(clInfo$Chrom==i & clInfo$kb < chrInfo.this$centr[i])] <- chromCol[1]
			chromcols[which(clInfo$Chrom==i & clInfo$kb >= chrInfo.this$centr[i])] <- chromCol[2]
		} else if (!(is.odd(i))) {
			chromcols[which(clInfo$Chrom==i & clInfo$kb < chrInfo.this$centr[i])] <- chromCol[3]
			chromcols[which(clInfo$Chrom==i & clInfo$kb >= chrInfo.this$centr[i])] <- chromCol[4]
		}
	}

	## ------------------------------------------

	dat=dataAcgh[ind.cl,samId]
	#colnames(dat) <- colnames(dataAcgh)[samId]

	## ------------------------------------------
	## Samples
	if (is.null(samName)) {samName <- colnames(dataAcgh)[samId]}

	if (is.null(resp)) {
		sampCols <- NULL
	} else {
		sampCols=matrix(NA, nrow=nPhen, ncol=length(samId))
		for (i in 1:nrow(resp)) {
			respUniq <- 0:(length(unique(resp[i,][!is.na(resp[i,])]))-1)
			if (is.null(phenColor) || is.null(phenColor[[i]])) {
				tmp=brewer.pal(min(length(respUniq),3),"Accent")
				if (length(tmp)<length(respUniq)) {
					tmp=c(brewer.pal(length(respUniq),"Set3"),brewer.pal(length(respUniq),"Set2"))[1:length(respUniq)]
					tmp=c(brewer.pal(length(respUniq),"Set3"),brewer.pal(length(respUniq),"Set2"))[1:length(respUniq)]
					tmp[1] <- "#1F78B4"
					if (length(tmp)>8) {
						tmp[9] <- "#999999"
					}
				}
				tmp<- tmp[1:length(respUniq)]
				tmp[tmp=="#FFFF99"]="#FFFF60"
			} else {
				tmp <- phenColor[[i]]
			}
			sampcols <- rep(NA, length(resp[i,]))
			for (j in 1:length(respUniq)) {
				sampcols[which(resp[i,]==respUniq[j])] <- tmp[j]
			}
			sampcols[which(sampcols=="#FFFF99")]="yellow2"
			sampCols[i,]=sampcols
		}
		sampCols <- matrix(sampCols,nrow=nPhen)
		rownames(sampCols) <- phenName
	}
	## ------------------------------------------

	cat("Check for flooring:\n")
	print(round(summary(as.vector(dat)),2))
	if (!is.null(distMethod)) {
		if (distMethod=="euclidean") {distfun <- dist
		} else {distfun <- getDist}
	}

	dat <- dataAcgh[ind.cl,samId][rev(1:nrow(clInfo[ind.cl,])),]
	if (candidateClone) {
		id <- as.character(clInfo$Clone)[ind.cl][rev(1:nrow(clInfo[ind.cl,]))]%in%as.character(clInfo$Clone)
		dat <- dat[id,]
		cloneName <- paste(clInfo$Clone," chr",clInfo$Chr," kb",clInfo$kb,sep="")[ind.cl][rev(1:nrow(clInfo[ind.cl,]))][id]
		tmp <- as.character(clInfo$Clone)[ind.cl][rev(1:nrow(clInfo[ind.cl,]))][id]
		id <- match(tmp,as.character(cloneInfo[,1]))
		cloneCol <- rep("white",length(cloneName))
		cloneCol[!is.na(id)] <- as.character(cloneInfo[,2])[id[!is.na(id)]]
	}
	if (is.null(distMethod)) {
		clustSamples=NULL
		clustClones=NULL
	} else {
		if (clustCol) {
			clustSamples=hclust(getDist(t(dat),method=distMethod),method="ward")
		} else {
			clustSamples=NULL
		}
		if (clustRow) {
			if (candidateClone) {
				clustClones=NULL
			} else {
				if (!is.null(cloneInfo)) {
					id <- as.character(clInfo$Clone)[ind.cl][rev(1:nrow(clInfo[ind.cl,]))]%in%as.character(clInfo$Clone)
					dat <- dat[id,]
					cloneName <- paste(clInfo$Clone," chr",clInfo$Chr," kb",clInfo$kb,sep="")[ind.cl][rev(1:nrow(clInfo[ind.cl,]))][id]
					tmp <- as.character(clInfo$Clone)[ind.cl][rev(1:nrow(clInfo[ind.cl,]))][id]
					id <- match(tmp,as.character(cloneInfo[,1]))
					cloneCol <- rep("white",length(cloneName))
					cloneCol[!is.na(id)] <- as.character(cloneInfo[,2])[id[!is.na(id)]]
				}
				clustClones=hclust(getDist(dat,method=distMethod),method="ward")
			}
		} else {
			clustClones=NULL
		}
	}
	if (is.null(clustSamples)) {
		clustSamples2=NA
	} else {
		clustSamples2=as.dendrogram(clustSamples)
	}
	if (is.null(clustClones)) {
		clustClones2=NA
		cloneCol=chromcols[ind.cl][rev(1:length(chromcols[ind.cl]))]
		cloneName=NA
	} else {
		clustClones2=as.dendrogram(clustClones)
	}

	if (is.null(amplif)) {
		datAmp=NULL; colAmp=NULL
	} else {
		datAmp <- amplif[ind.cl,samId][rev(1:nrow(clInfo[ind.cl,])),]
		colAmp="yellow"
		if (candidateClone) {
			datAmp <- datAmp[(as.character(clInfo$Clone)[ind.cl][rev(1:nrow(clInfo[ind.cl,]))]%in%as.character(clInfo$Clone)),]
		}
	}
	if (!is.null(distMethod)) {
		if (candidateClone) {
			hcc <- heatmap3(x=dat, Rowv=NA, Colv=clustSamples2, distfun=distfun, hclustfun=hclust, symm=F, ColSideColors=sampCols, RowSideColors=cloneCol, labCol=samName, labRow=cloneName, scale="none", na.rm=F, margins=margins, main=main, xlab=NULL, ylab=NULL, zlm=limit, addamps=datAmp, colamps=colAmp)
		} else {
			if (is.null(cexCol)) {
				hcc <- heatmap3(x=dat, Rowv=clustClones2, Colv=clustSamples2, distfun=distfun, hclustfun=hclust, symm=F, ColSideColors=sampCols, RowSideColors=cloneCol, labCol=samName, labRow=cloneName, scale="none", na.rm=F, margins=margins, main=main, xlab=NULL, ylab=NULL, zlm=limit, addamps=datAmp, colamps=colAmp)
			} else {
				hcc <- heatmap3(x=dat, Rowv=clustClones2, Colv=clustSamples2, distfun=distfun, hclustfun=hclust, symm=F, ColSideColors=sampCols, RowSideColors=cloneCol, labCol=samName, labRow=cloneName, scale="none", na.rm=F, margins=margins, main=main, xlab=NULL, ylab=NULL, zlm=limit, addamps=datAmp, colamps=colAmp, cexCol=cexCol, totalC=ncol(dataAcgh))
			}
		}
	} else {
		if (candidateClone) {
			hcc <- heatmap3(x=dat, Rowv=NA, Colv=NA, distfun=distfun, hclustfun=hclust, symm=F, ColSideColors=sampCols, RowSideColors=cloneCol, labCol=samName, labRow=cloneName, scale="none", na.rm=F, margins=margins, main=main, xlab=NULL, ylab=NULL, zlm=limit, addamps=datAmp, colamps=colAmp)
		} else {
			if (is.null(cexCol)) {
				hcc <- heatmap3(x=dat, Rowv=NA, Colv=NA, distfun=distfun, hclustfun=hclust, symm=F, ColSideColors=sampCols, RowSideColors=chromcols[ind.cl][rev(1:length(chromcols[ind.cl]))], labCol=samName, labRow=NA, scale="none", na.rm=F, margins=margins, main=main, xlab=NULL, ylab=NULL, zlm=limit, addamps=datAmp, colamps=colAmp)
			} else {
				hcc <- heatmap3(x=dat, Rowv=NA, Colv=NA, distfun=distfun, hclustfun=hclust, symm=F, ColSideColors=sampCols, RowSideColors=chromcols[ind.cl][rev(1:length(chromcols[ind.cl]))], labCol=samName, labRow=NA, scale="none", na.rm=F, margins=margins, main=main, xlab=NULL, ylab=NULL, zlm=limit, addamps=datAmp, colamps=colAmp, cexCol=cexCol, totalC=ncol(dataAcgh))
			}
		}
	}

	invisible(list(clustSamples,clustClones))
}

heatmapColorBar <- function(limit,cols=c("green","red","black"),main=NULL) {
    if (length(cols)==3) {
        try <- maPalette(high=cols[1], low=cols[2], mid=cols[3])
    } else {
        ## 5. Ritu
        try <- maPalette(high=cols[1], low=cols[2])
    }
	maColorBar(try, scale=limit,main=main)
}

## 6. Ritu
### 1. Ritu
#sampleColorLegend <- function(tls,col=NULL,lty=NULL,legendTitle=NULL,cex=NULL) {
sampleColorLegend <- function(tls,col=NULL,lty=NULL,legendTitle=NULL,cex=NULL,density=NULL) {
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

## 2. Ritu
#getDist <- function(dat,method="pearson") {
getDist <- function(dat,method="pearson",absolute=FALSE) {
	if (method=="pearson") {
		print(method)
		## 2. Ritu
		#as.dist(1 - cor(t(dat),use="complete.obs"))
		y=cor(t(dat),use="complete.obs")
		if (absolute) y=abs(y)
		as.dist(y)
	} else {
		dist(dat, method=method)
	}
}

#library(psy)
## 2. Ritu
getKappaDist=function(x,type="Cohen",absolute=FALSE) {
	res=matrix(nrow=ncol(x),ncol=ncol(x),dimnames=list(colnames(x),colnames(x)))
	for (k1 in 1:ncol(x)) {
		for (k2 in 1:ncol(x)) {
			res[k1,k2]=res[k2,k1]=lkappa(x[,c(k1,k2)], type=type)
		}
	}
	if (absolute) res=abs(res)
	as.dist(1-res)
}

## 3. Ritu
## Used for dichotomous data. Set the categories as -1 & 1
getCosineDist=function(x) {
    cos.sim <- function(ix) {
        A = x[ix[1],]
        B = x[ix[2],]
        return( sum(A*B)/sqrt(sum(A^2)*sum(B^2)) )
    }
    res=matrix(nrow=ncol(x),ncol=ncol(x),dimnames=list(colnames(x),colnames(x)))
    n <- nrow(x)
    cmb <- expand.grid(i=1:n, j=1:n)
    ## 4. Ritu
    #res <- matrix(apply(cmb,1,cos.sim),n,n)
    res <- matrix(apply(cmb,1,cos.sim),n,n,dimnames=list(rownames(x),rownames(x)))
    as.dist(1-res)
}

