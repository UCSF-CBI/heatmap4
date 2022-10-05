## 1. Ritu 01/11/09 Fixed logic for "lty" option & removed param box.lty
## 2. Ritu 02/21/14 New function getKappaDist
##					New param absolute for getDist
## 3. Ritu 03/08/16 New function getCosineDist
## 4. Ritu 04/14/17 Set row & column names in getCosineDist
## 5. Ritu 05/29/17 Set row & column names in getCosineDist
## 6. Ritu 01/27/18 New option density in function sampleColorLegend
## 7. Ritu 08/25/21 No border for color fill in function sampleColorLegend
## 8. Ritu 10/12/21 Reverse cluster designation if rev=T
## 9. Ritu 10/13/21 New options pch, lwd in function sampleColorLegend
## 10. Ritu 10/17/22 New options border, ... in function sampleColorLegend

heatmapColorBar <- function(limit,cols=c("green","red","black"),main=NULL,...) {
    if (length(cols)==3) {
        try <- marray::maPalette(high=cols[1], low=cols[2], mid=cols[3])
    } else {
        ## 5. Ritu
        try <- marray::maPalette(high=cols[1], low=cols[2])
    }
    maColorBar(try, scale=limit,main=main,...)
}

## 10. Ritu
### 9. Ritu
#### 6. Ritu
##### 1. Ritu
###sampleColorLegend <- function(tls,col=NULL,lty=NULL,legendTitle=NULL,cex=NULL) {
##sampleColorLegend <- function(tls,col=NULL,lty=NULL,legendTitle=NULL,cex=NULL,density=NULL) {
#sampleColorLegend <- function(tls,col=NULL,lty=NULL,pch=NULL,lwd=NULL,legendTitle=NULL,cex=NULL,density=NULL) {
sampleColorLegend <- function(tls,col=NULL,lty=NULL,border=NULL,pch=NULL,lwd=NULL,legendTitle=NULL,cex=NULL,density=NULL,...) {
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
    ## 10. Ritu
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
            ## 10. Ritu
            ### 7. Ritu
            ##legend(0,length(tls),tls,fill=fill,col=col,lty=lty,cex=cex,title=legendTitle)
            #legend(0,length(tls),tls,fill=fill,col=col,lty=lty,border=fill,cex=cex,title=legendTitle)
            legend(0,length(tls),tls,fill=fill,col=col,lty=lty,border=border,cex=cex,title=legendTitle,...)
        } else {
            ## 10. Ritu
            ### 7. Ritu
            #### 6. Ritu
            ##legend(0,length(tls),tls,col=fill,lty=lty,cex=cex,density=density,title=legendTitle)
            #legend(0,length(tls),tls,col=fill,lty=lty,border=fill,cex=cex,density=density,title=legendTitle)
            legend(0,length(tls),tls,col=fill,lty=lty,border=border,cex=cex,density=density,title=legendTitle,...)
            if (F) {
                text(1,k-1,legendTitle)
                for (k in 1:length(tls)) {
                    rect(0,k-0.5,1,k+0.5,col=fill[k],density=density)
                    text(2,k,tls[k])
                }
            }
        }
	} else {
        ## 10. Ritu
        #legend(0,length(tls),tls,col=col,lty=lty,pch=pch,lwd=lwd,cex=cex,title=legendTitle)
        legend(0,length(tls),tls,col=col,lty=lty,pch=pch,lwd=lwd,cex=cex,title=legendTitle,...)
	}
}

## 2. Ritu
#getDist <- function(dat,method="pearson") {
getDist <- function(dat,method="pearson",absolute=FALSE) {
	if (method=="pearson") {
		#print(method)
		## 2. Ritu
		#as.dist(1 - cor(t(dat),use="complete.obs"))
		y=cor(t(dat),use="complete.obs")
		if (absolute) y=abs(y)
		as.dist(1-y)
	} else {
		dist(dat, method=method)
	}
}

## 2. Ritu
getKappaDist=function(x,type="Cohen",absolute=FALSE) {
	res=matrix(nrow=ncol(x),ncol=ncol(x),dimnames=list(colnames(x),colnames(x)))
	for (k1 in 1:ncol(x)) {
		for (k2 in 1:ncol(x)) {
			res[k1,k2]=res[k2,k1]=psy::lkappa(x[,c(k1,k2)], type=type)
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

getCluster=function(dat,distMethod,linkMethod,absFlag=F) {
    switch(distMethod,
        "cosine"={
            if (any(apply(dat,2,sum,na.rm=T)==0)) dat=dat+1
            distMat=getCosineDist(t(dat))
            clustThis=hclust(distMat,method=linkMethod)},
        "pearson"={
            x=cor(dat,method=distMethod,use="complete.obs")
            if (absFlag) x=abs(x)
            distMat=as.dist(1-x)
            clustThis=hclust(distMat,method=linkMethod)},
        "spearman"={
            x=cor(dat,method=distMethod,use="complete.obs")
            if (absFlag) x=abs(x)
            distMat=as.dist(1-x)
            clustThis=hclust(distMat,method=linkMethod)},
        "kappa"={
            distMat=getKappaDist(dat,absolute=absFlag)
            clustThis=hclust(distMat,method=linkMethod)},
        "euclidean"={
            distMat=dist(t(dat),method=distMethod)
            clustThis=hclust(distMat,method=linkMethod)}
    )
    invisible(clustThis)
}

cutCluster=function(clustObj,ann,nClust=2,rev=F) {
    if (is.na(nClust)) {
        if (class(clustObj)=="hclust") {
            tbl=cbind(ann[clustObj$order,],order=1:nrow(ann))
        } else {
            tbl=cbind(ann,order=1:nrow(ann))
        }
    } else {
        tbl=matrix(nrow=nrow(ann),ncol=nClust-1)
        colnames(tbl)=paste("clustId_",1:ncol(tbl)+1,sep="")
        #for (kk in kVec) {
        for (kk in 1:ncol(tbl)) {
            clustId=cutree(clustObj,k=kk+1)[clustObj$order]
            k1=which(!duplicated(clustId))
            for (k in 1:length(k1)) {
                ## 8. Ritu
                #clustId[which(clustId==clustId[k1[k]])]=paste("cluster",k,sep="")
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
