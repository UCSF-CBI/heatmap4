setwd("~/UCSF/functions/scripts/heatmap")

## ----------------------------------------------
######################################################################
######################################################################
## Script to run heatmap function
######################################################################
######################################################################

load("data.RData")

i=which(anno$Chrom%in%1:2)
i=1:20
j=1:10
genomDat=genomDat[i,j]
anno=anno[i,]
phen=phen[j,]
amplifDat=amplifDat[i,j]

set.seed(4534)
countDat=matrix(as.character(sample(1:10,nrow(genomDat)*ncol(genomDat),replace=T)),nrow=nrow(genomDat),ncol=ncol(genomDat))
countDat[2,1:4]=NA

library(RColorBrewer)
library(marray)
#library(sma)
#library(aCGH)

source("heatmap4.R")
source("heatmapRelated.R")

#row colors (chromosomes)

cols=c("red","blue","grey")
limit=c(-.5,.5)

########################
## Side color bars
## Can be matrix or vector
## If matrix, then columns represent the observations,rows the variables

## ---------------------
## Chromosome color bars

chrCol <- rep(NA, nrow(anno))

for (chr in 1:23)
{
	if (chr%%2!=0) {
		#chrCol[which(anno$Chrom==chr & anno$kb >= chrInfo$centromere[chr])] <- "yellowgreen"
		#chrCol[which(anno$Chrom==chr & anno$kb < chrInfo$centromere[chr])] <- "green"
		chrCol[which(anno$Chrom==chr)] <- "gray10"
		
	} else	{
		#chrCol[which(anno$Chrom==chr & anno$kb >= chrInfo$centromere[chr])] <- "skyblue"
		#chrCol[which(anno$Chrom==chr & anno$kb < chrInfo$centromere[chr])] <- "blue"
		chrCol[which(anno$Chrom==chr)] <- "gray90"
		
	}
}

## ---------------------
## Sample color bars
## Can be matrix or vector

phenName=c("node","sex")
samColUniq=c("skyblue","blue")
samCol=matrix(nrow=2,ncol=nrow(phen))
rownames(samCol)=phenName
for (phenId in 1:length(phenName)) {
	resp=phen[,phenName[phenId]]
	grpUniq=sort(unique(resp))
	for (k in 1:length(grpUniq)) {samCol[phenId,resp==grpUniq[k]]=samColUniq[k]}
}

#########################

## Reverse the order of probes
probeId=nrow(anno):1



pdf("heatmap.pdf", paper="letter")

## If Rowv=NA, ordering of rows (probes) will remain as is
## If Rowv=NULL, rows (probes) will be reordered by applying heirarchical clustering
## Similarly for Colv, which represents samples
try10=heatmap4(x=genomDat[probeId,], Rowv=NA, Colv=NULL, distfun = dist, hclustfun = hclust,  symm=F, ColSideColors = samCol, RowSideColors=chrCol[probeId], labCol=colnames(genomDat), labRow=NA, scale="none", na.rm = F, margins = c(5, 5), main = NULL, xlab = NULL, ylab = NULL, zlm=limit, high=cols[1], low=cols[2], mid=cols[3], addamps=amplifDat[probeId,], colamps="yellow",addText=countDat)
#try10=heatmap4(x=genomDat[probeId,], Rowv=NA, Colv=NULL, distfun = dist, hclustfun = hclust,  methodR = "ward.D2", methodC = "ward.D2", symm=F, ColSideColors = samCol, RowSideColors=chrCol[probeId], labCol=colnames(genomDat), labRow=NA, scale="none", na.rm = F, margins = c(5, 5), main = NULL, xlab = NULL, ylab = NULL, zlm=limit, high=cols[1], low=cols[2], mid=cols[3], addamps=amplifDat[probeId,], colamps="yellow",addText=countDat)
#try10=heatmap4(x=genomDat[probeId,], Rowv=NA, Colv=NULL, distfun = dist, hclustfun = hclust,  methodR = "ward.D", methodC = "ward.D", symm=F, ColSideColors = samCol, RowSideColors=chrCol[probeId], labCol=colnames(genomDat), labRow=NA, scale="none", na.rm = F, margins = c(5, 5), main = NULL, xlab = NULL, ylab = NULL, zlm=limit, high=cols[1], low=cols[2], mid=cols[3], addamps=amplifDat[probeId,], colamps="yellow",addText=countDat)

plot(try10$colClust); rect.hclust(try10$colClust,k=2)

heatmapColorBar(limit=limit,cols=cols)

sampleColorLegend(tls=c("N0","N+"),col=samColUniq[1:2],lty=NULL,legendTitle="Node",cex=NULL)
sampleColorLegend(tls=c("F","M"),col=samColUniq[1:2],lty=NULL,legendTitle="Sex",cex=NULL)

dev.off()

