setwd("/Users/rohanbhanot/Desktop/UCSF/Repositories") # wd and github repository

## ----------------------------------------------
######################################################################
######################################################################
## Script to run heatmap function
######################################################################
######################################################################

## Downloads 'marray' package from Bioconductor
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("marray")
load("data.RData")

i <- 1:20
j <- 1:10

## Simplifying Data Matrices
genomDat <- genomDat[i, j]
anno <- anno[i, ]
phen <- phen[j, ]
amplifDat <- amplifDat[i, j]

## Random sample matrix that is the same size as genomDat
countDat <- matrix(as.character(sample(1:10, nrow(genomDat) * ncol(genomDat), replace = T)),
                   nrow = nrow(genomDat), ncol = ncol(genomDat))

countDat[2, 1:4] <- NA

## Calls Libraries
library(RColorBrewer)
library(marray)
#library(sma)
#library(aCGH)

## Calls Files
source("heatmap4.R")
source("heatmapRelated.R")

## Row colors (chromosomes)
cols <- c("red", "blue", "grey")
limit <- c(-.5, .5)

########################
## Side color bars
## Can be matrix or vector
## If matrix, then columns represent the observations,rows the variables

## ---------------------
## Chromosome color bars


### May need to be repalced
chrCol <- rep(NA, nrow(anno)) # replicates no vector (NA), nrow(anno) times (20)

for (chr in 1:23)
{
	if (chr %% 2 != 0) {
		#chrCol[which(anno$Chrom==chr & anno$kb >= chrInfo$centromere[chr])] <- "yellowgreen"
		#chrCol[which(anno$Chrom==chr & anno$kb < chrInfo$centromere[chr])] <- "green"
		chrCol[which(anno$Chrom == chr)] <- "gray10"

	} else	{
		#chrCol[which(anno$Chrom==chr & anno$kb >= chrInfo$centromere[chr])] <- "skyblue"
		#chrCol[which(anno$Chrom==chr & anno$kb < chrInfo$centromere[chr])] <- "blue"
		chrCol[which(anno$Chrom == chr)] <- "gray90"

	}
} # all odd numbers are gray10 and even numbers are gray90

## ---------------------
## Sample color bars
## Can be matrix or vector

## Setting colors to annotations
phenName <- c("node", "sex") # 2 defined annotations
samColUniq <- c("skyblue", "blue") # 2 color names for the 2 annotations
samCol <- matrix(nrow = 2, ncol = nrow(phen)) # matrix created assigning a color to an annotation
rownames(samCol) <- phenName # defines the rownames of samCol matrix

for (phenId in 1:length(phenName)) {
	resp <- phen[ ,phenName[phenId]]
	grpUniq <- sort(unique(resp))
	for (k in 1:length(grpUniq)) {samCol[phenId, resp == grpUniq[k]] = samColUniq[k]}
} # need help understanding this for loop

#########################

## Reverse the order of probes
probeId = nrow(anno):1

## PDF creation
pdf("heatmap.pdf", paper = "letter") # creates letter paper size pdf document with "heatmap.pdf" title

## If Rowv=NA, ordering of rows (probes) will remain as is
## If Rowv=NULL, rows (probes) will be reordered by applying heirarchical clustering
## Similarly for Colv, which represents samples
try10 <- heatmap4(x = genomDat[probeId, ], Rowv = NA, Colv = NULL, distfun = dist, hclustfun = hclust,  symm = FALSE,
                 ColSideColors = samCol, RowSideColors = chrCol[probeId], labCol = colnames(genomDat), labRow = NA,
                 scale = "none", na.rm = FALSE, margins = c(5, 5), main = NULL, xlab = NULL, ylab = NULL, zlm = limit,
                 high = cols[1], low = cols[2], mid = cols[3], addamps = amplifDat[probeId,], colamps = "yellow",
                 addText = countDat)

#try10=heatmap4(x=genomDat[probeId,], Rowv=NA, Colv=NULL, distfun = dist, hclustfun = hclust,  methodR = "ward.D2", methodC = "ward.D2", symm=F, ColSideColors = samCol, RowSideColors=chrCol[probeId], labCol=colnames(genomDat), labRow=NA, scale="none", na.rm = F, margins = c(5, 5), main = NULL, xlab = NULL, ylab = NULL, zlm=limit, high=cols[1], low=cols[2], mid=cols[3], addamps=amplifDat[probeId,], colamps="yellow",addText=countDat)
#try10=heatmap4(x=genomDat[probeId,], Rowv=NA, Colv=NULL, distfun = dist, hclustfun = hclust,  methodR = "ward.D", methodC = "ward.D", symm=F, ColSideColors = samCol, RowSideColors=chrCol[probeId], labCol=colnames(genomDat), labRow=NA, scale="none", na.rm = F, margins = c(5, 5), main = NULL, xlab = NULL, ylab = NULL, zlm=limit, high=cols[1], low=cols[2], mid=cols[3], addamps=amplifDat[probeId,], colamps="yellow",addText=countDat)

## Creates Dendrogram
plot(try10$colClust); rect.hclust(try10$colClust, k = 2)

## Creates Color Legend Spectrum
heatmapColorBar(limit = limit, cols = cols)

## Creates Legends for Annotations
sampleColorLegend(tls = c("N0", "N+"), col = samColUniq[1:2], lty = NULL, legendTitle = "Node", cex = NULL)
sampleColorLegend(tls = c("F", "M"), col = samColUniq[1:2], lty = NULL, legendTitle = "Sex", cex = NULL)

#dev.off()


