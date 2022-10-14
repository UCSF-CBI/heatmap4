#' Gene expression data
#'
#' A numeric matrix with gene expression data with rows representing genes and columns representing subjects.
#'   ...
#'
#' @format ## `genomDat`
#' A matrix with 3469 rows and 39 columns.
"genomDat"


#' Phenotypic (patient information) data
#'
#' A data.frame containing subject information.
#'   ...
#'
#' @format ## `phen`
#' A data.frame with 39 rows and 7 columns.
"phen"


#' Gene information
#'
#' A data.frame containing gene information.
#'   ...
#'
#' @format ## `anno`
#' A data.frame with 3469 rows and 2 columns.
"anno"


#' Gene amplification status
#'
#' A numeric matrix of 0s and 1s of the same dimension as genomDat.
#'   ...
#'
#' @format ## `amplifDat`
#' A matrix with 3469 rows and 39 columns.
"amplifDat"


#' Chromosomal information
#'
#' A data.frame containing information for each chromosome.
#'   ...
#'
#' @format ## `chrInfo`
#' A data.frame with 24 rows and 3 columns.
"chrInfo"
