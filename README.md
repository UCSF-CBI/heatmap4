# heatmap4
This package takes the original heatmap function and reduces the argument complexity.
A heat map is a false color image (basically image(t(x))) with a dendrogram added to the left side and to the top. Typically, reordering of the rows and columns according to some set of values (row or column means) within the restrictions imposed by the dendrogram is carried out.
# installing the heatmap4 package
This section covers installing the heatmap4 package from this UCSF-CBI repository. Installing this package will give you access to examples and help files for running the function “generate_heatmap” that you can use in R.

Please ensure that you have an access token to this heatmap4 repository before attempting to install the package (contact a owner/contributor to send you a personal access token). 

## install devtools package (this can be done from CRAN)
```{r}
install.packages("devtools")
```

## load devtools package 
```{r}
library("devtools")
```
Note: R should show that it is loading the required package "usethis"


## use install_github with “author/package”
```{r}
 install_github(“UCSF-CBI/heatmap4”,  auth_token = “xyz,” build_vignettes = TRUE)
```
Note: make sure to include the “build_vignettes” argument, otherwise the package vignettes will not download. Also know that the auth_token given to you may be subject to change.

After downloading the package, you should have access to a heatmap4 help file as well as example the files that are in this repository. 

# usage
After inputting your data set and assigning variables and values as needed, you can call the generate_heatmap function to build a heatmap based on our data set 

```{r}
library("heatmap4")
generate_heatmap(x, col_info = annotation_df, row_anno = FALSE, col_anno = TRUE, row_lab = TRUE, col_lab = TRUE,
                 row_dend = FALSE, col_dend = TRUE, file_name = "example")

```

# license
UCSF


