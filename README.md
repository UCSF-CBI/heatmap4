# heatmap4
This package takes the original heatmap function and reduces the argument complexity.
A heat map is a false color image (basically image(t(x))) with a dendrogram added to the left side and to the top. Typically, reordering of the rows and columns according to some set of values (row or column means) within the restrictions imposed by the dendrogram is carried out.
# installing the heatmap4 package
This section covers installing the heatmap4 package from the UCSF-CBI repository that was shown in the examples above. The heatmap4 package takes the original heatmap function and reduces the complexity of its argument. Installing this package will give you access to examples and help files for running the function “generate_heatmap” that you can use in R.

Please ensure that you have an access token to this heatmap4 repository so you can install the package. 

## install devtools package [this can be done from CRAN]
```{r}
install.packages("devtools")
```

## load devtools package 
```{r}
library("devtools")
Loading required package: usethis
```

## use install_github with “author/package”
```{r}
 install_github(“UCSF-CBI/heatmap4”,  auth_token = “xyz,” build_vignettes = TRUE)
```
Note: make sure to include the “build_vignettes” argument, otherwise the package vignettes will not download. Also know that the auth_token given to you may be subject to change.

After downloading the package, you should have access to a heatmap4 help file as well as example the files that are in this repository. 

# license
UCSF


