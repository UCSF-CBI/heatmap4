# heatmap4
A heatmap is a false color image (image(t(x))) with a dendrogram added to the top and left side. Typically, reordering of the rows and columns according to some set of values (row or column means) within the restrictions imposed by the dendrogram is carried out.

This package takes the original heatmap function and reduces the argument complexity.

# Installation
'heatmap4' is available in R-universe. This is how it can be installed.
```{r}
options(repos = c(
    rituroy = 'https://rituroy.r-universe.dev',
    CRAN = 'https://cloud.r-project.org'))

install.packages('heatmap4', repos = 'https://github.com/rituroy/heatmap4')
```

# Usage
See vignette on how to use 'heatmap4'

```{r}
library('heatmap4')
vignette('heatmap4')
```


