evoplot
-------

evoplot is an R package that draws clonal evolution plots based on clonal size counts or frequencies

--------------------------------------------------------------------------------

### News

-   Version `0.1` released


### Installing evoplot
Install the development version directly from github (requires devtools)


```r
require(devtools)
devtools::install_github("sp00nman/evoplot") 
```

To install a local copy:

```r
evoplot_package <- "~/R/evoplot";
install.packages(evoplot_packages, repos=NULL)
```

--------------------------------------------------------------------------------
### Usage

```r
library(evoplot)
?evoplot
```

``` r
# basic example
df <- data.frame(clone=c(1,1,2),
                 subclone=c("A","B","C"),
                 colony_count=c(46,43,1),
                 gene=c("GENE.A,GENE.B,GENE.C",
                     "GENE.A,GENE.B,GENE.C,GENE.D",
                     "GENE.E")) 
```

``` r
df_freq <- calculate.frequencies(df)
```

