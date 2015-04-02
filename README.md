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
# create dataframe with colony counts for each subclone
df <- data.frame(clone=c(1,1,2),
                 subclone=c("A","B","C"),
                 colony_count=c(25,15,10),
                 gene=c("GENE.A,GENE.B,GENE.C",
                     "GENE.A,GENE.B,GENE.C,GENE.D",
                     "GENE.E")) 
```

``` r
# if necessary, calculate frequencies
df$subcone_freq <- calculate.frequencies(df)
```

``` r
# calculate coordinates to plot polygons
d_plot <- get_coordinates(df, yaxis_length=4, debug=TRUE)
```

``` r
#draw plot
g_plot <- draw.clonal.evolution.plot(d_plot)
print(g_plot)
```

``` r
# save plot to pdf
pdf(paste(path2file, "evoplot.pdf", sep ="/"), 
    width=5, height=5, useDingbats=FALSE)
print(g_plot)
dev.off()
```

![overlapping](https://github.com/sp00nman/evoplot/blob/master/pics/evoplot.png?raw=true 50x100)


