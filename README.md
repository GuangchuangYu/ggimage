# ggimage: Use Image in 'ggplot2'


[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/ggimage?color=green)](https://cran.r-project.org/package=ggimage)
![](http://cranlogs.r-pkg.org/badges/grand-total/ggimage?color=green)
![](http://cranlogs.r-pkg.org/badges/ggimage?color=green)
![](http://cranlogs.r-pkg.org/badges/last-week/ggimage?color=green)
[![gitter](https://img.shields.io/badge/GITTER-join%20chat-green.svg)](https://gitter.im/GuangchuangYu/Bioinformatics)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.344625.svg)](https://doi.org/10.5281/zenodo.344625)



Supports aesthetic mapping of image files to be visualized in 'ggplot2' graphic system.


## Authors

Guangchuang YU <https://guangchuangyu.github.io>

School of Public Health, The University of Hong Kong

## Installation

Get the released version from CRAN:

```r
## include BioC repo for dependencies
setRepositories(ind=1:2)
install.packages("ggimage")
```

Or the development version from github:

```r
setRepositories(ind=1:2)
## install.packages("devtools")
devtools::install_github("GuangchuangYu/ggimage")
```

## Related Packages

| package                                                  |purpose        |base plot| ggplot2| geom layer<sup>&</sup>| aes mapping<sup>^</sup>|
| ---------------------------------------------------------| ------- | ------ |---------- |--------------- | ----------------- |
| [CatterPlots](https://github.com/Gibbsdavidl/CatterPlots)| cats                | Y           |             |                    |                |
| [rphylopic](https://github.com/sckott/rphylopic)         | phylopic           | Y           | Y          | Y & N<sup>*</sup>   |                 |
| [emoGG](https://github.com/dill/emoGG)                   | emoji                |             | Y          | Y                 |                 |
| [ggflags](https://github.com/baptiste/ggflags)           | flags                         |             | Y           | Y          | Y          |
| [ggimage](https://github.com/GuangchuangYu/ggimage)      |**general**, all kinds of images|    | Y           | Y                 | Y             |


<sup>\*</sup> `rphylopic` supports using `p+add_phylopic()` to add **1** image each time since it internally use `annotation_custom`.

<sup>&</sup> a geom layer supports adding multiple (copy/different) images simultaneously.

<sup>^</sup> without `aes` mapping, a layer can only add multiple copy of **an** image at different positions. While with `aes`, one can map a categorical variable to **different** images and adding them to a layer.

## Examples

**ggimage** provides general solution for using images in *ggplot2*. It's easy to reproduce examples of other packages that designed for specific need.

### CatterPlots

```r
library(ggplot2)
library(ggimage)

mytheme <- theme_minimal() +
    theme(axis.title=element_blank())
theme_set(mytheme)

x <- seq(-2*pi, 2*pi, length.out=30)
d <- data.frame(x=x, y=sin(x))

img <- "http://www.belleamibengals.com/bengal_cat_2.png"
ggplot(d, aes(x, y)) + geom_image(image=img, size=.1)
```

<img src="https://guangchuangyu.github.io/blog_images/R/ggimage/ggimage_CatterPlots.png", width="400">


### rphylopic

```r
img <- "http://phylopic.org/assets/images/submissions/500bd7c6-71c1-4b86-8e54-55f72ad1beca.128.png"
ggplot(d, aes(x, y)) + geom_image(image=img, size=.1)
```

<img src="https://guangchuangyu.github.io/blog_images/R/ggimage/ggimage_rphylopic.png", width="400">

### emoGG

```r
emoji <- "https://twemoji.maxcdn.com/72x72/1f63b.png"
ggplot(d, aes(x, y)) + geom_image(image=emoji)
```

<img src="https://guangchuangyu.github.io/blog_images/R/ggimage/ggimage_emoGG.png", width="400">

### ggflags

```r
cn <- "https://behdad.github.io/region-flags/png/CN.png"
fr <- "https://behdad.github.io/region-flags/png/FR.png"
us <- "https://behdad.github.io/region-flags/png/US.png"

set.seed(123)
d$image <- sample(c(cn, fr, us), size=nrow(d), replace=TRUE)
ggplot(d, aes(x, y)) + geom_image(aes(image=image))
```

<img src="https://guangchuangyu.github.io/blog_images/R/ggimage/ggimage_ggflags.png", width="400">

## TODO

+ legend of images (`draw_key_image` function)
+ move `ggtree::subview` to `ggimage`.
+ reimplement `ggtree::phylopic` function to `geom_phylopic` layer.
