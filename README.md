# ggimage: Use Image in 'ggplot2'


[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/ggimage?color=green)](https://cran.r-project.org/package=ggimage)
![](http://cranlogs.r-pkg.org/badges/grand-total/ggimage?color=green)
![](http://cranlogs.r-pkg.org/badges/ggimage?color=green)
![](http://cranlogs.r-pkg.org/badges/last-week/ggimage?color=green)
[![gitter](https://img.shields.io/badge/GITTER-join%20chat-green.svg)](https://gitter.im/GuangchuangYu/Bioinformatics)


Supports aesthetic mapping of image files to be visualized in 'ggplot2' graphic system.


## Authors

Guangchuang YU <http://guangchuangyu.github.io>

School of Public Health, The University of Hong Kong

## Installation

Get the released version from CRAN:

```r
install.packages("ggimage")
```

Or the development version from github:

```r
## install.packages("devtools")
devtools::install_github("GuangchuangYu/ggimage")
```

## Related Packages

| package                                                                              | purpose                                        |base plot| ggplot2| geom layer^&^| aes mapping^^^|
| -------------------------------------------------------------------------| ------------------------------------------ | ---------- |---------- |--------------- | ----------------- |
| [CatterPlots](https://github.com/Gibbsdavidl/CatterPlots)| cats                                               | Y           |             |                    |                       |
| [rphylopic](https://github.com/sckott/rphylopic)               | phylopic                                        | Y           | Y          | Y & N^*^   |                       |
| [emoGG](https://github.com/dill/emoGG)                         | emoji                                             |             | Y          | Y                 |                       |
| [ggflags](https://github.com/baptiste/ggflags)                  | flags                                              |             | Y           | Y                 | Y                    |
| [ggimage](https://github.com/GuangchuangYu/ggimage)| **general**, all kinds of images|             | Y           | Y                 | Y                    |


^*^ `rphylopic` supports using `p+add_phylopic()` to add **1** image each time since it internally use `annotation_custom`.
^&^ a geom layer supports adding multiple (copy/different) images simultaneously
^^^ without `aes` mapping, a layer can only add multiple copy of **a** image at different positions. While with `aes`, one can map a categorical variable to different images and adding it to a layer.


## TODO

+ move `ggtree::subview` to `ggimage`.
+ reimplement `ggtree::phylopic` function to `geom_phylopic` layer.
