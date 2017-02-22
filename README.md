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

| package                                                  |purpose                       |base plot| ggplot2| geom layer<sup>&</sup>| aes mapping<sup>^</sup>|
| ---------------------------------------------------------| ------- | ------ |---------- |--------------- | ----------------- |
| [CatterPlots](https://github.com/Gibbsdavidl/CatterPlots)| cats                         | Y           |             |                    |                       |
| [rphylopic](https://github.com/sckott/rphylopic)         | phylopic                      | Y           | Y          | Y & N<sup>*</sup>   |                       |
| [emoGG](https://github.com/dill/emoGG)                   | emoji                         |             | Y          | Y                 |                       |
| [ggflags](https://github.com/baptiste/ggflags)           | flags                          |             | Y           | Y                 | Y          |
| [ggimage](https://github.com/GuangchuangYu/ggimage)      |**general**, all kinds of images|             | Y           | Y                 | Y                    |


<sup>\*</sup> `rphylopic` supports using `p+add_phylopic()` to add **1** image each time since it internally use `annotation_custom`.

<sup>&</sup> a geom layer supports adding multiple (copy/different) images simultaneously.

<sup>^</sup> without `aes` mapping, a layer can only add multiple copy of **a** image at different positions. While with `aes`, one can map a categorical variable to different images and adding them to a layer.


## TODO

+ legend of images (`draw_key_image` function)
+ move `ggtree::subview` to `ggimage`.
+ reimplement `ggtree::phylopic` function to `geom_phylopic` layer.
