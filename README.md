[![Get started with zread](https://img.shields.io/badge/Get%20started%20with%20zread-green)](https://zread.ai/chain-buds/laggedcorAPP) [![Ask DeepWiki](https://deepwiki.com/badge.svg)](https://deepwiki.com/chain-buds/laggedcorAPP) [![](https://www.r-pkg.org/badges/version/laggedcorAPP?color=green)](https://cran.r-project.org/package=laggedcorAPP)

---
`laggedcorAPP` is a shiny web application for [`laggedcor`](https://github.com/jaspershen-lab/laggedcor) package.

## **Installation**
---

You can install `laggedcorAPP` from [GitHub](https://github.com/chain-buds/laggedcorAPP).

``` R
if(!require(devtools)){
install.packages("devtools")
}
devtools::install_github("chain-buds/laggedcorAPP")
```

## **Usage**

------------------------------------------------------------------------

You can use `laggedcorAPP` by

``` R
library(laggedcorAPP)
```

``` R
laggedcorAPP::laggedcor_shiny()
```

It will start a web app in your device.


## **Simple tutorial**

---
The `laggedcorAPP` web app only contain 5 pages, you can browse and get BibTex
 format citation text for `Home` page, and start calculation along with `Data Upload`
 `Calculate` `Result Plots` `Results & Report` pages.
 
We provide a lot of parameters to adjust, so that you can analyse you data in visual 
approach. If you want to save the plots, you can export them with customizable size and format (`png`, 
`jpg`, `jpeg`, `tiff`, `svg`, `eps`, `bmp`, `pdf`). Also, we support export the report in three 
formats (`pdf`, `Powerpoint`, `HTML`).