[![Get started with zread](https://img.shields.io/badge/Get%20started%20with%20zread-green)](https://zread.ai/jaspershen-lab/lagcishiny) [![Ask DeepWiki](https://deepwiki.com/badge.svg)](https://deepwiki.com/jaspershen-lab/lagcishiny) [![](https://www.r-pkg.org/badges/version/lagcishiny?color=green)](https://cran.r-project.org/package=lagcishiny)

---
`lagcishiny` is a shiny web application for [`lagci`](https://github.com/jaspershen-lab/lagci) package.

## **Installation**
---

You can install `lagcishiny` from [GitHub](https://github.com/jaspershen-lab/lagcishiny).

``` R
if(!require(devtools)){
install.packages("devtools")
}
devtools::install_github("jaspershen-lab/lagcishiny")
```

## **Usage**

------------------------------------------------------------------------

You can use `lagcishiny` by

``` R
library(lagcishiny)
```

``` R
lagcishiny::lagci_shiny()
```

It will start a web app in your device.


## **Simple tutorial**

---
The `lagcishiny` web app only contain 5 pages, you can browse and get BibTex
 format citation text for `Home` page, and start calculation along with `Data Upload`
 `Calculate` `Result Plots` `Results & Report` pages.
 
We provide a lot of parameters to adjust, so that you can analyse you data in visual 
approach. If you want to save the plots, you can export them with customizable size and format (`png`, 
`jpg`, `jpeg`, `tiff`, `svg`, `eps`, `bmp`, `pdf`). Also, we support export the report in three 
formats (`pdf`, `Powerpoint`, `HTML`).