
library(rmarkdown)
library(knitr)
library(xfun)

input <- "/Users/emmafoessing/Documents/Master/MA/Code/Master-Thesis/SDG_RF_v2.ipynb"
convert_ipynb(input, output = xfun::with_ext(input, "Rmd"))
