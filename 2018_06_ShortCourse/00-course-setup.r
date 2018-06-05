### -----------------------------
### workshop setup
### Hui Lin @Netlify
### http://scientistcafe.com
### -----------------------------


# install packages from CRAN
p_needed <- c( "dplyr", "caret","e1071", "imputeMissings","nnet","car",
               "reshape2","psych","tidyr","ggplot2","sparklyr","readr","corrplot")

packages <- rownames(installed.packages())
p_to_install <- p_needed[!(p_needed %in% packages)]
if (length(p_to_install) > 0) {
  install.packages(p_to_install)
}

lapply(p_needed, require, character.only = TRUE)
