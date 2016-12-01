#' # Publication Bias
#' 
#' ## As estimated wth fail-safe numbers
#' 
#' ### Preamble
#' 
#' Load Libraries
#+ libraries, messages=FALSE, warning=FALSE
library(ezknitr)
library(knitr)
library(metafor)

#' Clear environment
#+ clear
remove(list=ls())

#' Document settings
#+ settings
opts_chunk$set(fig.width = 6, fig.height = 4)

#' ### Load data
#+ loadData
load(file="data/output_data/data_cleaned.R")


#' ### Fail-safe numbers
#' 
#' Rust
#+ fsnRust
fsn.rust <- fsn(yi = yi, vi = vi, data = rust.data.SMD)
fsn.rust
# compared to actual number of data points
nrow(rust.data.SMD)

#' Yield
#+ fsnYield
fsn.yield <- fsn(yi = yi, vi = vi, data = yield.data.SMD)
fsn.yield
# compared to actual number of data points
nrow(yield.data.SMD)

#' 100-seed-weight
#+ fsn100
fsn.100sw <- fsn(yi = yi, vi = vi, data = seedwt.data.SMD)
fsn.100sw
# compared to actual number of data points
nrow(seedwt.data.SMD)

#' Cercospora
#+ fsnCerco
fsn.cerco <- fsn(yi = yi, vi = vi, data = cerco.data.MD)
fsn.cerco
# compared to actual number of data points
nrow(cerco.data.MD)

#' Target Spot
#+ fsnTarget
fsn.target.spot <- fsn(yi = yi, vi = vi, data = target.spot.data.MD)
fsn.target.spot
# compared to actual number of data points
nrow(target.spot.data.MD)

#' ### Footer
#' 
#' Spun with ezspin("programs/publication_bias.R", out_dir="output", fig_dir="figures", keep_md=FALSE)
#' 
#' Session Info:
#+ sessionInfo
sessionInfo()