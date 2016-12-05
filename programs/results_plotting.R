#' # Results processing for plots
#' 
#' Also, analysis for the numbers reported in the paper (e.g., number of studies
#' and entries/study)
#' 
#' ### Preamble
#' 
#' Load Libraries
#+ libraries, messages=FALSE, warning=FALSE
library(ezknitr)
library(knitr)
library(metafor)
library(devtools)
library(doBy)
library(data.table)
library(ggplot2)
library(ggthemes)

#' Clear environment and set seed
#+ clear
remove(list=ls())
set.seed(776)

#' Document settings
#+ settings
opts_chunk$set(fig.width = 6, fig.height = 4)

#' ### Load data
#+ loadData
load(file="data/output_data/summary_results.R")
#' Separate by analysis
analysis.split <- split(summary.means, by = "Analysis")
category.split <- split(summary.means, by = "Category")

test <- merge(category.split$Rust, category.split$Yield, by = "Moderator")
test <- merge(test, category.split$`Seed Weight`, by = "Moderator")

#' ### Footer
#' 
#' Spun with ezspin("programs/results_processing.R", out_dir="output", fig_dir="figures", keep_md=FALSE)
#' 
#' Session Info:
#+ sessionInfo
devtools::session_info()