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
library(devtools)
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
analysis.split <- split(summary.means, summary.means$Analysis)
category.split <- split(summary.means, summary.means$Category)

wide <- merge(category.split$Rust, category.split$Yield, by = "Moderator")
wide <- merge(wide, category.split$`Seed Weight`, by = "Moderator")

ggplot(data = wide[!is.na(wide$Analysis.x),], aes(x = Mean.x, y = Mean.y))+geom_point()+geom_errorbar(aes(ymin=LL.y,ymax=UL.y))+geom_errorbarh(aes(xmin=LL.x,xmax=UL.x))

ggplot(data = wide[!is.na(wide$Analysis.x),], aes(x = Mean.x, y = Mean))+geom_point()+geom_errorbar(aes(ymin=LL,ymax=UL))+geom_errorbarh(aes(xmin=LL.x,xmax=UL.x))


ggplot(data = wide[!is.na(wide$Analysis.x),], aes(x = Mean, y = Mean.y))+geom_point()+geom_errorbar(aes(ymin=LL.y,ymax=UL.y))+geom_errorbarh(aes(xmin=LL,xmax=UL))

#' ### Footer
#' 
#' Spun with ezspin("programs/results_processing.R", out_dir="output", fig_dir="figures", keep_md=FALSE)
#' 
#' Session Info:
#+ sessionInfo
devtools::session_info()