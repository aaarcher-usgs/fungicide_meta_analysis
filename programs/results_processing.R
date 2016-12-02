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

#' Clear environment and set seed
#+ clear
remove(list=ls())
set.seed(281)

#' Document settings
#+ settings
opts_chunk$set(fig.width = 6, fig.height = 4)

#' ### Load data
#+ loadData
raw.data <- read.csv("data/workspace.csv")
load(file="data/output_data/data_cleaned.R")
rust.data <- as.data.frame(rust.data.ROM)
yield.data <- as.data.frame(yield.data.ROM)
seedwt.data <- as.data.frame(seedwt.data.ROM)
load(file="data/output_data/results_rust.R")
load(file="data/output_data/results_yield.R")
load(file="data/output_data/results_seedwt.R")

#' #############################################################################
#' ### Step 1: Calculate numbers for paper: Dataset properties
#' 
#' **Number of studies and entries of raw data**
print(c("Studies in raw data", length(unique(raw.data$Reference))))
print(c("Entries in raw data", nrow(raw.data)))
#' 
#' **Number of studies/category and number of entries/category**
#' 
#' Number of studies overall
print(c("Studies in rust dataset: ",length(unique(rust.data$ReferenceNumb))))
print(c("Studies in yield dataset: ",length(unique(yield.data$ReferenceNumb))))
print(c("Studies in seedwt dataset: ",length(unique(seedwt.data$ReferenceNumb))))
#' Number of entries overall
print(c("Entries in rust dataset: ",nrow(rust.data)))
print(c("Entries in yield dataset: ",nrow(yield.data)))
print(c("Entries in seedwt dataset: ",nrow(seedwt.data)))
#' Table of references with number of entries/study (rust)
referencetable <- sort(table(rust.data$Reference))
referencetable[referencetable>=1]

#' **Number of entries and studies by year and location**
summaryBy(FID~studyYear+State+Reference, data=rust.data, FUN=length)

#' **Mean and std of dependent vars**
print(c("mean, SD of rust severity in treated plants",
        mean(rust.data$m1i), sd(rust.data$m1i)))
print(c("mean, SD of rust severity in control plants",
        mean(rust.data$m2i), sd(rust.data$m2i)))
print(c("mean, SD of rust severity response ratio in control plants",
        mean(exp(rust.data$yi)), sd(exp(rust.data$yi))))

print(c("mean, SD of yield in treated plants",
        mean(yield.data$m1i), sd(yield.data$m1i)))
print(c("mean, SD of yield in control plants",
        mean(yield.data$m2i), sd(yield.data$m2i)))
print(c("mean, SD of yield response ratio in control plants",
        mean(exp(yield.data$yi)), sd(exp(yield.data$yi))))

print(c("mean, SD of seedwt in treated plants",
        mean(seedwt.data$m1i), sd(seedwt.data$m1i)))
print(c("mean, SD of seedwt in control plants",
        mean(seedwt.data$m2i), sd(seedwt.data$m2i)))
print(c("mean, SD of seedwt response ratio in control plants",
        mean(exp(seedwt.data$yi)), sd(exp(seedwt.data$yi))))

#' #############################################################################
#' ### Step 1: Calculate numbers for paper: Results
#' 
#' 
#' **Overall mean, 95% CI and Tau^2**
print(c("Mean e.s. of rust severity", mean(exp(results.rust$OVERALL))))
print(c("CI of mean e.s. of rust severity", 
        quantile(exp(results.rust$OVERALL), probs = c(0.025,0.975))))
print(c("Mean tau^2 for rust", mean(results.rust$tau2)))

print(c("Mean e.s. of yield severity", mean(exp(results.yield$OVERALL))))
print(c("CI of mean e.s. of yield severity", 
        quantile(exp(results.yield$OVERALL), probs = c(0.025,0.975))))
print(c("Mean tau^2 for yield", mean(results.yield$tau2)))


#' ### Footer
#' 
#' Spun with ezspin("programs/results_processing.R", out_dir="output", fig_dir="figures", keep_md=FALSE)
#' 
#' Session Info:
#+ sessionInfo
devtools::session_info()