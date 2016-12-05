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

#' **Number of entries and studies by year and location**
summaryBy(FID~studyYear+Reference, data=rust.data, FUN=length)
table(rust.data$studyYear)
summaryBy(FID~State+Reference, data=rust.data, FUN=length)
table(rust.data$State)

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
#' ### Step 2: Information for table with references
summaryBy(FID~ReferenceNumb+Reference, data=raw.data, FUN=length)

# For Rust analysis
summaryBy(FID~ReferenceNumb+Reference, data=rust.data, FUN=length)


#' #############################################################################
#' ### Step 3: Calculate numbers for paper: Results
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

print(c("Mean e.s. of seedwt severity", mean(exp(results.seedwt$OVERALL))))
print(c("CI of mean e.s. of seedwt severity", 
        quantile(exp(results.seedwt$OVERALL), probs = c(0.025,0.975))))
print(c("Mean tau^2 for seedwt", mean(results.seedwt$tau2)))

#' ### Store means from each in new dataframe
summary.means <- as.data.frame(matrix(NA, nrow = 65, ncol = 5))
colnames(summary.means) <- c("Category", "Moderator" , "Mean", "LL", "UL")

#' Transform model results 
transform.rust <- exp(results.rust)
colnames(transform.rust) <- colnames(results.rust)
transform.rust$tau2 <- results.rust$tau2

transform.yield <- exp(results.yield)
colnames(transform.yield) <- colnames(results.yield)
transform.yield$tau2 <- results.yield$tau2

transform.seedwt <- exp(results.seedwt)
colnames(transform.seedwt) <- colnames(results.seedwt)
transform.seedwt$tau2 <- results.seedwt$tau2

#' Rust means
summary.means$Moderator[1:23] <- colnames(results.rust)
summary.means$Category[1:23] <- "Rust"
summary.means$Mean[1:23] <- apply(X = transform.rust, MARGIN = 2, FUN = mean, na.rm=T)
summary.means$LL[1:23] <- apply(X = transform.rust, MARGIN = 2, 
                                FUN = function(x){quantile(x, probs = c(0.025), na.rm=T)})
summary.means$UL[1:23] <- apply(X = transform.rust, MARGIN = 2, 
                                FUN = function(x){quantile(x, probs = c(0.975), na.rm=T)})

#' Yield means
summary.means$Moderator[24:48] <- colnames(results.yield)
summary.means$Category[24:48] <- "Yield"
summary.means$Mean[24:48] <- apply(X = transform.yield, MARGIN = 2, FUN = mean, na.rm=T)
summary.means$LL[24:48] <- apply(X = transform.yield, MARGIN = 2, 
                                FUN = function(x){quantile(x, probs = c(0.025), na.rm=T)})
summary.means$UL[24:48] <- apply(X = transform.yield, MARGIN = 2, 
                                FUN = function(x){quantile(x, probs = c(0.975), na.rm=T)})

#' Seed weight means
summary.means$Moderator[49:65] <- colnames(results.seedwt)
summary.means$Category[49:65] <- "Seed Weight"
summary.means$Mean[49:65] <- apply(X = transform.seedwt, MARGIN = 2, FUN = mean, na.rm=T)
summary.means$LL[49:65] <- apply(X = transform.seedwt, MARGIN = 2, 
                                 FUN = function(x){quantile(x, probs = c(0.025), na.rm=T)})
summary.means$UL[49:65] <- apply(X = transform.seedwt, MARGIN = 2, 
                                 FUN = function(x){quantile(x, probs = c(0.975), na.rm=T)})

summary.means$Analysis[summary.means$Moderator=="1 Application" | 
                         summary.means$Moderator=="2 Applications"] <- "Applications"
summary.means$Analysis[summary.means$Moderator=="2006" | 
                         summary.means$Moderator=="2007"|
                         summary.means$Moderator=="2013"] <- "Study Year"
summary.means$Analysis[summary.means$Moderator=="AZO + PROP"|
                         summary.means$Moderator=="FLUT"|
                         summary.means$Moderator=="PYR"|
                         summary.means$Moderator=="MIXED"|
                         summary.means$Moderator=="TEBU"] <- "Active Ingredient"
summary.means$Analysis[summary.means$Moderator=="low"|
                         summary.means$Moderator=="medium"|
                         summary.means$Moderator=="high"] <- "Disease Pressure"
summary.means$Analysis[summary.means$Moderator=="R1+"|
                         summary.means$Moderator=="R2+"|
                         summary.means$Moderator=="R3"|
                         summary.means$Moderator=="R5"] <- "Growth Stage"
summary.means$Analysis[summary.means$Moderator=="Strobilurin"|
                         summary.means$Moderator=="Triaz_Strob"|
                         summary.means$Moderator=="Triazole"] <- "Fungicide Class"

#' Save summary.means
save(summary.means, file="data/output_data/summary_results.R")

#' ### Fungicide table for manuscript
#' 
#' Rust
#' 
rust.data$temp <- paste0(rust.data$Reference,":",
                         rust.data$category_class)
length(unique(rust.data$temp[rust.data$category_class=="strobilurin"]))
length(unique(rust.data$temp[rust.data$category_class=="triazole"]))
length(unique(rust.data$temp[rust.data$category_class=="triaz + strob"]))
summaryBy(FID~category_class, 
          data = rust.data[! is.na(rust.data$category_class),], FUN=length)
summaryBy(FID~category_class+alphaIngred+Reference, 
          data = rust.data[! is.na(rust.data$alphaIngred),], FUN=length)
table(rust.data$category_ai)
unique(rust.data$Reference[rust.data$category_ai=="FLUT"])
unique(rust.data$Reference[rust.data$category_ai=="MIXED"])
unique(rust.data$Reference[rust.data$category_ai=="PYR"])
unique(rust.data$Reference[rust.data$category_ai=="TEBU"])

#' Yield
#' 
yield.data$temp <- paste0(yield.data$Reference,":",
                         yield.data$category_class)
length(unique(yield.data$temp[yield.data$category_class=="strobilurin"]))
length(unique(yield.data$temp[yield.data$category_class=="triazole"]))
length(unique(yield.data$temp[yield.data$category_class=="triaz + strob"]))
summaryBy(FID~category_class, 
          data = yield.data[! is.na(yield.data$category_class),], FUN=length)

table(yield.data$category_ai)
unique(yield.data$Reference[yield.data$category_ai=="FLUT"])
unique(yield.data$Reference[yield.data$category_ai=="MIXED"])
unique(yield.data$Reference[yield.data$category_ai=="PYR"])
unique(yield.data$Reference[yield.data$category_ai=="TEBU"])

#' 100-seed-weight
seedwt.data$temp <- paste0(seedwt.data$Reference,":",
                          seedwt.data$category_class)
length(unique(seedwt.data$temp[seedwt.data$category_class=="strobilurin"]))
length(unique(seedwt.data$temp[seedwt.data$category_class=="triazole"]))
length(unique(seedwt.data$temp[seedwt.data$category_class=="triaz + strob"]))
summaryBy(FID~category_class, 
          data = seedwt.data[! is.na(seedwt.data$category_class),], FUN=length)

table(seedwt.data$category_ai)
unique(seedwt.data$Reference[seedwt.data$category_ai=="FLUT"])
unique(seedwt.data$Reference[seedwt.data$category_ai=="MIXED"])
unique(seedwt.data$Reference[seedwt.data$category_ai=="TEBU"])

#' ### Footer
#' 
#' Spun with ezspin("programs/results_processing.R", out_dir="output", fig_dir="figures", keep_md=FALSE)
#' 
#' Session Info:
#+ sessionInfo
devtools::session_info()