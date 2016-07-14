#' # Data analysis
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


#' ## Meta-analysis of rust severity
#' 
#' Overall raw mean difference
#+ overallDiffRust
overall.rust <- rma.uni(yi = yi, vi = vi, data = rust.data.MD, method="ML")
summary(overall.rust)

#' Effects of Active Ingredient on SMD
#+ activeVsRust
rust.active <- rma.uni(yi = yi, vi = vi, data=rust.data.SMD,
                      method = "REML", mods = ~activeIngClean-1)
summary(rust.active)

#' Effects of Class on SMD
#+ classVsRust
rust.class <- rma.uni(yi = yi, vi = vi, data=rust.data.SMD,
                      method = "REML", mods = ~classClean-1)
summary(rust.class)

#' Effects of Growth stage on SMD
#+ growthStageVsRust
rust.growth.stage <- rma.uni(yi = yi, vi = vi, 
                            data=rust.data.SMD[rust.data.SMD$growthStateClean!="unknown",],
                            method = "REML", mods = ~growthStateClean-1)
summary(rust.growth.stage)

#' Effects of application number on SMD
#+ appsNumbVsRust
table(rust.data.SMD$applicationsNumb)
rust.applications <- rma.uni(yi = yi, vi = vi, 
                            data=rust.data.SMD,
                            method = "REML", mods = ~applicationsNumb)
summary(rust.applications)
# Categorical
rust.data.SMD$applicationsCat <- as.character(rust.data.SMD$applicationsNumb)
rust.applications.cat <- rma.uni(yi = yi, vi = vi, 
                            data=rust.data.SMD,
                            method = "REML", mods = ~applicationsCat-1)
summary(rust.applications.cat)

#' ## Meta-analysis of yield
#' 
#' Overall mean difference
#+ overallDiffYield
overall.yield <- rma.uni(yi = yi, vi = vi, data = yield.data.MD, method = "REML")
summary(overall.yield)

#' Effects of Active Ingredient on SMD
#+ activeVsYield
yield.active <- rma.uni(yi = yi, vi = vi, data=yield.data.SMD,
                      method = "REML", mods = ~activeIngClean-1)
summary(yield.active)

#' Effects of Class on SMD
#+ classVsYield
yield.class <- rma.uni(yi = yi, vi = vi, data=yield.data.SMD,
                     method = "REML", mods = ~classClean-1)
summary(yield.class)

#' Effects of Growth stage on SMD
#+ growthStageVsYield
yield.growth.stage <- rma.uni(yi = yi, vi = vi, 
                            data=yield.data.SMD[yield.data.SMD$growthStateClean!="unknown",],
                            method = "REML", mods = ~growthStateClean-1)
summary(yield.growth.stage)

#' Effects of application number on SMD
#+ appsNumbVsYield
table(yield.data.SMD$applicationsNumb)
yield.applications <- rma.uni(yi = yi, vi = vi, 
                            data=yield.data.SMD,
                            method = "REML", mods = ~applicationsNumb)
summary(yield.applications)
# Categorical
yield.data.SMD$applicationsCat <- as.character(yield.data.SMD$applicationsNumb)
yield.applications.cat <- rma.uni(yi = yi, vi = vi, 
                                data=yield.data.SMD,
                                method = "REML", mods = ~applicationsCat-1)
summary(yield.applications.cat)


#' ## Meta-analysis of seedwt
#' 
#' Overall mean difference
#+ overallDiffSeed
overall.seedwt <- rma.uni(yi = yi, vi = vi, data = seedwt.data.MD, method = "REML")
summary(overall.seedwt)

#' Effects of Active Ingredient on SMD
#+ activeVsSeed
seedwt.active <- rma.uni(yi = yi, vi = vi, data=seedwt.data.SMD,
                       method = "REML", mods = ~activeIngClean-1)
summary(seedwt.active)

#' Effects of Class on SMD
#+ classVsSeed
seedwt.class <- rma.uni(yi = yi, vi = vi, data=seedwt.data.SMD,
                      method = "REML", mods = ~classClean-1)
summary(seedwt.class)

#' Effects of Growth stage on SMD
#+ growthStageVsSeed
seedwt.growth.stage <- rma.uni(yi = yi, vi = vi, 
                             data=seedwt.data.SMD[seedwt.data.SMD$growthStateClean!="unknown",],
                             method = "REML", mods = ~growthStateClean-1)
summary(seedwt.growth.stage)

#' Effects of application number on SMD
#+ appsNumbVsSeed
table(seedwt.data.SMD$applicationsNumb)
seedwt.applications <- rma.uni(yi = yi, vi = vi, 
                             data=seedwt.data.SMD,
                             method = "REML", mods = ~applicationsNumb)
summary(seedwt.applications)
# Categorical
seedwt.data.SMD$applicationsCat <- as.character(seedwt.data.SMD$applicationsNumb)
seedwt.applications.cat <- rma.uni(yi = yi, vi = vi, 
                                 data=seedwt.data.SMD,
                                 method = "REML", mods = ~applicationsCat-1)
summary(seedwt.applications.cat)

#' ## Meta-analysis of Cercospora severity
#' 
#' Overall mean difference
#+ overallDiffCerco
overall.cerco <- rma.uni(yi = yi, vi = vi, data = cerco.data.MD, method = "REML")
summary(overall.cerco)

#' ## Meta-analysis of Target spot severity
#' 
#' Overall mean difference
#+ overallDiffTargetSpot
overall.target.spot <- rma.uni(yi = yi, vi = vi, data = target.spot.data.MD, method = "REML")
summary(overall.target.spot)

#' ## Save for graphing
#+ saving
save(overall.rust, overall.yield, overall.seedwt, overall.cerco, overall.target.spot,
     rust.active, rust.class, rust.growth.stage, rust.applications, rust.applications.cat,
     yield.active, yield.class, yield.growth.stage, yield.applications, yield.applications.cat,
     seedwt.active, seedwt.class, seedwt.growth.stage, seedwt.applications, seedwt.applications.cat,
     file="data/output_data/analysis_results.R")


#' ### Footer
#' 
#' Spun with ezspin("programs/data_analysis.R", out_dir="output", fig_dir="figures", keep_md=FALSE)
#' 
#' Session Info:
#+ sessionInfo
sessionInfo()