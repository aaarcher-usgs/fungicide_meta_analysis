#' # Sensitivity Analysis
#' 
#' Including:
#' * Tests of effect sizes versus study year
#' * Influence diagnostics
#' * Tests for the effect of pseudoreplication (multiple data points per study)
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

#' ### Tests of effect sizes versus study year
#' 
#' Rust
#+ rustVsYear
rust.v.year <- rma.uni(yi = yi, vi = vi, data=rust.data.SMD[rust.data.SMD$studyYear!=2009&
                                                              rust.data.SMD$studyYear!=2010&
                                                              rust.data.SMD$studyYear!=2011,],
                       method = "REML",
                       mods = ~studyYear)
rust.v.year.cat <- rma.uni(yi = yi, vi = vi, data=rust.data.SMD[rust.data.SMD$studyYear!=2009&
                                                                  rust.data.SMD$studyYear!=2010&
                                                                  rust.data.SMD$studyYear!=2011,],
                             method = "REML",
                             mods = ~as.character(studyYear)-1)
summary(rust.v.year)

#' Yield
#+ yieldVsYear
yield.v.year <- rma.uni(yi = yi, vi = vi, data=yield.data.SMD,
                       method = "REML",
                       mods = ~studyYear)
yield.v.year.cat <- rma.uni(yi = yi, vi = vi, data=yield.data.SMD,
                             method = "REML",
                             mods = ~as.character(studyYear)-1)
summary(yield.v.year)

#' 100-seed-weight
#+ seedwtVsYear
seedwt.v.year <- rma.uni(yi = yi, vi = vi, data=seedwt.data.SMD,
                       method = "REML",
                       mods = ~studyYear)
seedwt.v.year.cat <- rma.uni(yi = yi, vi = vi, data=seedwt.data.SMD,
                             method = "REML",
                             mods = ~as.character(studyYear)-1)
summary(seedwt.v.year)

#' Combine data for ggplot2
#+ figEffectsVsYear
# Yield data
pred.yr.yield <- predict(yield.v.year, newmods = seq(from=2004, to=2014, by=0.1))
pred.yr.yield <- as.data.frame(cbind(seq(from=2004, to=2014, by=0.1),
                                     pred.yr.yield$pred,
                                     pred.yr.yield$ci.lb,
                                     pred.yr.yield$ci.ub))
colnames(pred.yr.yield) <- c("x", "mean", "pLL", "pUL")
pred.yr.yield$dependent <- "Yield"
coef.yield.year.cat <- as.data.frame(cbind(yield.v.year.cat$b, 
                                           yield.v.year.cat$ci.lb, 
                                           yield.v.year.cat$ci.ub))
coef.yield.year.cat$dependent <- "Yield"

# Rust data
pred.yr.rust <- predict(rust.v.year, newmods = seq(from=2004, to=2014, by=0.1))
pred.yr.rust <- as.data.frame(cbind(seq(from=2004, to=2014, by=0.1),
                                     pred.yr.rust$pred,
                                     pred.yr.rust$ci.lb,
                                     pred.yr.rust$ci.ub))
colnames(pred.yr.rust) <- c("x", "mean", "pLL", "pUL")
pred.yr.rust$dependent <- "Rust"
coef.rust.year.cat <- as.data.frame(cbind(rust.v.year.cat$b, 
                                          rust.v.year.cat$ci.lb, 
                                          rust.v.year.cat$ci.ub))
coef.rust.year.cat$dependent <- "Rust"

# 100-seed-weight
pred.yr.seedwt <- predict(seedwt.v.year, newmods = seq(from=2004, to=2014, by=0.1))
pred.yr.seedwt <- as.data.frame(cbind(seq(from=2004, to=2014, by=0.1),
                                     pred.yr.seedwt$pred,
                                     pred.yr.seedwt$ci.lb,
                                     pred.yr.seedwt$ci.ub))
colnames(pred.yr.seedwt) <- c("x", "mean", "pLL", "pUL")
pred.yr.seedwt$dependent <- "100sw"
pred.app <- rbind(pred.yr.yield, pred.yr.rust, pred.yr.seedwt)
coef.seedwt.year.cat <- as.data.frame(cbind(seedwt.v.year.cat$b, 
                                      seedwt.v.year.cat$ci.lb, 
                                      seedwt.v.year.cat$ci.ub))
coef.seedwt.year.cat$dependent <- "100sw"

# Bind categorical data together
year.cat <- rbind(coef.seedwt.year.cat, 
                          coef.rust.year.cat,
                          coef.yield.year.cat)
colnames(year.cat) <- c("mean", "LL", "UL", "dependent")
year.cat$Year[year.cat$dependent=="Yield"] <- 
  sort(unique(yield.data.SMD$studyYear))
year.cat$Year[year.cat$dependent=="Rust"] <- 
  sort(unique(rust.data.SMD$studyYear[rust.data.SMD$studyYear!=2009&
                                        rust.data.SMD$studyYear!=2010&
                                        rust.data.SMD$studyYear!=2011]))
year.cat$Year[year.cat$dependent=="100sw"] <- 
  sort(unique(seedwt.data.SMD$studyYear))

ggplot(data=pred.app, aes(x = x, y=mean))+
  geom_line(aes(color=dependent))+
  geom_ribbon(aes(ymin=pLL, ymax=pUL, fill=dependent), alpha=0.4)+
  geom_point(data=year.cat,
             aes(x=as.numeric(Year), y=mean, colour=dependent, shape=dependent))+
  geom_hline(yintercept = 0, lty=2, color="grey")+
  geom_errorbar(data=year.cat, 
                aes(x=Year, ymin=LL, ymax=UL, color=dependent) ,width=0.4)+
  theme_bw()+
  xlab("Study Year")+
  ylab("Standardized Mean Difference")+
  theme(legend.position="none")

#' ### Influence diagnostics
#' 
#' Rust
#+ rustInfluence, fig.width=5, fig.height=10
# Model with all of the data points included
overall.std.rust <- rma.uni(yi = yi, vi = vi, data = rust.data.SMD, method="ML")
summary(overall.std.rust)
# Testing the influence of each data point
inf.rust <- influence.rma.uni(overall.std.rust) # Test the influence
plot(inf.rust) # Plot the influence 
rust.data.SMD[inf.rust$is.infl==TRUE,c(1:3,47:50)] # Records that are influential
rust.is.infl <- rust.data.SMD$FID[inf.rust$is.infl==TRUE]
# Re-run analysis without the influential datapoints
overall.std.rust.noinfl <- rma.uni(yi = yi, vi = vi, 
                            data = rust.data.SMD[!rust.data.SMD$FID %in% rust.is.infl,], method="ML")
summary(overall.std.rust.noinfl)

#' Yield
#+ yieldInfluence, fig.width=5, fig.height=10
# Model with all of the data points included
overall.std.yield <- rma.uni(yi = yi, vi = vi, data = yield.data.SMD, method="ML")
summary(overall.std.yield)
# Testing the influence of each data point
inf.yield <- influence.rma.uni(overall.std.yield) # Test the influence
plot(inf.yield) # Plot the influence 
yield.data.SMD[inf.yield$is.infl==TRUE,c(1:3,48:51)] # Records that are influential
yield.is.infl <- yield.data.SMD$FID[inf.yield$is.infl==TRUE]
# Re-run analysis without the influential datapoints
overall.std.yield.noinfl <- rma.uni(yi = yi, vi = vi, 
                                   data = yield.data.SMD[!yield.data.SMD$FID %in% yield.is.infl,], method="ML")
summary(overall.std.yield.noinfl)

#' 100-seed-weight
#+ seedwtInfluence, fig.width=5, fig.height=10
# Model with all of the data points included
overall.std.seedwt <- rma.uni(yi = yi, vi = vi, data = seedwt.data.SMD, method="ML")
summary(overall.std.seedwt)
# Testing the influence of each data point
inf.seedwt <- influence.rma.uni(overall.std.seedwt) # Test the influence
plot(inf.seedwt) # Plot the influence 
seedwt.data.SMD[inf.seedwt$is.infl==TRUE,c(1:3,47:50)] # Records that are influential
# No record were found to be influential!

#' Cercospora
#+ cercoInfluence, fig.width=5, fig.height=10
# Model with all of the data points included
overall.std.cerco <- rma.uni(yi = yi, vi = vi, data = cerco.data.MD, method="ML")
summary(overall.std.cerco)
# Testing the influence of each data point
inf.cerco <- influence.rma.uni(overall.std.cerco) # Test the influence
plot(inf.cerco) # Plot the influence 
cerco.data.MD[inf.cerco$is.infl==TRUE,c(1:3,47:50)] # Records that are influential
# No record were found to be influential!

#' Target Spot
#+ targetSpotInfluence, fig.width=5, fig.height=10
# Model with all of the data points included
overall.std.target.spot <- rma.uni(yi = yi, vi = vi, data = target.spot.data.MD, method="ML")
summary(overall.std.target.spot)
# Testing the influence of each data point
inf.target.spot <- influence.rma.uni(overall.std.target.spot) # Test the influence
plot(inf.target.spot) # Plot the influence 
target.spot.data.MD[inf.target.spot$is.infl==TRUE,c(1:3,47:50)] # Records that are influential
target.spot.is.infl <- target.spot.data.MD$FID[inf.target.spot$is.infl==TRUE]
# No record were found to be influential!


#' ### Test for the effects of pseudoreplication
#' 
#' Do analysis results change if random-effects model with study-level clustering is incorporated?
#' 
#' Rust
#+ rustMV
rust.pseudorep <- rma.mv(yi = yi, V = vi, data=rust.data.SMD,
                         random = ~1|Reference)
summary(rust.pseudorep)

#' Yield
#+ yieldMV
yield.pseudorep <- rma.mv(yi = yi, V = vi, data=yield.data.SMD,
                         random = ~1|Reference)
summary(yield.pseudorep)

#' 100-seed-weight
#+ seedwtMV
seedwt.pseudorep <- rma.mv(yi = yi, V = vi, data=seedwt.data.SMD,
                         random = ~1|Reference)
summary(seedwt.pseudorep)

#' Cercospora
#+ cercoMV
cerco.pseudorep <- rma.mv(yi = yi, V = vi, data=cerco.data.MD,
                         random = ~1|Reference)
summary(cerco.pseudorep)

#' Target spot
#+ targetSpotMV
target.spot.pseudorep <- rma.mv(yi = yi, V = vi, data=target.spot.data.MD,
                         random = ~1|Reference)
summary(target.spot.pseudorep)

#' ### Footer
#' 
#' Spun with ezspin("programs/sensitivity_analysis.R", out_dir="output", fig_dir="figures", keep_md=FALSE)
#' 
#' Session Info:
#+ sessionInfo
sessionInfo()