#' # Graphs of results
#' 
#' ### Preamble
#' 
#' Load Libraries
#+ libraries, messages=FALSE, warning=FALSE
library(ezknitr)
library(knitr)
library(metafor)
library(ggplot2)
library(dplyr)
library(proto)

#' Clear environment
#+ clear
remove(list=ls())

#' Document settings
#+ settings
opts_chunk$set(fig.width = 6, fig.height = 4)

#' ### Load data
#+ loadData
load(file="data/output_data/data_cleaned.R")
load(file="data/output_data/analysis_results.R")

#' ### Active ingredient results
#' 
#' Combine data for ggplot2
#+ dataActiveIng
coef.rust.active <- as.data.frame(rust.active$b)
coef.rust.active$dependent <- "Rust"
coef.yield.active <- as.data.frame(yield.active$b)
coef.yield.active$dependent <- "Yield"
coef.seedwt.active <- as.data.frame(seedwt.active$b)
coef.seedwt.active$dependent <- "100sw"
active.ing <- rbind(coef.rust.active, coef.yield.active, coef.seedwt.active)
colnames(active.ing) <- c("mean", "dependent")
# Append confidence intervals
active.ing$LL[active.ing$dependent=="Rust"] <- rust.active$ci.lb
active.ing$UL[active.ing$dependent=="Rust"] <- rust.active$ci.ub
active.ing$LL[active.ing$dependent=="Yield"] <- yield.active$ci.lb
active.ing$UL[active.ing$dependent=="Yield"] <- yield.active$ci.ub
active.ing$LL[active.ing$dependent=="100sw"] <- seedwt.active$ci.lb
active.ing$UL[active.ing$dependent=="100sw"] <- seedwt.active$ci.ub
# Names of active ingredients
active.ing$Category <- gsub("activeIngClean", "", rownames(active.ing))
active.ing$Category <- gsub("1", "", active.ing$Category)
active.ing$Category <- gsub("2", "", active.ing$Category)
#' Plot in ggplot
#+ figActiveIng
# 100 seed weight = red circle
# Yield = blue square
# Rust Severity = green triangle
ggplot(data=active.ing, aes(x = mean, y = Category, colour=dependent)) +
  geom_point(aes(shape=dependent), size=3)+
  geom_errorbarh(aes(xmin=LL, xmax=UL), height=0.4)+
  geom_vline(xintercept = 0, lty=2, color="grey")+
  theme_bw()+
  xlab("Standardized Mean Difference")+
  ylab("Active Ingredient")+
  theme(legend.position="none")

#' ### Class results
#' 
#' Combine data for ggplot2
#+ dataClass
coef.rust.class <- as.data.frame(rust.class$b)
coef.rust.class$dependent <- "Rust"
coef.yield.class <- as.data.frame(yield.class$b)
coef.yield.class$dependent <- "Yield"
coef.seedwt.class <- as.data.frame(seedwt.class$b)
coef.seedwt.class$dependent <- "100sw"
class.ing <- rbind(coef.rust.class, coef.yield.class, coef.seedwt.class)
colnames(class.ing) <- c("mean", "dependent")
# Append confidence intervals
class.ing$LL[class.ing$dependent=="Rust"] <- rust.class$ci.lb
class.ing$UL[class.ing$dependent=="Rust"] <- rust.class$ci.ub
class.ing$LL[class.ing$dependent=="Yield"] <- yield.class$ci.lb
class.ing$UL[class.ing$dependent=="Yield"] <- yield.class$ci.ub
class.ing$LL[class.ing$dependent=="100sw"] <- seedwt.class$ci.lb
class.ing$UL[class.ing$dependent=="100sw"] <- seedwt.class$ci.ub
# Names of active ingredients
class.ing$Category <- gsub("classClean", "", rownames(class.ing))
class.ing$Category <- gsub("1", "", class.ing$Category)
class.ing$Category <- gsub("2", "", class.ing$Category)
#' Plot in ggplot
#+ figClass
# 100 seed weight = red circle
# Yield = blue square
# Rust Severity = green triangle
ggplot(data=class.ing, aes(x = mean, y = Category, colour=dependent)) +
  geom_point(aes(shape=dependent), size=3)+
  geom_errorbarh(aes(xmin=LL, xmax=UL), height=0.4)+
  geom_vline(xintercept = 0, lty=2, color="grey")+
  theme_bw()+
  xlab("Standardized Mean Difference")+
  ylab("Class")+
  theme(legend.position="none")

#' ### Growth stage results
#' 
#' Combine data for ggplot2
#+ dataGrowthStage
coef.rust.growth.stage <- as.data.frame(rust.growth.stage$b)
coef.rust.growth.stage$dependent <- "Rust"
coef.yield.growth.stage <- as.data.frame(yield.growth.stage$b)
coef.yield.growth.stage$dependent <- "Yield"
coef.seedwt.growth.stage <- as.data.frame(seedwt.growth.stage$b)
coef.seedwt.growth.stage$dependent <- "100sw"
growth.stage.ing <- rbind(coef.rust.growth.stage, coef.yield.growth.stage, coef.seedwt.growth.stage)
colnames(growth.stage.ing) <- c("mean", "dependent")
# Append confidence intervals
growth.stage.ing$LL[growth.stage.ing$dependent=="Rust"] <- rust.growth.stage$ci.lb
growth.stage.ing$UL[growth.stage.ing$dependent=="Rust"] <- rust.growth.stage$ci.ub
growth.stage.ing$LL[growth.stage.ing$dependent=="Yield"] <- yield.growth.stage$ci.lb
growth.stage.ing$UL[growth.stage.ing$dependent=="Yield"] <- yield.growth.stage$ci.ub
growth.stage.ing$LL[growth.stage.ing$dependent=="100sw"] <- seedwt.growth.stage$ci.lb
growth.stage.ing$UL[growth.stage.ing$dependent=="100sw"] <- seedwt.growth.stage$ci.ub
# Names of growth.stage ingredients
growth.stage.ing$Category <- gsub("growthStateClean", "", rownames(growth.stage.ing))
growth.stage.ing$Category[growth.stage.ing$dependent=="Yield"] <- 
  gsub("1$", "", growth.stage.ing$Category[growth.stage.ing$dependent=="Yield"])
growth.stage.ing$Category[growth.stage.ing$dependent=="100sw"] <- 
  gsub("2$", "", growth.stage.ing$Category[growth.stage.ing$dependent=="100sw"])
#' Plot in ggplot
#+ figGrowthStage
# 100 seed weight = red circle
# Yield = blue square
# Rust Severity = green triangle
ggplot(data=growth.stage.ing, aes(x = mean, y = Category, colour=dependent)) +
  geom_point(aes(shape=dependent), size=3)+
  geom_errorbarh(aes(xmin=LL, xmax=UL), height=0.4)+
  geom_vline(xintercept = 0, lty=2, color="grey")+
  theme_bw()+
  xlab("Standardized Mean Difference")+
  ylab("Growth Stage")+
  theme(legend.position="none")

#' ### Application results
#' 
#' Combine data for ggplot2
#+ dataApplications
coef.rust.applications.cat <- as.data.frame(rust.applications.cat$b)
coef.rust.applications.cat$dependent <- "Rust"
coef.yield.applications.cat <- as.data.frame(yield.applications.cat$b)
coef.yield.applications.cat$dependent <- "Yield"
coef.seedwt.applications.cat <- as.data.frame(seedwt.applications.cat$b)
coef.seedwt.applications.cat$dependent <- "100sw"
applications.cat <- rbind(coef.rust.applications.cat, coef.yield.applications.cat, coef.seedwt.applications.cat)
colnames(applications.cat) <- c("mean", "dependent")
# Append confidence intervals
applications.cat$LL[applications.cat$dependent=="Rust"] <- rust.applications.cat$ci.lb
applications.cat$UL[applications.cat$dependent=="Rust"] <- rust.applications.cat$ci.ub
applications.cat$LL[applications.cat$dependent=="Yield"] <- yield.applications.cat$ci.lb
applications.cat$UL[applications.cat$dependent=="Yield"] <- yield.applications.cat$ci.ub
applications.cat$LL[applications.cat$dependent=="100sw"] <- seedwt.applications.cat$ci.lb
applications.cat$UL[applications.cat$dependent=="100sw"] <- seedwt.applications.cat$ci.ub
# Names of applications.cat ingredients
applications.cat$Category <- gsub("applicationsCat", "", rownames(applications.cat))
applications.cat$Category[applications.cat$dependent=="Yield"] <- 
  gsub("1$", "", applications.cat$Category[applications.cat$dependent=="Yield"])
applications.cat$Category[applications.cat$dependent=="100sw"] <- 
  gsub("2$", "", applications.cat$Category[applications.cat$dependent=="100sw"])
applications.cat$CategoryNumb <- as.numeric(applications.cat$Category)
#' Plot in ggplot
#+ figApplications
# 100 seed weight = red circle
# Yield = blue square
# Rust Severity = green triangle
ggplot(data=applications.cat, aes(x = mean, y = as.numeric(Category), colour=dependent)) +
  geom_point(aes(shape=dependent), size=3)+
  geom_errorbarh(aes(xmin=LL, xmax=UL), height=0.4)+
  geom_vline(xintercept = 0, lty=2, color="grey")+
  theme_bw()+
  ylab("Applications")+
  xlab("Standardized Mean Difference")+
  theme(legend.position="none")
# Add lines for regression
pred.app.yield <- predict(yield.applications, newmods = seq(from=0, to=6, by=0.01))
pred.app.yield <- as.data.frame(cbind(seq(from=0, to=6, by=0.01),
                                      pred.app.yield$pred,
                                         pred.app.yield$ci.lb,
                                         pred.app.yield$ci.ub))
colnames(pred.app.yield) <- c("x", "mean", "pLL", "pUL")
pred.app.yield$dependent <- "Yield"
pred.app.100sw <- predict(seedwt.applications, newmods = seq(from=0, to=6, by=0.01))
pred.app.100sw <- as.data.frame(cbind(seq(from=0, to=6, by=0.01),
                                      pred.app.100sw$pred,
                                      pred.app.100sw$ci.lb,
                                      pred.app.100sw$ci.ub))
colnames(pred.app.100sw) <- c("x", "mean", "pLL", "pUL")
pred.app.100sw$dependent="100sw"
pred.app <- rbind(pred.app.yield, pred.app.100sw)

ggplot(data=pred.app, aes(x = x, y=mean))+
  geom_line(aes(color=dependent))+
  geom_ribbon(aes(ymin=pLL, ymax=pUL, fill=dependent), alpha=0.4)+
  geom_point(data=applications.cat,
             aes(x=CategoryNumb, y=mean, colour=dependent, shape=dependent))+
  geom_errorbar(data=applications.cat,
                aes(x=CategoryNumb, ymin=LL, ymax=UL,
                    color=dependent), width=0.4)+
  geom_hline(yintercept = 0, lty=2, color="grey")+
  theme_bw()+
  xlab("Applications")+
  ylab("Standardized Mean Difference")+
  theme(legend.position="none")

#' ### Footer
#' 
#' Spun with ezspin("programs/result_graphing.R", out_dir="output", fig_dir="figures", keep_md=FALSE)
#' 
#' Session Info:
#+ sessionInfo
sessionInfo()