#' # Results plotting
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

wide <- merge(category.split$Rust, category.split$Yield, by = "Moderator",all=T)
wide <- merge(wide, category.split$`Seed Weight`, by = "Moderator",all = T)

#' ### Scatter plots of Relationships between dependent variables 
#' 
#' Rust vs Yield
ggplot(data = wide[!is.na(wide$Analysis.x),], aes(x = Mean.x, y = Mean.y))+
  geom_point()+
  geom_errorbar(aes(ymin=LL.y,ymax=UL.y))+
  geom_errorbarh(aes(xmin=LL.x,xmax=UL.x))+
  theme_tufte()

#' Rust vs Seed Weight
ggplot(data = wide[!is.na(wide$Analysis.x),], aes(x = Mean.x, y = Mean))+
  geom_point()+
  geom_errorbar(aes(ymin=LL,ymax=UL))+
  geom_errorbarh(aes(xmin=LL.x,xmax=UL.x))+
  theme_tufte()

#' Seed Weight vs Yield
ggplot(data = wide[!is.na(wide$Analysis.x),], aes(x = Mean, y = Mean.y))+
  geom_point()+
  geom_errorbar(aes(ymin=LL.y,ymax=UL.y))+
  geom_errorbarh(aes(xmin=LL,xmax=UL))+
  theme_tufte()

#' ### Scatter plot main results
new.wide.rust <- category.split$Rust
new.wide.y100 <- rbind(category.split$Yield, category.split$`Seed Weight`)
new.wide <- merge(new.wide.rust, new.wide.y100, by = "Moderator",all = T)

#' All analyses types
ggplot(data = new.wide[!is.na(new.wide$Analysis.x),], 
       aes(x = Mean.x, y = Mean.y, colour=Category.y, shape=Category.y))+
  geom_errorbar(aes(ymin=LL.y,ymax=UL.y),colour="grey")+
  geom_errorbarh(aes(xmin=LL.x,xmax=UL.x),colour="grey")+
  geom_point(size=2)+
  geom_errorbar(data = new.wide[new.wide$Analysis.y=="Overall Mean",],
                aes(ymin=LL.y,ymax=UL.y),colour="black",width=0)+
  geom_errorbarh(data = new.wide[new.wide$Analysis.y=="Overall Mean",],
                 aes(xmin=LL.x,xmax=UL.x),colour="black",height=0)+
  geom_point(data = new.wide[new.wide$Analysis.y=="Overall Mean",],colour="black",size=2)+
  theme_tufte()

#' Active Ingredient
ggplot(data = new.wide[new.wide$Analysis.x=="Active Ingredient"&
                         !is.na(new.wide$Analysis.x),], 
       aes(x = Mean.x, y = Mean.y, shape=Moderator, colour=Category.y))+
  geom_point()+
  geom_errorbar(aes(ymin=LL.y,ymax=UL.y))+
  geom_errorbarh(aes(xmin=LL.x,xmax=UL.x))+
  theme_tufte()

#' Active Ingredient
ggplot(data = new.wide[new.wide$Analysis.x=="Study Year"&
                         !is.na(new.wide$Analysis.x),], 
       aes(x = Mean.x, y = Mean.y, shape=Moderator, colour=Category.y))+
  geom_point()+
  geom_errorbar(aes(ymin=LL.y,ymax=UL.y))+
  geom_errorbarh(aes(xmin=LL.x,xmax=UL.x))+
  theme_tufte()


#' ### Main results
#' 
pd <- position_dodge(width=0.2)
#' Active Ingredient
ggplot(data = summary.means[summary.means$Analysis=="Active Ingredient"|
                              summary.means$Moderator=="OVERALL",], 
       aes(x = Moderator, y = Mean, colour=Category, shape=Category))+
  geom_pointrange(aes(ymin = LL, ymax = UL), position=pd)+
  geom_hline(aes(yintercept=1), colour="grey")+
  geom_vline(aes(xintercept=1.5), colour="grey")+
  scale_x_discrete(limits=c("OVERALL","FLUT","PYR","TEBU","MIXED","AZO_PROP"), 
                   labels=c("Overall","FLUT","PYR","TEBU","Mixed","AZO + \nPROP"))+
  ylab("Mean Effect Size (95% C.I.)")+
  xlab("Active Ingredient")+
  theme_tufte()+
  theme(legend.position = "none")

#' Fungicide Class
ggplot(data = summary.means[summary.means$Analysis=="Fungicide Class"|
                              summary.means$Moderator=="OVERALL",], 
       aes(x = Moderator, y = Mean, colour=Category, shape=Category))+
  geom_pointrange(aes(ymin = LL, ymax = UL), position=pd)+
  geom_hline(aes(yintercept=1), colour="grey")+
  geom_vline(aes(xintercept=1.5), colour="grey")+
  scale_x_discrete(limits=c("OVERALL","Strobilurin", "Triazole", "Triaz_Strob"), 
                   labels=c("Overall","Strobilurin", "Triazole", "Mixed Triazole\n& Strobilurin"))+
  ylab("Mean Effect Size (95% C.I.)")+
  xlab("Fungicide Class")+
  theme_tufte()+
  theme(legend.position = "none")

#' Disease Pressure
ggplot(data = summary.means[summary.means$Analysis=="Disease Pressure"|
                              summary.means$Moderator=="OVERALL",], 
       aes(x = Moderator, y = Mean, colour=Category, shape=Category))+
  geom_pointrange(aes(ymin = LL, ymax = UL), position=pd)+
  geom_hline(aes(yintercept=1), colour="grey")+
  geom_vline(aes(xintercept=1.5), colour="grey")+
  scale_x_discrete(limits=c("OVERALL","low","medium","high"), 
                   labels=c("Overall","Low","Medium","High"))+
  ylab("Mean Effect Size (95% C.I.)")+
  xlab("Disease Pressure")+
  theme_tufte()+
  theme(legend.position = "none")

#' Growth Stage
ggplot(data = summary.means[summary.means$Analysis=="Growth Stage"|
                              summary.means$Moderator=="OVERALL",], 
       aes(x = Moderator, y = Mean, colour=Category, shape=Category))+
  geom_pointrange(aes(ymin = LL, ymax = UL), position=pd)+
  geom_hline(aes(yintercept=1), colour="grey")+
  geom_vline(aes(xintercept=1.5), colour="grey")+
  scale_x_discrete(limits=c("OVERALL","R1+","R2+","R3","R5"), 
                   labels=c("Overall","R1+","R2+","R3","R5"))+
  ylab("Mean Effect Size (95% C.I.)")+
  xlab("Growth Stage")+
  theme_tufte()+
  theme(legend.position = "none")

#' Number of Applications
ggplot(data = summary.means[summary.means$Analysis=="Applications"|
                              summary.means$Moderator=="OVERALL",], 
       aes(x = Moderator, y = Mean, colour=Category, shape=Category))+
  geom_pointrange(aes(ymin = LL, ymax = UL), position=pd)+
  geom_hline(aes(yintercept=1), colour="grey")+
  geom_vline(aes(xintercept=1.5), colour="grey")+
  scale_x_discrete(limits=c("OVERALL","1 Application","2 Applications"), 
                   labels=c("OVERALL","1 Application","2 Applications"))+
  ylab("Mean Effect Size (95% C.I.)")+
  xlab("Applications")+
  theme_tufte()+
  theme(legend.position = "none")

#' Study Year
ggplot(data = summary.means[summary.means$Analysis=="Study Year"|
                              summary.means$Moderator=="OVERALL",], 
       aes(x = Moderator, y = Mean, colour=Category, shape=Category))+
  geom_pointrange(aes(ymin = LL, ymax = UL), position=pd)+
  geom_hline(aes(yintercept=1), colour="grey")+
  geom_vline(aes(xintercept=1.5), colour="grey")+
  scale_x_discrete(limits=c("OVERALL","2006","2007","2008","2009","2010","2011","2012","2013"), 
                   labels=c("Overall","2006","2007","2008","2009","2010","2011","2012","2013"))+
  ylab("Mean Effect Size (95% C.I.)")+
  xlab("Study Year")+
  theme_tufte()+
  theme(legend.position = "none")



#' ### Footer
#' 
#' Spun with ezspin("programs/results_plotting.R", out_dir="output", fig_dir="figures", keep_md=FALSE)
#' 
#' Session Info:
#+ sessionInfo
devtools::session_info()