#' # Data analysis using bootstrapping
#' 
#' 100-seed-weight as dependent variable
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
set.seed(18837)

#' Document settings
#+ settings
opts_chunk$set(fig.width = 6, fig.height = 4)

#' ### Load data
#+ loadData
load(file="data/output_data/data_cleaned.R")
remove(list=c(#"seedwt.data.ROM",
              "cerco.data.ROM",
              "yield.data.ROM",
              "target.spot.data.ROM",
              "rust.data.ROM"
              ))

#' Number of bootstrap simulations: 
nsims <- 5000

#' Empty data frame to hold results
results.seedwt <- as.data.frame(matrix(NA,ncol=17,nrow=nsims))
colnames(results.seedwt) <- c("OVERALL","tau2",
                            "FLUT","MIXED","TEBU",
                            "Strobilurin","Triaz_Strob","Triazole",
                            "R3",
                            "Application Intrcpt","Application Slope",
                            "1 Application","2 Applications",
                            "high","medium","low",
                            "2006")

#' Running the bootstraps
#+ boots, warning=FALSE
for(ii in 1:nsims){
  studyIDS <- data.frame(ReferenceNumb = sample(seedwt.data.ROM$ReferenceNumb, 
                                                length(unique(seedwt.data.ROM$ReferenceNumb)),
                                                replace=T))
  newdata <- merge(studyIDS, seedwt.data.ROM)
  # Overall analysis
  meta <- rma.uni(yi = yi,
                  vi = (n1i + n2i)/(n1i*n2i),
                  data = newdata,
                  method = "REML")
  results.seedwt$OVERALL[ii] <- meta$b
  results.seedwt$tau2[ii] <- meta$tau2
  
  # Active Ingredients
  ai <- rma.uni(yi = yi,
                vi = (n1i + n2i)/(n1i*n2i),
                data = newdata,
                method = "REML",
                mods = ~category_ai-1)
  results.seedwt$FLUT[ii] <- ai$b[rownames(ai$b)=="category_aiFLUT"]
  results.seedwt$MIXED[ii] <- ai$b[rownames(ai$b)=="category_aiMIXED"]
  results.seedwt$TEBU[ii] <- ai$b[rownames(ai$b)=="category_aiTEBU"]
  
  # Classes
  class <- rma.uni(yi = yi,
                   vi = (n1i + n2i)/(n1i*n2i),
                   data = newdata,
                   method = "REML",
                   mods = ~category_class-1)
  results.seedwt$Strobilurin[ii] <- 
    class$b[rownames(class$b)=="category_classstrobilurin"]
  results.seedwt$Triaz_Strob[ii] <- 
    class$b[rownames(class$b)=="category_classtriaz + strob"]
  results.seedwt$Triazole[ii] <- 
    class$b[rownames(class$b)=="category_classtriazole"]
  
  # Growth stage
  if(nrow(newdata[!is.na(newdata$category_rstage),])>0){
    rstage <- rma.uni(yi = yi,
                      vi = (n1i + n2i)/(n1i*n2i),
                      data = newdata[! is.na(newdata$category_rstage),],
                      method = "REML")
    results.seedwt$R3[ii] <- rstage$b
  }

  # Disease pressure
  pressure <- rma.uni(yi = yi,
                      vi = (n1i+n2i)/(n1i*n2i),
                      data = newdata,
                      method = "REML",
                      mods = ~category_pressure-1)
  if("low" %in% unique(newdata$category_pressure)){
    results.seedwt$low[ii] <- 
      pressure$b[rownames(pressure$b)=="category_pressurelow"]
  }
  if("medium" %in% unique(newdata$category_pressure)){
    results.seedwt$medium[ii] <- 
      pressure$b[rownames(pressure$b)=="category_pressuremedium"]
  }
  results.seedwt$high[ii] <- pressure$b[rownames(pressure$b)=="category_pressurehigh"]
  
  
  # Number of applications
  applications <- rma.uni(yi = yi,
                          vi = (n1i + n2i)/(n1i*n2i),
                          data = newdata,
                          method = "REML",
                          mods = ~number_applications)
  results.seedwt$`Application Intrcpt`[ii] <- 
    applications$b[rownames(applications$b)=="intrcpt"]
  results.seedwt$`Application Slope`[ii] <- 
    applications$b[rownames(applications$b)=="number_applications"]
  
  # Number of applications as category
  apps.categ <- rma.uni(yi = yi,
                        vi = (n1i + n2i)/(n1i*n2i),
                        data = newdata,
                        method = "REML",
                        mods = ~as.character(number_applications)-1)
  results.seedwt$`1 Application`[ii] <- 
    apps.categ$b[rownames(apps.categ$b)=="as.character(number_applications)1"]
  results.seedwt$`2 Applications`[ii] <- 
    apps.categ$b[rownames(apps.categ$b)=="as.character(number_applications)2"]
  
  # Study year
  # Condition on year1 = 2005
  year <- rma.uni(yi = yi,
                  vi = (n1i+n2i)/(n1i*n2i),
                  data = newdata[newdata$category_year==2006,],
                  method = "REML")
  results.seedwt$`2006`[ii] <- year$b
}


#' ### Save final results
save(results.seedwt, file="data/output_data/results_seedwt.R")

#' ### Footer
#' 
#' Spun with ezspin("programs/data_analysis_bootstrap_seedwt.R", out_dir="output", fig_dir="figures", keep_md=FALSE)
#' 
#' Session Info:
#+ sessionInfo
devtools::session_info()