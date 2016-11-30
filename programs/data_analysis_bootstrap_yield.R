#' # Data analysis using bootstrapping
#' 
#' Yield as dependent variable
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
remove(list=c(#"yield.data.ROM",
              "cerco.data.ROM",
              "seedwt.data.ROM",
              "target.spot.data.ROM",
              "rust.data.ROM"
              ))

#' Number of bootstrap simulations: 
nsims <- 5000

#' Empty data frame to hold results
results.yield <- as.data.frame(matrix(NA,ncol=14,nrow=nsims))
colnames(results.yield) <- c("OVERALL",
                            "FLUT","MIXED","PYR","TEBU",
                            "Strobilurin","Triaz_Strob","Triazole",
                            "R1+", "R2+", 
                            "R3","R5",
                            "Application Intrcpt","Application Slope")

#' Running the bootstraps
#+ boots, warning=FALSE
for(ii in 1:nsims){
  studyIDS <- data.frame(ReferenceNumb = sample(yield.data.ROM$ReferenceNumb, 
                                                length(unique(yield.data.ROM$ReferenceNumb)),
                                                replace=T))
  newdata <- merge(studyIDS, yield.data.ROM)
  # Overall analysis
  meta <- rma.uni(yi = yi,
                  vi = (n1i + n2i)/(n1i*n2i),
                  data = newdata,
                  method = "ML")
  results.yield$OVERALL[ii] <- meta$b
  
  # Active Ingredients
  ai <- rma.uni(yi = yi,
                vi = (n1i + n2i)/(n1i*n2i),
                data = newdata,
                method = "REML",
                mods = ~category_ai-1)
  results.yield$FLUT[ii] <- ai$b[rownames(ai$b)=="category_aiFLUT"]
  results.yield$MIXED[ii] <- ai$b[rownames(ai$b)=="category_aiMIXED"]
  results.yield$PYR[ii] <- ai$b[rownames(ai$b)=="category_aiPYR"]
  results.yield$TEBU[ii] <- ai$b[rownames(ai$b)=="category_aiTEBU"]
  
  # Classes
  class <- rma.uni(yi = yi,
                   vi = (n1i + n2i)/(n1i*n2i),
                   data = newdata,
                   method = "REML",
                   mods = ~category_class-1)
  results.yield$Strobilurin[ii] <- 
    class$b[rownames(class$b)=="category_classstrobilurin"]
  results.yield$Triaz_Strob[ii] <- 
    class$b[rownames(class$b)=="category_classtriaz + strob"]
  results.yield$Triazole[ii] <- 
    class$b[rownames(class$b)=="category_classtriazole"]
  
  # Growth stage
  rstage <- rma.uni(yi = yi,
                    vi = (n1i + n2i)/(n1i*n2i),
                    data = newdata,
                    method = "REML",
                    mods = ~category_rstage-1)
  results.yield$`R1+`[ii] <- 
    rstage$b[rownames(rstage$b)=="category_rstage1+"]
  results.yield$`R2+`[ii] <- 
    rstage$b[rownames(rstage$b)=="category_rstage2+"]
  results.yield$R3[ii] <- 
    rstage$b[rownames(rstage$b)=="category_rstage3"]
  results.yield$R5[ii] <- 
    rstage$b[rownames(rstage$b)=="category_rstage5"]
  
  # Number of applications
  applications <- rma.uni(yi = yi,
                          vi = (n1i + n2i)/(n1i*n2i),
                          data = newdata,
                          method = "REML",
                          mods = ~number_applications)
  results.yield$`Application Intrcpt`[ii] <- 
    applications$b[rownames(applications$b)=="intrcpt"]
  results.yield$`Application Slope`[ii] <- 
    applications$b[rownames(applications$b)=="number_applications"]
}


#' ### Save final results
save(results.yield, file="data/output_data/results_yield.R")

#' ### Footer
#' 
#' Spun with ezspin("programs/data_analysis_bootstrap_yield.R", out_dir="output", fig_dir="figures", keep_md=FALSE)
#' 
#' Session Info:
#+ sessionInfo
devtools::session_info()