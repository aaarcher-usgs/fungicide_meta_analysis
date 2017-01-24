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

#' ### Load data
#+ loadData
load(file="data/output_data/data_cleaned.R")
remove(list=c(#"yield.data.ROM",
              "seedwt.data.ROM",
              "rust.data.ROM"
              ))

#' Number of bootstrap simulations: 
nsims <- 5000

#' Empty data frame to hold results
results.yield <- as.data.frame(matrix(NA,ncol=25,nrow=nsims))
colnames(results.yield) <- c("OVERALL","tau2",
                            "FLUT","MIXED","PYR","TEBU",
                            "Strobilurin","Triaz_Strob","Triazole",
                            "R1+", "R2+", 
                            "R3","R5",
                            "Application Intrcpt","Application Slope",
                            "1 Application", "2 Applications",
                            "high","medium","low",
                            "Year Intrcpt|2004","Year Slope",
                            "2006","2007","2013")

#' Running the bootstraps
#+ boots, warning=FALSE
for(ii in 1:nsims){
  studyIDS <- data.frame(ReferenceNumb = sample(yield.data.ROM$ReferenceNumb, 
                                                length(unique(yield.data.ROM$ReferenceNumb)),
                                                replace=T))
  newdata <- merge(studyIDS, yield.data.ROM)
  # Overall analysis
  meta <- rma.uni(yi = yi,
                  vi = 1/rust.m2i,
                  data = newdata,
                  method = "REML")
  results.yield$OVERALL[ii] <- meta$b
  results.yield$tau2[ii] <- meta$tau2
  
  # Active Ingredients
  ai <- rma.uni(yi = yi,
                vi = 1/rust.m2i,
                data = newdata,
                method = "REML",
                mods = ~category_ai-1)
  results.yield$FLUT[ii] <- ai$b[rownames(ai$b)=="category_aiFLUT"]
  results.yield$MIXED[ii] <- ai$b[rownames(ai$b)=="category_aiMIXED"]
  results.yield$PYR[ii] <- ai$b[rownames(ai$b)=="category_aiPYR"]
  results.yield$TEBU[ii] <- ai$b[rownames(ai$b)=="category_aiTEBU"]
  
  # Classes
  class <- rma.uni(yi = yi,
                   vi = 1/rust.m2i,
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
                    vi = 1/rust.m2i,
                    data = newdata,
                    method = "REML",
                    mods = ~category_rstage-1)
    if("1+" %in% unique(newdata$category_rstage)){
      results.yield$`R1+`[ii] <- 
        rstage$b[rownames(rstage$b)=="category_rstage1+"]
    }
    results.yield$`R2+`[ii] <- 
      rstage$b[rownames(rstage$b)=="category_rstage2+"]
    results.yield$R3[ii] <- 
      rstage$b[rownames(rstage$b)=="category_rstage3"]
    if("5" %in% unique(newdata$category_rstage)){
      results.yield$R5[ii] <- 
        rstage$b[rownames(rstage$b)=="category_rstage5"]
    }

    # Disease pressure
    pressure <- rma.uni(yi = yi,
                        vi = (n1i+n2i)/(n1i*n2i),
                        data = newdata,
                        method = "REML",
                        mods = ~category_pressure-1)
    results.yield$low[ii] <- pressure$b[rownames(pressure$b)=="category_pressurelow"]
    if(nrow(newdata[newdata$category_pressure=="medium",])>0){
      results.yield$medium[ii] <- pressure$b[rownames(pressure$b)=="category_pressuremedium"]
    }
    results.yield$high[ii] <- pressure$b[rownames(pressure$b)=="category_pressurehigh"]
    
    
  # Number of applications
  applications <- rma.uni(yi = yi,
                          vi = 1/rust.m2i,
                          data = newdata,
                          method = "REML",
                          mods = ~number_applications)
  results.yield$`Application Intrcpt`[ii] <- 
    applications$b[rownames(applications$b)=="intrcpt"]
  results.yield$`Application Slope`[ii] <- 
    applications$b[rownames(applications$b)=="number_applications"]
  
  # Number of applications as category
  apps.categ <- rma.uni(yi = yi,
                        vi = 1/rust.m2i,
                        data = newdata,
                        method = "REML",
                        mods = ~as.character(number_applications)-1)
  results.yield$`1 Application`[ii] <- 
    apps.categ$b[rownames(apps.categ$b)=="as.character(number_applications)1"]
  results.yield$`2 Applications`[ii] <- 
    apps.categ$b[rownames(apps.categ$b)=="as.character(number_applications)2"]
  
  # Study year
  # Condition on year1 = 2005
  if(length(unique(newdata$category_year))>2){
  newdata$category_yearC <- newdata$category_year - 2004
  year <- rma.uni(yi = yi,
                  vi = (n1i+n2i)/(n1i*n2i),
                  data = newdata,
                  method = "REML",
                  mods = ~category_yearC)
  results.yield$`Year Intrcpt|2004`[ii] <- year$b[rownames(year$b)=="intrcpt"]
  results.yield$`Year Slope`[ii] <- year$b[rownames(year$b)=="category_yearC"]
  
  # Study year as category
  year.categ <- rma.uni(yi = yi,
                 vi = (n1i+n2i)/(n1i*n2i),
                 data = newdata,
                 method = "REML",
                 mods = ~as.character(category_year)-1)
  results.yield$`2006`[ii] <- 
    year.categ$b[rownames(year.categ$b)=="as.character(category_year)2006"]
  if("2007" %in% unique(newdata$category_year)){
    results.yield$`2007`[ii] <- 
      year.categ$b[rownames(year.categ$b)=="as.character(category_year)2007"]
  }
  if("2013" %in% unique(newdata$category_year)){
    results.yield$`2013`[ii] <- 
      year.categ$b[rownames(year.categ$b)=="as.character(category_year)2013"]
  }
  }
  if(length(unique(newdata$category_year))==2 &
     unique(newdata$category_year[!is.na(newdata$category_year)]=="2006")){
    year.categ <- rma.uni(yi = yi,
                          vi = (n1i+n2i)/(n1i*n2i),
                          data = newdata[!is.na(newdata$category_year),],
                          method = "REML")
    results.yield$`2006`[ii] <- year.categ$b
  }
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