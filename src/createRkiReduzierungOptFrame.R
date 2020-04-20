## script to optimize reduzierungs values


setwd("~/CloudProjectsUnderWork/ProjectsUnderWork/PredCo/CoronaPredict/src")
source(file = "Rechenkern.R")
source(file = "helperForCovid19.R")
load("../data/inputExample.RData")
input <- isolate(reactiveValuesToList(inputExample))
load("../data/createDfBundLandKreisOutput.RData") 
#  loads 
# dataframe RkiDataWithRoNoOpimizedUpToDate 
# from  file createDfBundLandKreisOutput.RData created by 
# cronjob running createDfBundLandKreis.R every day at 0.01am 


load("../data/landkreiseBadenWuerttemberg.RData")

############ define optimization parameters, optFunction and resultColumnName #################
parameter_tibble <- tribble(
  ~var_name,         ~var_value, ~var_min,  ~var_max,  ~var_selected,
  "reduzierung_rt1", 0         ,  0,        60,        "TRUE",
  "reduzierung_rt2", 0         ,  0,        60,        "TRUE",
  "reduzierung_rt3", -20       ,  -40,      30,        "TRUE")
# function to be used to calculate metric for optimizer
optFunction <- calcPredictionsForGaOptimization
resultColumnName <- "reduzierungsOptResult"
gaPara <- list("popSize" = 15, "maxiter" = 4, run = 5)
RkiDataWithRoNoOpimizedUpToDate <- RkiDataWithRoNoOpimizedUpToDate %>% 
  as_tibble() %>% 
  select(!contains("redu")) %>% add_column("reduzierungsOptResult" = list("a"),
                                           "optimizedInput" = list("OptimizedInputValues" = 0)) # %>% filter(whichRegion == "Brandenburg")



################## for tests #########
RkiDataWithRoNoOpimizedUpToDate <- RkiDataWithRoNoOpimizedUpToDate %>%
  filter(whichRegion %in% c("Deutschland", "Baden-Württemberg" , "LK Esslingen"))

#####################################################

RkiDataWithRoNoAndReduzierungOpimized <- appendOpt(RkiDataWithRoNoOpimizedUpToDate, parameter_tibble, optFunction, resultColumnName, gaPara) 
 browser()

save(RkiDataWithRoNoAndReduzierungOpimized, file  = "../data/RkiReduzierungOptFrameDeutschland.RData")
browser()
############### create compare plots  #######################


# load("../data/RkiReduzierungOptFrameServer0416.RData")
plotCreate <- 1
#load("../data/RkiReduzierungOptFrameServerMPEItr500417.RData")


if(plotCreate){
  plot <- createPlotReduOpt( input)
  plot
    
}