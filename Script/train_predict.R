###############################################################################
###############################################################################
###############################################################################
#loading package
  library(readxl)
  library(rhierbaps)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(rlist)
  library(ape)
  library(caret)
  library(rpart.plot)
  library(modeldata)
  library(doParallel)
###############################################################################
###############################################################################
###############################################################################
#DENV-1
  ggplot(rffit_GISDD.1.3.2_D1_Subgenotype)+
    theme_bw()+theme(legend.position = "top")
  test.Model_rawdata$predic <- predict(
    rffit_GISDD.1.3.2_D1_Subgenotype,
    newdata=test.Model_rawdata)
  preresult <-data.frame(table(test.Model_rawdata$predic,test.Model_rawdata$Subgenotype))  
  ggplot(rffit_GISDD.1.3.2_D1_Clade)+
    theme_bw()+theme(legend.position = "top")
  test.Model_rawdata$predic <- predict(
    rffit_GISDD.1.3.2_D1_Clade,
    newdata=test.Model_rawdata)
  preresult <-data.frame(table(test.Model_rawdata$predic,test.Model_rawdata$Clade))  
###############################################################################
#DENV-2
  ggplot(rffit_GISDD.1.3.2_D2_Subgenotype)+
    theme_bw()+theme(legend.position = "top")
  test.Model_rawdata$predic <- predict(
    rffit_GISDD.1.3.2_D2_Subgenotype,
    newdata=test.Model_rawdata)
  preresult <-data.frame(table(test.Model_rawdata$predic,test.Model_rawdata$Subgenotype))  
###############################################################################
#DENV-3
  ggplot(rffit_GISDD.1.3.2_D3_Subgenotype)+
    theme_bw()+theme(legend.position = "top")
  test.Model_rawdata$predic <- predict(
    rffit_GISDD.1.3.2_D3_Subgenotype,
    newdata=test.Model_rawdata)
  preresult <-data.frame(table(test.Model_rawdata$predic,test.Model_rawdata$Subgenotype))  
  

