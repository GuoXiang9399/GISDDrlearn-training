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
load("Result/rffit_GISDD.1.3.2_D3_Clade.rda")
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
  

 Test <- predict(
    rffit_GISDD.1.3.2_D3_Clade,
    newdata=test.Model_rawdata[,-1480])
  
  preresult <-data.frame(table(test.Model_rawdata$predic,test.Model_rawdata$Clade))  
  
  model1 <- rffit_GISDD.1.3.2_D3_Clade
  dfraw <- read.dna("Data/DENV3.test2.fas",format = "fasta",as.character = T)
  df <- as.data.frame(dfraw, row.names = NULL) 
  predictions1 <- predict(model1, df) 
  predictionResult1 <- data.frame(
    Sequence=labels.DNAbin(dfraw),
    Pred_Subgenotype=predictions1,
    model=c("rffit_GISDD.1.3.2_Subgenotype"))