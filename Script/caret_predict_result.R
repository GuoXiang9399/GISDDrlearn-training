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
  library(ggplot2)
  library(cowplot)
  library(pROC)
  library(yardstick)
  library(multiROC)
###############################################################################
#compute acclerate
  cl <- makePSOCKcluster(24)
  registerDoParallel(cl)
###############################################################################
  load("Result/rffit_GISDD.1.3.2_D1_Subgenotype.rda")
  load("Result/rffit_GISDD.1.3.2_D1_Clade.rda")
  load("Result/rffit_GISDD.1.3.2_D2_Subgenotype.rda")
  load("Result/rffit_GISDD.1.3.2_D2_Clade.rda")
  load("Result/rffit_GISDD.1.3.2_D3_Subgenotype.rda")
  load("Result/rffit_GISDD.1.3.2_D3_Clade.rda")
  load("Result/rffit_GISDD.1.3.2_D4_Subgenotype.rda")
  load("Result/rffit_GISDD.1.3.2_D4_Clade.rda")
###############################################################################
#data
  load("Result/rffit_test_rawdata_D1_Subgenotype.rda")
  test.Model_rawdata$predic <- predict(
    rffit_GISDD.1.3.2_D1_Subgenotype,newdata=test.Model_rawdata)
#data
  load("Result/rffit_test_rawdata_D2_Subgenotype.rda")
  test.Model_rawdata$predic <- predict(
    rffit_GISDD.1.3.2_D2_Subgenotype,newdata=test.Model_rawdata)
#data
  load("Result/rffit_test_rawdata_D3_Subgenotype.rda")
  test.Model_rawdata$predic <- predict(
    rffit_GISDD.1.3.2_D3_Subgenotype,newdata=test.Model_rawdata)
#data
  load("Result/rffit_test_rawdata_D4_Subgenotype.rda")
  test.Model_rawdata$predic <- predict(
    rffit_GISDD.1.3.2_D4_Subgenotype,newdata=test.Model_rawdata)
#data
  load("Result/rffit_test_rawdata_D1_Clade.rda")
  test.Model_rawdata$predic <- predict(
    rffit_GISDD.1.3.2_D1_Clade,newdata=test.Model_rawdata)
#data
  load("Result/rffit_test_rawdata_D2_Clade.rda")
  test.Model_rawdata$predic <- predict(
    rffit_GISDD.1.3.2_D2_Clade,newdata=test.Model_rawdata)
#data
  load("Result/rffit_test_rawdata_D3_Clade.rda")
  test.Model_rawdata$predic <- predict(
    rffit_GISDD.1.3.2_D3_Clade,newdata=test.Model_rawdata)
#data
  load("Result/rffit_test_rawdata_D4_Clade.rda")
  test.Model_rawdata$predic <- predict(
    rffit_GISDD.1.3.2_D4_Clade,newdata=test.Model_rawdata)
###############################################################################
#data
  test.Model_rawdata$Subgenotype <- factor(test.Model_rawdata$Subgenotype, ordered = TRUE)
  test.Model_rawdata$predic <- factor(test.Model_rawdata$predic, ordered = TRUE)
  test.Model_rawdata$predic <- factor(test.Model_rawdata$predic, 
                                      levels = levels(test.Model_rawdata$Subgenotype))
#comput
  confusionMatrix_Result <- 
    confusionMatrix(test.Model_rawdata$Subgenotype,  
                    test.Model_rawdata$predic)
  confusionMatrix_Result_overall <- data.frame(confusionMatrix_Result$overall)       
  confusionMatrix_Result_overall <- t.data.frame(confusionMatrix_Result_overall)
  confusionMatrix_Result_overall <- data.frame(confusionMatrix_Result_overall)
#data
  confusionMatrix_Result_overall$model <- "D1_Subgenotype"
  confusionMatrix_Result_overall$model <- "D2_Subgenotype"
  confusionMatrix_Result_overall$model <- "D3_Subgenotype"
  confusionMatrix_Result_overall$model <- "D4_Subgenotype"
  confusionMatrix_Result_overall$model <- "D1_Clade"
  confusionMatrix_Result_overall$model <- "D2_Clade"
  confusionMatrix_Result_overall$model <- "D3_Clade"
  confusionMatrix_Result_overall$model <- "D4_Clade"
  
  SummaryData <- data.frame(confusionMatrix_Result$byClass)
  write.csv(SummaryData,"Result/ConMatrix_D2_Subgenotype")
  
#summary  
  confusionMatrix_summary <- confusionMatrix_Result_overall
  confusionMatrix_summary <- 
    rbind(confusionMatrix_summary,confusionMatrix_Result_overall)
  
  
  
  