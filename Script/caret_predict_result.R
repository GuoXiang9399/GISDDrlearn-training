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
  GISDD <- read_excel("Data/GISDD.version.1.3.2.xlsx")
  GISDD <- unite(GISDD,Virus_Type,Subgenotype,col="Subgenotype",sep="_",remove=F)
  GISDD <- unite(GISDD,Virus_Type,Clade,col="Clade",sep="_",remove=F)
  GISDD_Subgenotype <- GISDD[,c("Accession","Subgenotype")]
  GISDD_Clade <- GISDD[,c("Accession","Clade")]
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
    rffit_GISDD.1.3.2_D1_Subgenotype,
    newdata=test.Model_rawdata)
  test.Model_rawdata$Subgenotype <- as.factor(test.Model_rawdata$Subgenotype)
  test.Model_rawdata$predic <- as.factor(test.Model_rawdata$predic)
###############################################################################
#data
  
  roc(converted_data$Subgenotype, converted_data$predic)

  roc(test.Model_rawdata$Subgenotype,test.Model_rawdata$predic)
  multiclass.roc(test.Model_rawdata$Subgenotype,test.Model_rawdata$predic)
  
  roc1 <-  multiclass.roc(test.Model_rawdata$Subgenotype,  
                 test.Model_rawdata$predic)
  ggroc(roc1)
  

  confusionMatrixTest <- confusionMatrix(test.Model_rawdata$Subgenotype,  
                  test.Model_rawdata$predic)

  precision(test.Model_rawdata, Subgenotype, predic, estimator = "micro")
  
  
  rf_pred <- data.frame(Subgenotype=test.Model_rawdata$Subgenotype,  
                        predic=test.Model_rawdata$predic)
  
  colnames(rf_pred) <- paste(colnames(rf_pred), "_pred_RF")
  
  roc_res <- multi_pr(rf_pred, force_diag = T)
  plot_roc_data(roc_res)

  

  preresult <-data.frame(table(test.Model_rawdata$predic,test.Model_rawdata$Subgenotype))  
  
  
  roc_auc(data = test.Model_rawdata,truth = Subgenotype,1:1485)
  
  
  rf.pred.prob <- predict(rffit_GISDD.1.3.2_D1_Subgenotype.rda, 
                          newdata=test.Model_rawdata)

  
  
  
  
  