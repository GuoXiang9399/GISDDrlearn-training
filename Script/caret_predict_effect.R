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
###############################################################################
#compute acclerate
  cl <- makePSOCKcluster(24)
  registerDoParallel(cl)
###############################################################################
#loading data
  D1.data <- read.dna("Data/GISDD.1.3.2_D1_20231204_L1_E.fas",format = "fasta",as.character = T)
  D2.data <- read.dna("Data/GISDD.1.3.2_D2_20231201_L1_E.fas",format = "fasta",as.character = T)
  D3.data <- read.dna("Data/GISDD.1.3.2_D3_20231201_L1_E.fas",format = "fasta",as.character = T)
  D4.data <- read.dna("Data/GISDD.1.3.2_D4_20231201_L1_E.fas",format = "fasta",as.character = T)
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
  #DENV-1
  Model_rawdata <- as.data.frame(D1.data)
  #############################################################################
  #data preprocess
  Model_rawdata$Accession <- row.names(Model_rawdata)
  Model_rawdata <- left_join(Model_rawdata,GISDD_Subgenotype)
  Model_rawdata <- Model_rawdata[,!(names(Model_rawdata) %in% "Accession")]
  Model_rawdata <- subset(Model_rawdata,Subgenotype!="DENV1_TBD")
  Model_rawdata <- subset(Model_rawdata,Subgenotype!="DENV1_NA")
  #data splict
  n<-nrow(Model_rawdata)
  ntrain<-round(n*0.6)
  set.seed(333)
  tindex<-sample(n,ntrain)
  train.Model_rawdata<-Model_rawdata[tindex,]
  test.Model_rawdata<-Model_rawdata[-tindex,]
  #random forest
  trControl <- trainControl(method = "cv", number = 10,classProbs=T)
  rfGrid <- expand.grid(splitrule = c( "gini", "extratrees", "hellinger"))
  #random forest
  rffit_GISDD.1.3.2_D1_Subgenotype <- train(
    x = train.Model_rawdata[,-1486],
    y = train.Model_rawdata$Subgenotype,
    method = "ranger",
    trControl = trControl,
    verbose = FALSE)
  save(rffit_GISDD.1.3.2_D1_Subgenotype,
       file = "Result/rffit_GISDD.1.3.2_D1_Subgenotype.rda")
  #############################################################################
  #data preprocess
  Model_rawdata$Accession <- row.names(Model_rawdata)
  Model_rawdata <- left_join(Model_rawdata,GISDD_Clade)
  Model_rawdata <- Model_rawdata[,!(names(Model_rawdata) %in% "Accession")]
  Model_rawdata <- subset(Model_rawdata,Clade!="DENV1_TBD")
  Model_rawdata <- subset(Model_rawdata,Clade!="DENV1_NA")
  #data splict
  n<-nrow(Model_rawdata)
  ntrain<-round(n*0.6)
  set.seed(333)
  tindex<-sample(n,ntrain)
  train.Model_rawdata<-Model_rawdata[tindex,]
  test.Model_rawdata<-Model_rawdata[-tindex,]
  #random forest
  trControl <- trainControl(method = "cv", number = 10,classProbs=T)
  rfGrid <- expand.grid(splitrule = c( "gini", "extratrees", "hellinger"))
  #random forest
  rffit_GISDD.1.3.2_D1_Clade <- train(
    x = train.Model_rawdata[,-1486],
    y = train.Model_rawdata$Clade,
    method = "ranger",
    trControl = trControl,
    verbose = FALSE)
  save(rffit_GISDD.1.3.2_D1_Clade,
       file = "Result/rffit_GISDD.1.3.2_D1_Clade.rda")
  
  rf.pred.prob <- predict(rf.model, newdata=Test)
  并绘制ROC曲线检验效果
  par(las=1, cex.axis=.8)
  rf.roc <- roc(
    y ~ pred,
    data=data.frame(y=Test$结局, pred=rf.pred.prob),
    plot=T, ci=T, main="ROC Curve of Random Forest",
    print.auc=T, print.auc.cex=.8,
    print.thres=T, print.thres.cex=.8,
    auc.polygon=T, max.auc.polygon=T, grid=T)
  
  test.Model_rawdata$predic <- predict(
    rffit_GISDD.1.3.2_D1_Subgenotype,
    newdata=test.Model_rawdata)
  preresult <-data.frame(table(test.Model_rawdata$predic,test.Model_rawdata$Subgenotype))  
  
  
  