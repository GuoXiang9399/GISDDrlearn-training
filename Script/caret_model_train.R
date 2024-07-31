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
###############################################################################
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
###############################################################################
###############################################################################
###############################################################################
#DENV-2
  Model_rawdata <- as.data.frame(D2.data)
  #############################################################################
  #data preprocess
    Model_rawdata$Accession <- row.names(Model_rawdata)
    Model_rawdata <- left_join(Model_rawdata,GISDD_Subgenotype)
    Model_rawdata <- Model_rawdata[,!(names(Model_rawdata) %in% "Accession")]
    Model_rawdata <- subset(Model_rawdata,Subgenotype!="DENV2_TBD")
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
    rffit_GISDD.1.3.2_D2_Subgenotype <- train(
      x = train.Model_rawdata[,-1486],
      y = train.Model_rawdata$Subgenotype,
      method = "ranger",
      trControl = trControl,
      verbose = FALSE)
    save(rffit_GISDD.1.3.2_D2_Subgenotype,
         file = "Result/rffit_GISDD.1.3.2_D2_Subgenotype.rda")
  #############################################################################
  #data preprocess
    Model_rawdata$Accession <- row.names(Model_rawdata)
    Model_rawdata <- left_join(Model_rawdata,GISDD_Clade)
    Model_rawdata <- Model_rawdata[,!(names(Model_rawdata) %in% "Accession")]
    Model_rawdata <- subset(Model_rawdata,Clade!="DENV2_TBD")
    Model_rawdata <- subset(Model_rawdata,Clade!="DENV2_NA")
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
    rffit_GISDD.1.3.2_D2_Clade <- train(
      x = train.Model_rawdata[,-1486],
      y = train.Model_rawdata$Clade,
      method = "ranger",
      trControl = trControl,
      verbose = FALSE)
    save(rffit_GISDD.1.3.2_D2_Clade,
         file = "Result/rffit_GISDD.1.3.2_D2_Clade.rda")
###############################################################################
###############################################################################
###############################################################################
#DENV-3
  Model_rawdata <- as.data.frame(D3.data)
  #############################################################################
  #data preprocess
    Model_rawdata$Accession <- row.names(Model_rawdata)
    Model_rawdata <- left_join(Model_rawdata,GISDD_Subgenotype)
    Model_rawdata <- Model_rawdata[,!(names(Model_rawdata) %in% "Accession")]
    Model_rawdata <- subset(Model_rawdata,Subgenotype!="DENV3_TBD")
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
    rffit_GISDD.1.3.2_D3_Subgenotype <- train(
      x = train.Model_rawdata[,-1486],
      y = train.Model_rawdata$Subgenotype,
      method = "ranger",
      trControl = trControl,
      verbose = FALSE)
    save(rffit_GISDD.1.3.2_D3_Subgenotype,
         file = "Result/rffit_GISDD.1.3.2_D3_Subgenotype.rda")
  #############################################################################
  #data preprocess
    Model_rawdata$Accession <- row.names(Model_rawdata)
    Model_rawdata <- left_join(Model_rawdata,GISDD_Clade)
    Model_rawdata <- Model_rawdata[,!(names(Model_rawdata) %in% "Accession")]
    Model_rawdata <- subset(Model_rawdata,Clade!="DENV3_TBD")
    Model_rawdata <- subset(Model_rawdata,Clade!="DENV3_NA")
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
    rffit_GISDD.1.3.2_D3_Clade <- train(
      x = train.Model_rawdata[,-1486],
      y = train.Model_rawdata$Clade,
      method = "ranger",
      trControl = trControl,
      verbose = FALSE)
    save(rffit_GISDD.1.3.2_D3_Clade,
         file = "Result/rffit_GISDD.1.3.2_D3_Clade.rda")
###############################################################################
###############################################################################
###############################################################################
#DENV-4
  Model_rawdata <- as.data.frame(D4.data)
  #############################################################################
  #data preprocess
    Model_rawdata$Accession <- row.names(Model_rawdata)
    Model_rawdata <- left_join(Model_rawdata,GISDD_Subgenotype)
    Model_rawdata <- Model_rawdata[,!(names(Model_rawdata) %in% "Accession")]
    Model_rawdata <- subset(Model_rawdata,Subgenotype!="DENV4_TBD")
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
    rffit_GISDD.1.3.2_D4_Subgenotype <- train(
      x = train.Model_rawdata[,-1486],
      y = train.Model_rawdata$Subgenotype,
      method = "ranger",
      trControl = trControl,
      verbose = FALSE)
    save(rffit_GISDD.1.3.2_D4_Subgenotype,
         file = "Result/rffit_GISDD.1.3.2_D4_Subgenotype.rda")
  #############################################################################
  #data preprocess
    Model_rawdata$Accession <- row.names(Model_rawdata)
    Model_rawdata <- left_join(Model_rawdata,GISDD_Clade)
    Model_rawdata <- Model_rawdata[,!(names(Model_rawdata) %in% "Accession")]
    Model_rawdata <- subset(Model_rawdata,Clade!="DENV4_TBD")
    Model_rawdata <- subset(Model_rawdata,Clade!="DENV4_NA")
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
    rffit_GISDD.1.3.2_D4_Clade <- train(
      x = train.Model_rawdata[,-1486],
      y = train.Model_rawdata$Clade,
      method = "ranger",
      trControl = trControl,
      verbose = FALSE)
    save(rffit_GISDD.1.3.2_D4_Clade,
         file = "Result/rffit_GISDD.1.3.2_D4_Clade.rda")
    
    
    
    
    
