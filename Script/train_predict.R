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
#  
  MyTheme <- theme(
   legend.position = "top",
   axis.title.y  = element_text(size=6),
   axis.title.x = element_blank(),
   axis.text.y = element_text(family="sans",size=5),
   axis.text.x = element_text(family="sans",size=5),
   axis.line = element_line(size=0.25,color="gray20"),
   axis.ticks = element_line(size=0.25),
   axis.ticks.length.x = unit(0.1,"lines"),
   axis.ticks.length.y = unit(0.1,"lines"),
   axis.ticks.y.right = element_line(size=0.1),
   legend.title = element_text(size=6),
   legend.text = element_text(size=6),
   panel.background = element_rect(size=0.1,fill=NA) 
  )
#
  p1 <- ggplot(rffit_GISDD.1.3.2_D1_Subgenotype)+
    scale_x_continuous(breaks = c(seq(0,1500,by=300)))+
    scale_y_continuous(breaks = c(seq(0,1,by=0.1)))+
    scale_color_manual(values=c("#EB1B22","#1A75BB"))+
    theme_bw()+MyTheme
  p2 <- ggplot(rffit_GISDD.1.3.2_D1_Clade)+
    scale_x_continuous(breaks = c(seq(0,1500,by=300)))+
    scale_y_continuous(breaks = c(seq(0,1,by=0.1)))+
    scale_color_manual(values=c("#EB1B22","#1A75BB"))+
    theme_bw()+MyTheme
  p3 <- ggplot(rffit_GISDD.1.3.2_D2_Subgenotype)+
    scale_x_continuous(breaks = c(seq(0,1500,by=300)))+
    scale_y_continuous(breaks = c(seq(0,1,by=0.1)))+
    scale_color_manual(values=c("#EB1B22","#1A75BB"))+
    theme_bw()+MyTheme
  p4 <- ggplot(rffit_GISDD.1.3.2_D2_Clade)+
    scale_x_continuous(breaks = c(seq(0,1500,by=300)))+
    scale_y_continuous(breaks = c(seq(0,1,by=0.1)))+
    scale_color_manual(values=c("#EB1B22","#1A75BB"))+
    theme_bw()+MyTheme
  p5 <- ggplot(rffit_GISDD.1.3.2_D3_Subgenotype)+
    scale_x_continuous(breaks = c(seq(0,1500,by=300)))+
    scale_y_continuous(breaks = c(seq(0,1,by=0.1)))+
    scale_color_manual(values=c("#EB1B22","#1A75BB"))+
    theme_bw()+MyTheme
  p6 <- ggplot(rffit_GISDD.1.3.2_D3_Clade)+
    scale_x_continuous(breaks = c(seq(0,1500,by=300)))+
    scale_y_continuous(breaks = c(seq(0,1,by=0.1)))+
    scale_color_manual(values=c("#EB1B22","#1A75BB"))+
    theme_bw()+MyTheme
  p7 <- ggplot(rffit_GISDD.1.3.2_D4_Subgenotype)+
    scale_x_continuous(breaks = c(seq(0,1500,by=300)))+
    scale_y_continuous(breaks = c(seq(0,1,by=0.1)))+
    scale_color_manual(values=c("#EB1B22","#1A75BB"))+
    theme_bw()+MyTheme
  p8 <- ggplot(rffit_GISDD.1.3.2_D4_Clade)+
    scale_x_continuous(breaks = c(seq(0,1500,by=300)))+
    scale_y_continuous(breaks = c(seq(0,1,by=0.1)))+
    scale_color_manual(values=c("#EB1B22","#1A75BB"))+
    theme_bw()+MyTheme
  plot_grid(p1,p2,p3,p4,p5,p6,p7,p8,ncol=4)
  ggsave("Plot/recall.pdf",width=17,height=11,units="cm")
  
  
  