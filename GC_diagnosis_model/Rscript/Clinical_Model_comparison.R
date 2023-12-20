# Comparison with clinical indicators such as CA199, CA724, CEA etc.
library(tidyverse)
library(readxl)
library(openxlsx)

rm(list=ls())
setwd("../")
Biomarker_clinical<-read.xlsx("Data/Biomarker_clinical.xlsx",sheet = "Sheet1")
predict_risk_score<-read.xlsx("Data/Biomarker_clinical.xlsx",sheet = "Sheet2")
Biomarker_clinical<-merge(Biomarker_clinical,predict_risk_score,by="orinumber")

Biomarker_clinical<-Biomarker_clinical%>%select(orinumber,CA199=`CA199（0-27）`,CA724=`CA724（0-6.9）`,CEA=`CEA,（0-5）`,cohort,pred_score,state)
Biomarker_clinical$CA199<-as.numeric(Biomarker_clinical$CA199)
Biomarker_clinical$CA724<-as.numeric(Biomarker_clinical$CA724)
Biomarker_clinical$CEA<-as.numeric(Biomarker_clinical$CEA)

CA199<-Biomarker_clinical%>%select(orinumber,CA199,pred_score,state)%>%filter(CA199!=0)
CA724<-Biomarker_clinical%>%select(orinumber,CA724,pred_score,state)%>%filter(CA724!=0)
CEA<-Biomarker_clinical%>%select(orinumber,CEA,pred_score,state)%>%filter(CEA!=0)

# CA199(0-27)
plot_CA199<-ggplot(data=CA199,aes(x=log2(CA199),y=pred_score,color=state))+
  geom_point(size=12,alpha=0.8)+
  theme_bw()+
  ggtitle("Comparison of Prediction Accuracy Between 
          CA199 and RF model")+
  theme(panel.grid=element_blank())+
  labs(x="log2(CA199)",y="Metabolite Biomarkers Risk Score",color="")+
  geom_vline(xintercept=c(log2(27)), linetype="dashed",color="blue")+
  geom_hline(yintercept=0.5, linetype="dashed",color="red")+
  theme(plot.title = element_text(hjust = 0.5,size = 24, face = "bold"),
        axis.title.x = element_text(size = 16, hjust = 0.5, face = "bold"),
        axis.title.y = element_text(size = 16, face = "bold"), 
        axis.text.y = element_text(size = 16, face = "bold", colour = "black"),
        axis.text.x = element_text(size = 16, face = "bold", colour = "black"),)+
  theme(legend.position = "none",
        legend.text = element_text(size = 16), 
        legend.title = element_text(size = 16), 
        legend.spacing.y = unit(0.2, 'cm'), 
        legend.key.height = unit(0.3, 'cm'), legend.key.width = unit(0.3, 'cm'))+
  scale_y_continuous(breaks = seq(0,1,0.1),limits = c(0,1))+
  scale_x_continuous(breaks = seq(0,10,5))

pdf("Figures/Clinical_Model_comparison//Comparison of Prediction Accuracy Between CA199 and RF Model.pdf",width=9,height = 8)
plot_CA199
dev.off()

# CA724(0-6.9);
plot_CA724<-ggplot(data=CA724,aes(x=log2(CA724),y=pred_score,color=state))+
  geom_point(size=12,alpha=0.8)+
  theme_bw()+
  ggtitle("Comparison of Prediction Accuracy Between 
          CA724 and RF model")+
  theme(panel.grid=element_blank())+
  labs(x="log2(CA724)",y="Metabolite Biomarkers Risk Score",color="")+
  geom_vline(xintercept=c(log2(6.9)), linetype="dashed",color="blue")+
  geom_hline(yintercept=0.5, linetype="dashed",color="red")+
  theme(plot.title = element_text(hjust = 0.5,size = 24, face = "bold"),
        axis.title.x = element_text(size = 16, hjust = 0.5, face = "bold"),
        axis.title.y = element_text(size = 16, face = "bold"), 
        axis.text.y = element_text(size = 16, face = "bold", colour = "black"),
        axis.text.x = element_text(size = 16, face = "bold", colour = "black"),)+
  theme(legend.position = "none",
        legend.text = element_text(size = 16), 
        legend.title = element_text(size = 16), 
        legend.spacing.y = unit(0.2, 'cm'), 
        legend.key.height = unit(0.3, 'cm'), legend.key.width = unit(0.3, 'cm'))+
  scale_y_continuous(breaks = seq(0,1,0.1),limits = c(0,1))+
  scale_x_continuous(breaks = seq(0,10,5))

pdf("Figures/Clinical_Model_comparison/Comparison of Prediction Accuracy Between CA724 and RF Model.pdf",width=9,height = 8)
plot_CA724
dev.off()

# CEA(0-5);
plot_CEA<-ggplot(data=CEA,aes(x=log2(CEA),y=pred_score,color=state))+
  geom_point(size=12,alpha=0.8)+
  theme_bw()+
  ggtitle("Comparison of Prediction Accuracy Between 
          CEA and RF model")+
  theme(panel.grid=element_blank())+
  labs(x="log2(CEA)",y="Metabolite Biomarkers Risk Score",color="")+
  geom_vline(xintercept=c(log2(5)), linetype="dashed",color="blue")+
  geom_hline(yintercept=0.5, linetype="dashed",color="red")+
  theme(plot.title = element_text(hjust = 0.5,size = 24, face = "bold"),
        axis.title.x = element_text(size = 16, hjust = 0.5, face = "bold"),
        axis.title.y = element_text(size = 16, face = "bold"), 
        axis.text.y = element_text(size = 16, face = "bold", colour = "black"),
        axis.text.x = element_text(size = 16, face = "bold", colour = "black"),)+
  theme(legend.position = "none",
        legend.text = element_text(size = 16), 
        legend.title = element_text(size = 16), 
        legend.spacing.y = unit(0.2, 'cm'), 
        legend.key.height = unit(0.3, 'cm'), legend.key.width = unit(0.3, 'cm'))+
  scale_y_continuous(breaks = seq(0,1,0.1),limits = c(0,1))+
  scale_x_continuous(breaks = seq(0,10,5))

pdf("Figures/Clinical_Model_comparison/Comparison of Prediction Accuracy Between CEA and RF Model.pdf",width=9,height = 8)
plot_CEA
dev.off()



