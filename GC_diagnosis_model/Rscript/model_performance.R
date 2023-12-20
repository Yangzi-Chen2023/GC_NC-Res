library(Rtsne)
library(openxlsx)
library(tidyverse)

rm(list=ls())
setwd("../")
########## model performance for test dataset ##########
test<-read.xlsx("Data/pred_score_for_test.xlsx",sheet = 1,rowNames = T,check.names=F)
# Patient's clinical information.
cohort_information<-read.xlsx("Data/cohort_information.xlsx",sheet = 1)
cohort_information<-cohort_information%>%dplyr::select(sample_id,stage)

patients<-test%>%inner_join(cohort_information,by = "sample_id")
normal<-test%>%filter(state==0)%>%mutate(stage="N")
test<-rbind(patients,normal)

# PCA analysis
df.PCA<-test[,1:147]
PCA_results<-prcomp(df.PCA,center = TRUE,scale=TRUE)
summ<-summary(PCA_results)
df1<-PCA_results$x
df1<-data.frame(df1,state=test$state,sample_id=test$sample_id)
df1$state<-as.factor(as.integer(df1$state))
xlab<-paste0("PC1(",round(summ$importance[2,1]*100,2),"%)")
ylab<-paste0("PC2(",round(summ$importance[2,2]*100,2),"%)")
p2<-ggplot(data=df1,aes(x=PC1,y=PC2,color=state))+
  stat_ellipse(aes(fill=state),
               type = "norm", geom ="polygon",alpha=0.2,color=NA)+
  geom_point()+labs(x=xlab,y=ylab,color="")+
  guides(fill="none")+
  theme_bw()+
  theme(panel.grid=element_blank())

pdf('Figures/Model_performance/PCA_Test.pdf',width=6.31,height=3.96)
p2
dev.off()

df2<-df1%>%dplyr::select(PC1,PC2,state=state,sample_id)
df3<-test%>%dplyr::select(sample_id,pred_score,stage)
df4<-merge(df2,df3,by='sample_id')

# Model performance validation plot
model_performance_prediction<-df4%>%dplyr::select(pred_score,PC1,state,sample_id,stage)
model_performance_prediction$state<-as.integer(as.character(model_performance_prediction$state))
model_performance_prediction<-model_performance_prediction%>%mutate(clinical_state=ifelse(state==1,"GC","Healthy"))
model_performance_prediction<-model_performance_prediction%>%dplyr::select(-state)

ylab=paste0("PC1(",round(summ$importance[2,1]*100,2),"%)")
xlab="predicted value for GC"

polt_model_performance_prediction_all_stage<-ggplot(data=model_performance_prediction,aes(x=pred_score,y=-PC1,color=clinical_state))+
  geom_point(size=9)+
  theme_bw()+
  theme(panel.grid=element_blank())+
  labs(x=xlab,y=ylab,color="")+
  geom_vline(xintercept=c(0.5), linetype="dotted")+
  theme(axis.title.x = element_text(size = 16, hjust = 0.5, face = "plain"),
        axis.title.y = element_text(size = 16, face = "plain"), 
        axis.text.y = element_text(size = 16, face = "plain", colour = "black"),
        axis.text.x = element_text(size = 16, face = "plain", colour = "black"))+
  theme(legend.text = element_text(size = 16), 
        legend.title = element_text(size = 16), 
        legend.spacing.y = unit(0.2, 'cm'), 
        legend.key.height = unit(0.3, 'cm'), legend.key.width = unit(0.3, 'cm'))+
  scale_x_continuous(limits = c(0, 1))

pdf('Figures/Model_performance/Model_performance_prediction_test_all_stage.pdf',width=9,height=6)
polt_model_performance_prediction_all_stage
dev.off()

########## model performance for test dataset  END ##########

########## model performance for external test dataset   ##########
rm(list=ls())
external_test<-read.xlsx("Data/pred_score_for_external_test.xlsx",sheet = 1,rowNames = T,check.names=F)
cohort_information<-read.xlsx("Data/cohort_information.xlsx",sheet = 1)
cohort_information<-cohort_information%>%dplyr::select(sample_id,stage)

patients<-external_test%>%inner_join(cohort_information,by = "sample_id")
normal<-external_test%>%filter(state==0)%>%mutate(stage="N")
external_test<-rbind(patients,normal)

# PCA analysis
df.PCA<-external_test[,1:147]
PCA_results<-prcomp(df.PCA,center = TRUE,scale=TRUE)
summ<-summary(PCA_results)
df1<-PCA_results$x
df1<-data.frame(df1,state=external_test$state,sample_id=external_test$sample_id)
df1$state<-as.factor(as.integer(df1$state))
xlab<-paste0("PC1(",round(summ$importance[2,1]*100,2),"%)")
ylab<-paste0("PC2(",round(summ$importance[2,2]*100,2),"%)")
p2<-ggplot(data=df1,aes(x=PC1,y=PC2,color=state))+
  stat_ellipse(aes(fill=state),
               type = "norm", geom ="polygon",alpha=0.2,color=NA)+
  geom_point()+labs(x=xlab,y=ylab,color="")+
  guides(fill="none")+
  theme_bw()+
  theme(panel.grid=element_blank())


pdf('Figures/Model_performance/PCA_External_test.pdf',width=6.31,height=3.96)
p2
dev.off()
df2<-df1%>%dplyr::select(PC1,PC2,state=state,sample_id)

df3<-external_test%>%dplyr::select(sample_id,pred_score,stage)
df4<-merge(df2,df3,by='sample_id')

model_performance_prediction<-df4%>%dplyr::select(pred_score,PC1,state,sample_id,stage)
model_performance_prediction$state<-as.integer(as.character(model_performance_prediction$state))
model_performance_prediction<-model_performance_prediction%>%mutate(clinical_state=ifelse(state==1,"GC","Healthy"))
model_performance_prediction<-model_performance_prediction%>%dplyr::select(-state)

ylab=paste0("PC1(",round(summ$importance[2,1]*100,2),"%)")
xlab="predicted value for GC"

polt_model_performance_prediction_all_stage<-ggplot(data=model_performance_prediction,aes(x=pred_score,y=PC1,color=clinical_state))+
  geom_point(size=9)+
  theme_bw()+
  theme(panel.grid=element_blank())+
  labs(x=xlab,y=ylab,color="")+
  geom_vline(xintercept=c(0.5), linetype="dotted")+
  theme(axis.title.x = element_text(size = 16, hjust = 0.5, face = "plain"),
        axis.title.y = element_text(size = 16, face = "plain"), 
        axis.text.y = element_text(size = 16, face = "plain", colour = "black"),
        axis.text.x = element_text(size = 16, face = "plain", colour = "black"))+
  theme(legend.text = element_text(size = 16), 
        legend.title = element_text(size = 16), 
        legend.spacing.y = unit(0.2, 'cm'), 
        legend.key.height = unit(0.3, 'cm'), legend.key.width = unit(0.3, 'cm'))+
  scale_x_continuous(limits = c(0, 1))

pdf('Figures/Model_performance/Model_performance_prediction_external_test_all_stage.pdf',width=9,height=6)
polt_model_performance_prediction_all_stage
dev.off()
