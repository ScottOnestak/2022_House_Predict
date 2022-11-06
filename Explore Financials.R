#Look at fiancial data


#libraries
library(dplyr)
library(ROCR)

options(scipen=999)

#data
train = read.csv("Data/Datasets/train.csv",header=T,stringsAsFactors=F)

thedata = train[,c(1:16,54:65)]
thedata$GOP_win = as.factor(thedata$GOP_win)
thedata$color = ifelse(thedata$GOP_win == 1,"firebrick","deepskyblue")



#look at straight subtraction
s_thedata = thedata %>% mutate(REL_TOTAL_RECEIPTS = TOTAL_RECEIPTS_GOP - TOTAL_RECEIPTS_DEM,
                               REL_TOTAL_DISBURSEMENTS = TOTAL_DISBURSEMENTS_GOP - TOTAL_DISBURSEMENTS_DEM,
                               REL_COH = COH_GOP - COH_DEM,
                               REL_INDV_CONTR = INDIVIDUAL_CONTRIBUTIONS_GOP - INDIVIDUAL_CONTRIBUTIONS_DEM)

##RECIPTS
s_rec = glm(GOP_win ~ REL_TOTAL_RECEIPTS,data=s_thedata,family=binomial)
summary(s_rec)
aic = round(summary(s_rec)[[5]],2)
s_thedata$TOTAL_RECEIPTS_EST = predict(s_rec,s_thedata,type="response")
pred = prediction(s_thedata$TOTAL_RECEIPTS_EST, s_thedata$GOP_win)
perf = performance(pred, measure = "tpr", x.measure = "fpr")
plot(perf)
auc = performance(pred, measure = "auc")
auc = round(auc@y.values[[1]],4)

png(filename = "Plots/Financials/Receipts/Method_1.png",width = 1920,height = 1080)
plot(TOTAL_RECEIPTS_EST ~ REL_TOTAL_RECEIPTS, data=s_thedata, col=s_thedata$color,pch=20)
title(main="Total Receipts - Subtraction",sub=paste("AIC: ",aic," AUC: ",auc,sep=""))
dev.off()

##DISBURSEMENTS
s_dis = glm(GOP_win ~ REL_TOTAL_DISBURSEMENTS,data=s_thedata,family=binomial)
summary(s_rec)
aic = round(summary(s_dis)[[5]],2)
s_thedata$TOTAL_DISBURSEMENTS_EST = predict(s_dis,s_thedata,type="response")
pred = prediction(s_thedata$TOTAL_DISBURSEMENTS_EST, s_thedata$GOP_win)
perf = performance(pred, measure = "tpr", x.measure = "fpr")
plot(perf)
auc = performance(pred, measure = "auc")
auc = round(auc@y.values[[1]],4)

png(filename = "Plots/Financials/Disbursements/Method_1.png",width = 1920,height = 1080)
plot(TOTAL_DISBURSEMENTS_EST ~ REL_TOTAL_DISBURSEMENTS, data=s_thedata, col=s_thedata$color,pch=20)
title(main="Total Disbursements - Subtraction",sub=paste("AIC: ",aic," AUC: ",auc,sep=""))
dev.off()

##COH
s_coh = glm(GOP_win ~ REL_COH,data=s_thedata,family=binomial)
summary(s_rec)
aic = round(summary(s_coh)[[5]],2)
s_thedata$TOTAL_COH_EST = predict(s_coh,s_thedata,type="response")
pred = prediction(s_thedata$TOTAL_COH_EST, s_thedata$GOP_win)
perf = performance(pred, measure = "tpr", x.measure = "fpr")
plot(perf)
auc = performance(pred, measure = "auc")
auc = round(auc@y.values[[1]],4)

png(filename = "Plots/Financials/COH/Method_1.png",width = 1920,height = 1080)
plot(TOTAL_COH_EST ~ REL_COH, data=s_thedata, col=s_thedata$color,pch=20)
title(main="Total COH - Subtraction",sub=paste("AIC: ",aic," AUC: ",auc,sep=""))
dev.off()

##INDIVIDUAL RECEIPTS
s_ind = glm(GOP_win ~ REL_INDV_CONTR,data=s_thedata,family=binomial)
summary(s_ind)
aic = round(summary(s_ind)[[5]],2)
s_thedata$INDV_CONTR_EST = predict(s_ind,s_thedata,type="response")
pred = prediction(s_thedata$INDV_CONTR_EST, s_thedata$GOP_win)
perf = performance(pred, measure = "tpr", x.measure = "fpr")
plot(perf)
auc = performance(pred, measure = "auc")
auc = round(auc@y.values[[1]],4)

png(filename = "Plots/Financials/Individual Contributions/Method_1.png",width = 1920,height = 1080)
plot(INDV_CONTR_EST ~ REL_INDV_CONTR, data=s_thedata, col=s_thedata$color,pch=20)
title(main="Total Individual Contributions - Subtraction",sub=paste("AIC: ",aic," AUC: ",auc,sep=""))
dev.off()




#look at logistic subtraction
l_thedata = thedata %>% mutate(REL_TOTAL_RECEIPTS = ifelse(TOTAL_RECEIPTS_GOP<=0,0,log(TOTAL_RECEIPTS_GOP)) - 
                                                    ifelse(TOTAL_RECEIPTS_DEM<=0,0,log(TOTAL_RECEIPTS_DEM)),
                               REL_TOTAL_DISBURSEMENTS = ifelse(TOTAL_DISBURSEMENTS_GOP<=0,0,log(TOTAL_DISBURSEMENTS_GOP)) - 
                                                         ifelse(TOTAL_DISBURSEMENTS_DEM<=0,0,log(TOTAL_DISBURSEMENTS_DEM)),
                               REL_COH = ifelse(COH_GOP<=0,0,log(COH_GOP)) - ifelse(COH_DEM<=0,0,log(COH_DEM)),
                               REL_INDV_CONTR = ifelse(INDIVIDUAL_CONTRIBUTIONS_GOP<=0,0,log(INDIVIDUAL_CONTRIBUTIONS_GOP)) - 
                                                ifelse(INDIVIDUAL_CONTRIBUTIONS_DEM<=0,0,log(INDIVIDUAL_CONTRIBUTIONS_DEM)))

##RECIPTS
l_rec = glm(GOP_win ~ REL_TOTAL_RECEIPTS,data=l_thedata,family=binomial)
summary(l_rec)
aic = round(summary(l_rec)[[5]],2)
l_thedata$TOTAL_RECEIPTS_EST = predict(l_rec,l_thedata,type="response")
pred = prediction(l_thedata$TOTAL_RECEIPTS_EST, l_thedata$GOP_win)
perf = performance(pred, measure = "tpr", x.measure = "fpr")
plot(perf)
auc = performance(pred, measure = "auc")
auc = round(auc@y.values[[1]],4)

png(filename = "Plots/Financials/Receipts/Method_2.png",width = 1920,height = 1080)
plot(TOTAL_RECEIPTS_EST ~ REL_TOTAL_RECEIPTS, data=l_thedata, col=l_thedata$color,pch=20)
title(main="Total Receipts - Subtraction",sub=paste("AIC: ",aic," AUC: ",auc,sep=""))
dev.off()

##DISBURSEMENTS
l_dis = glm(GOP_win ~ REL_TOTAL_DISBURSEMENTS,data=l_thedata,family=binomial)
summary(l_rec)
aic = round(summary(l_dis)[[5]],2)
l_thedata$TOTAL_DISBURSEMENTS_EST = predict(l_dis,l_thedata,type="response")
pred = prediction(l_thedata$TOTAL_DISBURSEMENTS_EST, l_thedata$GOP_win)
perf = performance(pred, measure = "tpr", x.measure = "fpr")
plot(perf)
auc = performance(pred, measure = "auc")
auc = round(auc@y.values[[1]],4)

png(filename = "Plots/Financials/Disbursements/Method_2.png",width = 1920,height = 1080)
plot(TOTAL_DISBURSEMENTS_EST ~ REL_TOTAL_DISBURSEMENTS, data=l_thedata, col=l_thedata$color,pch=20)
title(main="Total Disbursements - Subtraction",sub=paste("AIC: ",aic," AUC: ",auc,sep=""))
dev.off()

##COH
l_coh = glm(GOP_win ~ REL_COH,data=l_thedata,family=binomial)
summary(l_rec)
aic = round(summary(l_coh)[[5]],2)
l_thedata$TOTAL_COH_EST = predict(l_coh,l_thedata,type="response")
pred = prediction(l_thedata$TOTAL_COH_EST, l_thedata$GOP_win)
perf = performance(pred, measure = "tpr", x.measure = "fpr")
plot(perf)
auc = performance(pred, measure = "auc")
auc = round(auc@y.values[[1]],4)

png(filename = "Plots/Financials/COH/Method_2.png",width = 1920,height = 1080)
plot(TOTAL_COH_EST ~ REL_COH, data=l_thedata, col=l_thedata$color,pch=20)
title(main="Total COH - Subtraction",sub=paste("AIC: ",aic," AUC: ",auc,sep=""))
dev.off()

##INDIVIDUAL RECEIPTS
l_ind = glm(GOP_win ~ REL_INDV_CONTR,data=l_thedata,family=binomial)
summary(l_ind)
aic = round(summary(l_ind)[[5]],2)
l_thedata$INDV_CONTR_EST = predict(l_ind,l_thedata,type="response")
pred = prediction(l_thedata$INDV_CONTR_EST, l_thedata$GOP_win)
perf = performance(pred, measure = "tpr", x.measure = "fpr")
plot(perf)
auc = performance(pred, measure = "auc")
auc = round(auc@y.values[[1]],4)

png(filename = "Plots/Financials/Individual Contributions/Method_2.png",width = 1920,height = 1080)
plot(INDV_CONTR_EST ~ REL_INDV_CONTR, data=l_thedata, col=l_thedata$color,pch=20)
title(main="Total Individual Contributions - Subtraction",sub=paste("AIC: ",aic," AUC: ",auc,sep=""))
dev.off()


#look at logistic subtraction with a maximum amount
rec_levels = c(500000,1000000,1500000,2000000,2500000)
dis_levels = c(500000,1000000,1500000,2000000,2500000)
coh_levels = c(200000,400000,600000,800000,1000000)
ind_levels = c(500000,1000000,1500000,2000000,2500000)

for(i in seq(from=1,to=5,by=1)){
  c_thedata = thedata %>% mutate(TOTAL_RECEIPTS_GOP=ifelse(TOTAL_RECEIPTS_GOP>rec_levels[i],rec_levels[i],TOTAL_RECEIPTS_GOP),
                                 TOTAL_RECEIPTS_DEM=ifelse(TOTAL_RECEIPTS_DEM>rec_levels[i],rec_levels[i],TOTAL_RECEIPTS_DEM),
                                 TOTAL_DISBURSEMENTS_GOP=ifelse(TOTAL_DISBURSEMENTS_GOP>dis_levels[i],dis_levels[i],TOTAL_DISBURSEMENTS_GOP),
                                 TOTAL_DISBURSEMENTS_DEM=ifelse(TOTAL_DISBURSEMENTS_DEM>dis_levels[i],dis_levels[i],TOTAL_DISBURSEMENTS_DEM),
                                 COH_GOP=ifelse(COH_GOP>coh_levels[i],coh_levels[i],COH_GOP),
                                 COH_DEM=ifelse(COH_DEM>coh_levels[i],coh_levels[i],COH_DEM),
                                 INDIVIDUAL_CONTRIBUTIONS_GOP=ifelse(INDIVIDUAL_CONTRIBUTIONS_GOP>ind_levels[i],ind_levels[i],INDIVIDUAL_CONTRIBUTIONS_GOP),
                                 INDIVIDUAL_CONTRIBUTIONS_DEM=ifelse(INDIVIDUAL_CONTRIBUTIONS_DEM>ind_levels[i],ind_levels[i],INDIVIDUAL_CONTRIBUTIONS_DEM)) %>%
                        mutate(REL_TOTAL_RECEIPTS = ifelse(TOTAL_RECEIPTS_GOP<=0,0,log(TOTAL_RECEIPTS_GOP)) - 
                                                    ifelse(TOTAL_RECEIPTS_DEM<=0,0,log(TOTAL_RECEIPTS_DEM)),
                               REL_TOTAL_DISBURSEMENTS = ifelse(TOTAL_DISBURSEMENTS_GOP<=0,0,log(TOTAL_DISBURSEMENTS_GOP)) - 
                                                         ifelse(TOTAL_DISBURSEMENTS_DEM<=0,0,log(TOTAL_DISBURSEMENTS_DEM)),
                               REL_COH = ifelse(COH_GOP<=0,0,log(COH_GOP)) - ifelse(COH_DEM<=0,0,log(COH_DEM)),
                               REL_INDV_CONTR = ifelse(INDIVIDUAL_CONTRIBUTIONS_GOP<=0,0,log(INDIVIDUAL_CONTRIBUTIONS_GOP)) - 
                                                ifelse(INDIVIDUAL_CONTRIBUTIONS_DEM<=0,0,log(INDIVIDUAL_CONTRIBUTIONS_DEM)))
  
  
  
  ##RECIPTS
  c_rec = glm(GOP_win ~ REL_TOTAL_RECEIPTS,data=c_thedata,family=binomial)
  summary(c_rec)
  aic = round(summary(c_rec)[[5]],2)
  c_thedata$TOTAL_RECEIPTS_EST = predict(c_rec,c_thedata,type="response")
  pred = prediction(c_thedata$TOTAL_RECEIPTS_EST, c_thedata$GOP_win)
  perf = performance(pred, measure = "tpr", x.measure = "fpr")
  plot(perf)
  auc = performance(pred, measure = "auc")
  auc = round(auc@y.values[[1]],4)
  
  png(filename = paste("Plots/Financials/Receipts/Method_",i+2,".png",sep=""),width = 1920,height = 1080)
  plot(TOTAL_RECEIPTS_EST ~ REL_TOTAL_RECEIPTS, data=c_thedata, col=c_thedata$color,pch=20)
  title(main=paste("Total Receipts - Subtraction w/ Cutoff at ",rec_levels[i],sep=""),sub=paste("AIC: ",aic," AUC: ",auc,sep=""))
  dev.off()
  
  ##DISBURSEMENTS
  c_dis = glm(GOP_win ~ REL_TOTAL_DISBURSEMENTS,data=c_thedata,family=binomial)
  summary(c_rec)
  aic = round(summary(c_dis)[[5]],2)
  c_thedata$TOTAL_DISBURSEMENTS_EST = predict(c_dis,c_thedata,type="response")
  pred = prediction(c_thedata$TOTAL_DISBURSEMENTS_EST, c_thedata$GOP_win)
  perf = performance(pred, measure = "tpr", x.measure = "fpr")
  plot(perf)
  auc = performance(pred, measure = "auc")
  auc = round(auc@y.values[[1]],4)
  
  png(filename = paste("Plots/Financials/Disbursements/Method_",i+2,".png",sep=""),width = 1920,height = 1080)
  plot(TOTAL_DISBURSEMENTS_EST ~ REL_TOTAL_DISBURSEMENTS, data=c_thedata, col=c_thedata$color,pch=20)
  title(main=paste("Total Disbursements - Subtraction w/ Cutoff at ",dis_levels[i],sep=""),sub=paste("AIC: ",aic," AUC: ",auc,sep=""))
  dev.off()
  
  ##COH
  c_coh = glm(GOP_win ~ REL_COH,data=c_thedata,family=binomial)
  summary(c_rec)
  aic = round(summary(c_coh)[[5]],2)
  c_thedata$TOTAL_COH_EST = predict(c_coh,c_thedata,type="response")
  pred = prediction(c_thedata$TOTAL_COH_EST, c_thedata$GOP_win)
  perf = performance(pred, measure = "tpr", x.measure = "fpr")
  plot(perf)
  auc = performance(pred, measure = "auc")
  auc = round(auc@y.values[[1]],4)
  
  png(filename = paste("Plots/Financials/COH/Method_",i+2,".png",sep=""),width = 1920,height = 1080)
  plot(TOTAL_COH_EST ~ REL_COH, data=c_thedata, col=c_thedata$color,pch=20)
  title(main=paste("Total COH - Subtraction w/ Cutoff at ",coh_levels[i],sep=""),sub=paste("AIC: ",aic," AUC: ",auc,sep=""))
  dev.off()
  
  ##INDIVIDUAL RECEIPTS
  c_ind = glm(GOP_win ~ REL_INDV_CONTR,data=c_thedata,family=binomial)
  summary(c_ind)
  aic = round(summary(c_ind)[[5]],2)
  c_thedata$INDV_CONTR_EST = predict(c_ind,c_thedata,type="response")
  pred = prediction(c_thedata$INDV_CONTR_EST, c_thedata$GOP_win)
  perf = performance(pred, measure = "tpr", x.measure = "fpr")
  plot(perf)
  auc = performance(pred, measure = "auc")
  auc = round(auc@y.values[[1]],4)
  
  png(filename = paste("Plots/Financials/Individual Contributions/Method_",i+2,".png",sep=""),width = 1920,height = 1080)
  plot(INDV_CONTR_EST ~ REL_INDV_CONTR, data=c_thedata, col=c_thedata$color,pch=20)
  title(main=paste("Total Individual Contributions - Subtraction w/ Cutoff at ",ind_levels[i],sep=""),sub=paste("AIC: ",aic," AUC: ",auc,sep=""))
  dev.off()
  
}





