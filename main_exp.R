#### main exp ####



library(ggplot2)
library(randomForest) #na.roughfix was used in this package
library(ggepi)
library(ggridges)
library(patchwork)
library(party)
library(caret)
library(dplyr)
library(tidyverse)
library(ggpubr)
library(vegan)
library('dabestr')
library('Rmisc')

df<- read.csv('df_main.csv')
df$belowground<- NULL
df<-df%>%
  filter(D==0)
multi.two.group.unpaired <- 
  df %>%
  dabest(ID,aboveground, 
         idx = list(c('CT','I1','I2','I3','M1','M2','M3','C1','C2','C3','3','3_in','6','9')),
         paired = FALSE)
multi.two.group.unpaired.meandiff <- mean_diff(multi.two.group.unpaired)
multi.two.group.unpaired.meandiff %>% plot()





#### dissimilarity calculation ####
setwd('C:/Huiying data/2nd_chapter/main_exp')
df<- read.csv('df_main.csv')
df<-df%>%
  filter(ID!='3_in')
df_ct<-df%>%
  filter(D==0)


#df_s<- df_s%>%
 # filter(ID!='3_in')

#### distance calculation ####
library(vegan)
standardized_sig_facts<-df_ct%>%
  filter(lv=="1")%>%
  group_by(remark)%>%
  summarise(across(c(12:20),funs(mean(.,na.rm = TRUE)), .names = "(mean_{.col}"))%>% #collapse data frame into mean values of measurements for each factor while excluding NA values.#
  data.frame()%>%
  column_to_rownames(var = "remark")%>%
  scale()
#write.csv(standardized_sig_facts, "standardized_sig_facts_ct.csv")
bray_dis<- vegdist(standardized_sig_facts,method = "euclidean")
df<-as.matrix(bray_dis)
#### calculate distance for each factor level ####
f3<-df_ct%>%
  filter(lv==3)
f6<-df_ct%>%
  filter(lv==6)


####
####level correlation####
setwd('C:/Huiying data/2nd_chapter/main_exp')
df<- read.csv('df_main.csv')
df_s<-df%>%
  filter(D==0)
df_lv_ct<-df_s%>%
  filter(lv!=0)
#### plotting ####
p_lv_respiration<- ggplot(data=df_lv_ct,aes(x=lv,y=respiration))+
  geom_point(data=df_lv_ct, aes(lv,respiration),color='black', show.legend = FALSE,position=position_jitter(width=0.55),size = 2,alpha=0.08) +
  geom_smooth(method='lm',fullrange=T,se=T,linewidth=1.2,color='#619DBB',fill='#619DBB')+
  theme(axis.text = element_text(size = 12),axis.title = element_text(size = 12),plot.title = element_text(size = 12))+
  ylab('soil respiration')+
  xlab('')+scale_x_continuous(breaks = c(1, 3, 6, 9), labels = c("1", "3", "6", "9"))+
  stat_cor(aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~")),size=2.6,label.y.npc="top", label.x.npc = "left",method='spearman')+
  stat_regline_equation(label.y = 2500,size=2.6)+
  theme(panel.background = element_rect(fill = "gray96", color = "black"))
p_lv_ph<- ggplot(data=df_lv_ct,aes(x=lv,y=PH))+
  geom_point(data=df_lv_ct, aes(lv,PH),color='black', show.legend = FALSE,position=position_jitter(width=0.55),size = 2,alpha=0.08) +
  geom_smooth(method='lm',fullrange=T,se=T,linewidth=1.2,color='#619DBB',fill='#619DBB')+
  theme(axis.text = element_text(size = 12),axis.title = element_text(size = 12),plot.title = element_text(size = 12))+
  ylab('soil pH')+
  xlab('')+scale_x_continuous(breaks = c(1, 3, 6, 9), labels = c("1", "3", "6", "9"))+
  stat_cor(aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~")),size=2.6,label.y.npc="top", label.x.npc = "left",method='spearman')+
  stat_regline_equation(label.y = 5.5,size=2.6)+
  theme(panel.background = element_rect(fill = "gray96", color = "black"))
p_lv_wsa<-ggplot(data=df_lv_ct,aes(x=lv,y=WSA))+
  geom_point(data=df_lv_ct, aes(lv,WSA),color='black', show.legend = FALSE,position=position_jitter(width=0.55),size = 2,alpha=0.08) +
  geom_smooth(method='lm',fullrange=T,se=T,linewidth=1.2,color='#619DBB',fill='#619DBB')+
  theme(axis.text = element_text(size = 12),axis.title = element_text(size = 12),plot.title = element_text(size = 12))+
  ylab('water stable aggregate (%)')+
  xlab('')+scale_x_continuous(breaks = c(1, 3, 6, 9), labels = c("1", "3", "6", "9"))+
  stat_cor(aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~")),size=2.6,label.y.npc="top", label.x.npc = "left",method='spearman')+
  stat_regline_equation(label.y = 80,size=2.6)+
  theme(panel.background = element_rect(fill = "gray96", color = "black"))
p_lv_ace<-ggplot(data=df_lv_ct,aes(x=lv,y=ace))+
  geom_point(data=df_lv_ct, aes(lv,ace),color='black', show.legend = FALSE,position=position_jitter(width=0.55),size = 2,alpha=0.08) +
  geom_smooth(method='lm',fullrange=T,se=T,linewidth=1.2,color='#619DBB',fill='#619DBB')+
  theme(axis.text = element_text(size = 12),axis.title = element_text(size = 12),plot.title = element_text(size = 12))+
  ylab('ace_enzyme activity')+
  xlab('')+scale_x_continuous(breaks = c(1, 3, 6, 9), labels = c("1", "3", "6", "9"))+
  stat_cor(aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~")),size=2.6,label.y.npc="top", label.x.npc = "left",method='spearman')+
  stat_regline_equation(label.y = 0.60,size=2.6)+
  theme(panel.background = element_rect(fill = "gray96", color = "black"))
p_lv_gluco<- ggplot(data=df_lv_ct,aes(x=lv,y=gluco))+
  geom_point(data=df_lv_ct, aes(lv,gluco),color='black', show.legend = FALSE,position=position_jitter(width=0.55),size = 2,alpha=0.08) +
  geom_smooth(method='lm',fullrange=T,se=T,linewidth=1.2,color='#619DBB',fill='#619DBB')+
  theme(axis.text = element_text(size = 12),axis.title = element_text(size = 12),plot.title = element_text(size = 12))+
  ylab('gluco_enzyme activity')+
  xlab('')+scale_x_continuous(breaks = c(1, 3, 6, 9), labels = c("1", "3", "6", "9"))+
  stat_cor(aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~")),size=2.6,label.y.npc="top", label.x.npc = "left",method='spearman')+
  stat_regline_equation(label.y = 2,size=2.6)+
  theme(panel.background = element_rect(fill = "gray96", color = "black"))
#### no significance ####
p_lv_phos<-ggplot(data=df_lv_ct,aes(x=lv,y=phos))+
  geom_point(data=df_lv_ct, aes(lv,phos),color='black', show.legend = FALSE,position=position_jitter(width=0.55),size = 2,alpha=0.08) +
  geom_smooth(method='lm',fullrange=T,se=T,linewidth=1.2,color='#619DBB',fill='#619DBB')+
  theme(axis.text = element_text(size = 12),axis.title = element_text(size = 12),plot.title = element_text(size = 12))+
  ylab('phos_enzyme activity')+
  xlab('')+scale_x_continuous(breaks = c(1, 3, 6, 9), labels = c("1", "3", "6", "9"))+
  stat_cor(aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~")),size=2.6,label.y.npc="top", label.x.npc = "left",method='spearman')+
  stat_regline_equation(label.y = 3,size=2.6)+
  theme(panel.background = element_rect(fill = "gray96", color = "black"))
p_lv_cello<-ggplot(data=df_lv_ct,aes(x=lv,y=cello))+
  geom_point(data=df_lv_ct, aes(lv,cello),color='black', show.legend = FALSE,position=position_jitter(width=0.55),size = 2,alpha=0.08) +
  geom_smooth(method='lm',fullrange=T,se=T,linewidth=1.2,color='#619DBB',fill='#619DBB')+
  theme(axis.text = element_text(size = 12),axis.title = element_text(size = 12),plot.title = element_text(size = 12))+
  ylab('cello_enzyme activity')+
  xlab('')+scale_x_continuous(breaks = c(1, 3, 6, 9), labels = c("1", "3", "6", "9"))+
  stat_cor(aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~")),size=2.6,label.y.npc="top", label.x.npc = "left",method='spearman')+
  stat_regline_equation(label.y = 0.82,size=2.6)+
  theme(panel.background = element_rect(fill = "gray96", color = "black"))

p_lv_aboveground<-ggplot(data=df_lv_ct,aes(x=lv,y=aboveground))+
  geom_point(data=df_lv_ct, aes(lv,aboveground),color='black', show.legend = FALSE,position=position_jitter(width=0.55),size = 2,alpha=0.08) +
  geom_smooth(method='lm',fullrange=T,se=T,linewidth=1.2,color='#619DBB',fill='#619DBB')+
  theme(axis.text = element_text(size = 12),axis.title = element_text(size = 12),plot.title = element_text(size = 12))+
  ylab('aboveground')+
  xlab('')+scale_x_continuous(breaks = c(1, 3, 6, 9), labels = c("1", "3", "6", "9"))+
  stat_cor(aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~")),size=2.6,label.y.npc="top", label.x.npc = "left",method='spearman')+
  stat_regline_equation(label.y = 350,size=2.6)+
  theme(panel.background = element_rect(fill = "gray96", color = "black"))
p_lv_belowground<-ggplot(data=df_lv_ct,aes(x=lv,y=belowground))+
  geom_point(data=df_lv_ct, aes(lv,belowground),color='black', show.legend = FALSE,position=position_jitter(width=0.55),size = 2,alpha=0.15) +
  geom_smooth(method='lm',fullrange=T,se=T,linewidth=1.2,color='#619DBB',fill='#619DBB')+
  theme(axis.text = element_text(size = 12),axis.title = element_text(size = 12),plot.title = element_text(size = 12))+
  ylab('belowground')+
  xlab('')+scale_x_continuous(breaks = c(1, 3, 6, 9), labels = c("1", "3", "6", "9"))+
  stat_cor(aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~")),size=2.6,label.y.npc="top", label.x.npc = "left",method='spearman')+
  stat_regline_equation(label.y = 100,size=2.6)+
  theme(panel.background = element_rect(fill = "gray96", color = "black"))

df<- read.csv('df_main.csv')
df_d<-df%>%
  filter(D==1)
df_lv_d<-df_d%>%
  filter(lv!=0)

dp_lv_respiration<- ggplot(data=df_lv_d,aes(x=lv,y=respiration))+
  geom_point(data=df_lv_d, aes(lv,respiration),color='black', show.legend = FALSE,position=position_jitter(width=0.55),size = 2,alpha=0.08) +
  geom_smooth(method='lm',fullrange=T,se=T,linewidth=1.2,color='#619DBB',fill='#619DBB')+
  theme(axis.text = element_text(size = 12),axis.title = element_text(size = 12),plot.title = element_text(size = 12))+
  ylab('soil respiration')+
  xlab('')+scale_x_continuous(breaks = c(1, 3, 6, 9), labels = c("1", "3", "6", "9"))+
  stat_cor(aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~")),size=2.6,label.y.npc="top", label.x.npc = "left",method='spearman')+
  stat_regline_equation(label.y = 2500,size=2.6)+
  theme(panel.background = element_rect(fill = "gray96", color = "black"))
dp_lv_ph<- ggplot(data=df_lv_d,aes(x=lv,y=PH))+
  geom_point(data=df_lv_d, aes(lv,PH),color='black', show.legend = FALSE,position=position_jitter(width=0.55),size = 2,alpha=0.08) +
  geom_smooth(method='lm',fullrange=T,se=T,linewidth=1.2,color='#619DBB',fill='#619DBB')+
  theme(axis.text = element_text(size = 12),axis.title = element_text(size = 12),plot.title = element_text(size = 12))+
  ylab('soil pH')+
  xlab('')+scale_x_continuous(breaks = c(1, 3, 6, 9), labels = c("1", "3", "6", "9"))+
  stat_cor(aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~")),size=2.6,label.y.npc="top", label.x.npc = "left",method='spearman')+
  stat_regline_equation(label.y = 5.5,size=2.6)+
  theme(panel.background = element_rect(fill = "gray96", color = "black"))
dp_lv_wsa<-ggplot(data=df_lv_d,aes(x=lv,y=WSA))+
  geom_point(data=df_lv_d, aes(lv,WSA),color='black', show.legend = FALSE,position=position_jitter(width=0.55),size = 2,alpha=0.08) +
  geom_smooth(method='lm',fullrange=T,se=T,linewidth=1.2,color='#619DBB',fill='#619DBB')+
  theme(axis.text = element_text(size = 12),axis.title = element_text(size = 12),plot.title = element_text(size = 12))+
  ylab('water stable aggregate (%)')+
  xlab('')+scale_x_continuous(breaks = c(1, 3, 6, 9), labels = c("1", "3", "6", "9"))+
  stat_cor(aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~")),size=2.6,label.y.npc="top", label.x.npc = "left",method='spearman')+
  stat_regline_equation(label.y = 50,size=2.6)+
  theme(panel.background = element_rect(fill = "gray96", color = "black"))
dp_lv_ace<-ggplot(data=df_lv_d,aes(x=lv,y=ace))+
  geom_point(data=df_lv_d, aes(lv,ace),color='black', show.legend = FALSE,position=position_jitter(width=0.55),size = 2,alpha=0.08) +
  geom_smooth(method='lm',fullrange=T,se=T,linewidth=1.2,color='#619DBB',fill='#619DBB')+
  theme(axis.text = element_text(size = 12),axis.title = element_text(size = 12),plot.title = element_text(size = 12))+
  ylab('ace_enzyme activity')+
  ylim(0,1)+
  xlab('')+scale_x_continuous(breaks = c(1, 3, 6, 9), labels = c("1", "3", "6", "9"))+
  stat_cor(aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~")),size=2.6,label.y.npc="top", label.x.npc = "left",method='spearman')+
  stat_regline_equation(label.y = 0.60,size=2.6)+
  theme(panel.background = element_rect(fill = "gray96", color = "black"))
dp_lv_gluco<- ggplot(data=df_lv_d,aes(x=lv,y=gluco))+
  geom_point(data=df_lv_d, aes(lv,gluco),color='black', show.legend = FALSE,position=position_jitter(width=0.55),size = 2,alpha=0.08) +
  geom_smooth(method='lm',fullrange=T,se=T,linewidth=1.2,color='#619DBB',fill='#619DBB')+
  theme(axis.text = element_text(size = 12),axis.title = element_text(size = 12),plot.title = element_text(size = 12))+
  ylab('gluco_enzyme activity')+
  xlab('')+scale_x_continuous(breaks = c(1, 3, 6, 9), labels = c("1", "3", "6", "9"))+
  stat_cor(aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~")),size=2.6,label.y.npc="top", label.x.npc = "left",method='spearman')+
  stat_regline_equation(label.y = 2,size=2.6)+
  theme(panel.background = element_rect(fill = "gray96", color = "black"))

dp_lv_phos<-ggplot(data=df_lv_d,aes(x=lv,y=phos))+
  geom_point(data=df_lv_d, aes(lv,phos),color='black', show.legend = FALSE,position=position_jitter(width=0.55),size = 2,alpha=0.08) +
  geom_smooth(method='lm',fullrange=T,se=T,linewidth=1.2,color='#619DBB',fill='#619DBB')+
  theme(axis.text = element_text(size = 12),axis.title = element_text(size = 12),plot.title = element_text(size = 12))+
  ylab('phos_enzyme activity')+
  xlab('')+scale_x_continuous(breaks = c(1, 3, 6, 9), labels = c("1", "3", "6", "9"))+
  stat_cor(aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~")),size=2.6,label.y.npc="top", label.x.npc = "left",method='spearman')+
  stat_regline_equation(label.y = 3,size=2.6)+
  theme(panel.background = element_rect(fill = "gray96", color = "black"))
dp_lv_cello<-ggplot(data=df_lv_d,aes(x=lv,y=cello))+
  geom_point(data=df_lv_d, aes(lv,cello),color='black', show.legend = FALSE,position=position_jitter(width=0.55),size = 2,alpha=0.08) +
  geom_smooth(method='lm',fullrange=T,se=T,linewidth=1.2,color='#619DBB',fill='#619DBB')+
  theme(axis.text = element_text(size = 12),axis.title = element_text(size = 12),plot.title = element_text(size = 12))+
  ylab('cello_enzyme activity')+
  xlab('')+scale_x_continuous(breaks = c(1, 3, 6, 9), labels = c("1", "3", "6", "9"))+
  stat_cor(aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~")),size=2.6,label.y.npc="top", label.x.npc = "left",method='spearman')+
  stat_regline_equation(label.y = 0.62,size=2.6)+
  theme(panel.background = element_rect(fill = "gray96", color = "black"))

dp_lv_aboveground<-ggplot(data=df_lv_d,aes(x=lv,y=aboveground))+
  geom_point(data=df_lv_d, aes(lv,aboveground),color='black', show.legend = FALSE,position=position_jitter(width=0.55),size = 2,alpha=0.08) +
  geom_smooth(method='lm',fullrange=T,se=T,linewidth=1.2,color='#619DBB',fill='#619DBB')+
  theme(axis.text = element_text(size = 12),axis.title = element_text(size = 12),plot.title = element_text(size = 12))+
  ylab('aboveground')+
  xlab('')+scale_x_continuous(breaks = c(1, 3, 6, 9), labels = c("1", "3", "6", "9"))+
  stat_cor(aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~")),size=2.6,label.y.npc="top", label.x.npc = "left",method='spearman')+
  stat_regline_equation(label.y = 350,size=2.6)+
  theme(panel.background = element_rect(fill = "gray96", color = "black"))
dp_lv_belowground<-ggplot(data=df_lv_d,aes(x=lv,y=belowground))+
  geom_point(data=df_lv_d, aes(lv,belowground),color='black', show.legend = FALSE,position=position_jitter(width=0.55),size = 2,alpha=0.15) +
  geom_smooth(method='lm',fullrange=T,se=T,linewidth=1.2,color='#619DBB',fill='#619DBB')+
  theme(axis.text = element_text(size = 12),axis.title = element_text(size = 12),plot.title = element_text(size = 12))+
  ylab('belowground')+
  xlab('')+scale_x_continuous(breaks = c(1, 3, 6, 9), labels = c("1", "3", "6", "9"))+
  stat_cor(aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~")),size=2.6,label.y.npc="top", label.x.npc = "left",method='spearman')+
  stat_regline_equation(label.y = 100,size=2.6)+
  theme(panel.background = element_rect(fill = "gray96", color = "black"))

##### compare 3in and 3out ####
setwd('C:/Huiying data/2nd_chapter/main_exp')
df<- read.csv('df_main.csv')
df_3<-df%>%
  filter(lv!=1,lv!=6,lv!=9)
df_3d<- df_3%>%
  filter(D==1)
multi.two.group.unpaired <- 
  df_3d %>%
  dabest(ID,WSA, 
         idx = list(c('CT','3_in','3')),
         paired = FALSE)
multi.two.group.unpaired.meandiff <- mean_diff(multi.two.group.unpaired)
multi.two.group.unpaired.meandiff %>% plot()

dat<-multi.two.group.unpaired.meandiff$result 
AGGse_WSA<-summarySE(df_3d, measurevar="WSA", groupvars=c("ID"))
dat_WSA<-dat[c(1,2,8,10,11)] 

calculate_WSA<- subset(df_3d,df_3d$ID=='CT')
mean_WSA<- mean(calculate_WSA$WSA)
dat_WSA$bca_ci_low<- c(dat_WSA$bca_ci_low+mean_WSA)
dat_WSA$bca_ci_high<- c(dat_WSA$bca_ci_high+mean_WSA)
dat_WSA$difference<- c(dat_WSA$difference+mean_WSA)
colnames(dat_WSA)[2] <- 'Treatment3'
df_3d$ID<-factor(df_3d$ID, levels=c('CT','3_in','3'))
AGGse_WSA$ID<- factor(AGGse_WSA$ID, levels=c('CT','3_in','3'))

p_wsa_ct<- ggplot(data=df_3d,aes(x=ID,y=WSA,group=ID,color=ID))+
  geom_errorbar(data=AGGse_WSA,aes(x=as.numeric(factor(ID)),ymin=WSA-se,ymax=WSA+se),width=0.1,size=1,color='black')+
  geom_point(data=df_3d,aes(colour = factor(ID)),position=position_jitter(width=0.1),  size = 2.8,alpha=0.1,show.legend = FALSE) +
  scale_x_discrete(guide = guide_axis(angle = 45),limits =c('CT','3_in','3'))+
   geom_hline(yintercept=mean_WSA,linetype='dashed')+
  scale_color_manual(values=c('khaki4','burlywood4','burlywood4'))+
  xlab('') +
  ylim(20,100)+
  stat_halfeye(data = AGGse_WSA, aes(x = ID, y = WSA, ymin=WSA-se,ymax=WSA+se), scale=0.3, alpha =1,color='black') + 
   theme(axis.text = element_text(size = 12),axis.title = element_text(size = 12),plot.title = element_text(size = 12))+
  ylab('WSA (%)_no drought')+
  theme_bw()
p_wsa_d<- ggplot(data=df_3d,aes(x=ID,y=WSA,group=ID,color=ID))+
  geom_errorbar(data=AGGse_WSA,aes(x=as.numeric(factor(ID)),ymin=WSA-se,ymax=WSA+se),width=0.1,size=1,color='black')+
  geom_point(data=df_3d,aes(colour = factor(ID)),position=position_jitter(width=0.1),  size = 2.8,alpha=0.1,show.legend = FALSE) +
  scale_x_discrete(guide = guide_axis(angle = 45),limits =c('CT','3_in','3'))+
  geom_hline(yintercept=mean_WSA,linetype='dashed')+
  scale_color_manual(values=c('khaki4','burlywood4','burlywood4'))+
  xlab('') +
  ylim(20,100)+
  stat_halfeye(data = AGGse_WSA, aes(x = ID, y = WSA, ymin=WSA-se,ymax=WSA+se), scale=0.3, alpha =1,color='black') + 
  theme(axis.text = element_text(size = 12),axis.title = element_text(size = 12),plot.title = element_text(size = 12))+
  ylab('WSA (%)_drought')+
  theme_bw()
p_wsa_ct+p_wsa_d
library(ggdist)
#### ph ####
setwd('C:/Huiying data/2nd_chapter/main_exp')
df<- read.csv('df_main.csv')
df_3<-df%>%
  filter(lv!=1,lv!=6,lv!=9)
df_3d<- df_3%>%
  filter(D==0)
multi.two.group.unpaired <- 
  df_3d %>%
  dabest(ID,PH, 
         idx = list(c('CT','3_in','3')),
         paired = FALSE)
multi.two.group.unpaired.meandiff <- mean_diff(multi.two.group.unpaired)
multi.two.group.unpaired.meandiff %>% plot()

dat<-multi.two.group.unpaired.meandiff$result 
AGGse_WSA<-summarySE(df_3d, measurevar="PH", groupvars=c("ID"))
dat_WSA<-dat[c(1,2,8,10,11)] 

calculate_WSA<- subset(df_3d,df_3d$ID=='CT')
mean_WSA<- mean(calculate_WSA$PH)
dat_WSA$bca_ci_low<- c(dat_WSA$bca_ci_low+mean_WSA)
dat_WSA$bca_ci_high<- c(dat_WSA$bca_ci_high+mean_WSA)
dat_WSA$difference<- c(dat_WSA$difference+mean_WSA)
colnames(dat_WSA)[2] <- 'Treatment3'
df_3d$ID<-factor(df_3d$ID, levels=c('CT','3_in','3'))
AGGse_WSA$ID<- factor(AGGse_WSA$ID, levels=c('CT','3_in','3'))

p_ph_ct<- ggplot(data=df_3d,aes(x=ID,y=PH,group=ID,color=ID))+
  geom_errorbar(data=AGGse_WSA,aes(x=as.numeric(factor(ID)),ymin=PH-se,ymax=PH+se),width=0.1,size=1,color='black')+
  geom_point(data=df_3d,aes(colour = factor(ID)),position=position_jitter(width=0.1),  size = 2.8,alpha=0.1,show.legend = FALSE) +
  scale_x_discrete(guide = guide_axis(angle = 45),limits =c('CT','3_in','3'))+
  geom_hline(yintercept=mean_WSA,linetype='dashed')+
  scale_color_manual(values=c('khaki4','burlywood4','burlywood4'))+
  xlab('') +
  ylim(4.25,5.5)+
  theme(axis.text = element_text(size = 12),axis.title = element_text(size = 12),plot.title = element_text(size = 12))+
  ylab('pH_no drought')+
  theme_bw()

p_ph_d<- ggplot(data=df_3d,aes(x=ID,y=PH,group=ID,color=ID))+
  geom_errorbar(data=AGGse_WSA,aes(x=as.numeric(factor(ID)),ymin=PH-se,ymax=PH+se),width=0.1,size=1,color='black')+
  geom_point(data=df_3d,aes(colour = factor(ID)),position=position_jitter(width=0.1),  size = 2.8,alpha=0.1,show.legend = FALSE) +
  scale_x_discrete(guide = guide_axis(angle = 45),limits =c('CT','3_in','3'))+
  geom_hline(yintercept=mean_WSA,linetype='dashed')+
  scale_color_manual(values=c('khaki4','burlywood4','burlywood4'))+
  xlab('') +
  ylim(4.25,5.5)+
  stat_halfeye(data = AGGse_WSA, aes(x = ID, y = PH, ymin=PH-se,ymax=PH+se), scale=0.3, alpha =1,color='black') + 
  theme(axis.text = element_text(size = 12),axis.title = element_text(size = 12),plot.title = element_text(size = 12))+
  ylab('pH_drought')+
  theme_bw()
p_ph_ct+p_ph_d


#### aboveground ####
setwd('C:/Huiying data/2nd_chapter/main_exp')
df<- read.csv('df_main.csv')
df_3<-df%>%
  filter(lv!=1,lv!=6,lv!=9)
df_3d<- df_3%>%
  filter(D==0)
multi.two.group.unpaired <- 
  df_3d %>%
  dabest(ID,aboveground, 
         idx = list(c('CT','3_in','3')),
         paired = FALSE)
multi.two.group.unpaired.meandiff <- mean_diff(multi.two.group.unpaired)
multi.two.group.unpaired.meandiff %>% plot()

dat<-multi.two.group.unpaired.meandiff$result 
AGGse_WSA<-summarySE(df_3d, measurevar="aboveground", groupvars=c("ID"))
dat_WSA<-dat[c(1,2,8,10,11)] 

calculate_WSA<- subset(df_3d,df_3d$ID=='CT')
mean_WSA<- mean(calculate_WSA$aboveground)
dat_WSA$bca_ci_low<- c(dat_WSA$bca_ci_low+mean_WSA)
dat_WSA$bca_ci_high<- c(dat_WSA$bca_ci_high+mean_WSA)
dat_WSA$difference<- c(dat_WSA$difference+mean_WSA)
colnames(dat_WSA)[2] <- 'Treatment3'
df_3d$ID<-factor(df_3d$ID, levels=c('CT','3_in','3'))
AGGse_WSA$ID<- factor(AGGse_WSA$ID, levels=c('CT','3_in','3'))

p_aboveground_ct<- ggplot(data=df_3d,aes(x=ID,y=aboveground,group=ID,color=ID))+
  geom_errorbar(data=AGGse_WSA,aes(x=as.numeric(factor(ID)),ymin=aboveground-se,ymax=aboveground+se),width=0.1,size=1,color='black')+
  geom_point(data=df_3d,aes(colour = factor(ID)),position=position_jitter(width=0.1),  size = 2.8,alpha=0.1,show.legend = FALSE) +
  scale_x_discrete(guide = guide_axis(angle = 45),limits =c('CT','3_in','3'))+
  geom_hline(yintercept=mean_WSA,linetype='dashed')+
  scale_color_manual(values=c('khaki4','burlywood4','burlywood4'))+
  xlab('') +
  stat_halfeye(data = AGGse_WSA, aes(x = ID, y = aboveground, ymin=aboveground-se,ymax=aboveground+se), scale=0.3, alpha =1,color='black') + 
  theme(axis.text = element_text(size = 12),axis.title = element_text(size = 12),plot.title = element_text(size = 12))+
  ylab('aboveground_no drought')+
  theme_bw()

p_aboveground_d<- ggplot(data=df_3d,aes(x=ID,y=aboveground,group=ID,color=ID))+
  geom_errorbar(data=AGGse_WSA,aes(x=as.numeric(factor(ID)),ymin=aboveground-se,ymax=aboveground+se),width=0.1,size=1,color='black')+
  geom_point(data=df_3d,aes(colour = factor(ID)),position=position_jitter(width=0.1),  size = 2.8,alpha=0.1,show.legend = FALSE) +
  scale_x_discrete(guide = guide_axis(angle = 45),limits =c('CT','3_in','3'))+
  geom_hline(yintercept=mean_WSA,linetype='dashed')+
  scale_color_manual(values=c('khaki4','burlywood4','burlywood4'))+
  xlab('') +
  stat_halfeye(data = AGGse_WSA, aes(x = ID, y = aboveground, ymin=aboveground-se,ymax=aboveground+se), scale=0.3, alpha =1,color='black') + 
  theme(axis.text = element_text(size = 12),axis.title = element_text(size = 12),plot.title = element_text(size = 12))+
  ylab('aboveground_drought')+
  theme_bw()
p_aboveground_ct+p_aboveground_d

#### belowground ####
setwd('C:/Huiying data/2nd_chapter/main_exp')
df<- read.csv('df_main.csv')
df_3<-df%>%
  filter(lv!=1,lv!=6,lv!=9)
df_3d<- df_3%>%
  filter(D==1)
multi.two.group.unpaired <- 
  df_3d %>%
  dabest(ID,belowground, 
         idx = list(c('CT','3_in','3')),
         paired = FALSE)
multi.two.group.unpaired.meandiff <- mean_diff(multi.two.group.unpaired)
multi.two.group.unpaired.meandiff %>% plot()

dat<-multi.two.group.unpaired.meandiff$result 
AGGse_WSA<-summarySE(df_3d, measurevar="belowground", groupvars=c("ID"))
dat_WSA<-dat[c(1,2,8,10,11)] 

calculate_WSA<- subset(df_3d,df_3d$ID=='CT')
mean_WSA<- mean(calculate_WSA$belowground)
dat_WSA$bca_ci_low<- c(dat_WSA$bca_ci_low+mean_WSA)
dat_WSA$bca_ci_high<- c(dat_WSA$bca_ci_high+mean_WSA)
dat_WSA$difference<- c(dat_WSA$difference+mean_WSA)
colnames(dat_WSA)[2] <- 'Treatment3'
df_3d$ID<-factor(df_3d$ID, levels=c('CT','3_in','3'))
AGGse_WSA$ID<- factor(AGGse_WSA$ID, levels=c('CT','3_in','3'))

p_belowground_ct<- ggplot(data=df_3d,aes(x=ID,y=belowground,group=ID,color=ID))+
  geom_errorbar(data=AGGse_WSA,aes(x=as.numeric(factor(ID)),ymin=belowground-se,ymax=belowground+se),width=0.1,size=1,color='black')+
  geom_point(data=df_3d,aes(colour = factor(ID)),position=position_jitter(width=0.1),  size = 2.8,alpha=0.1,show.legend = FALSE) +
  scale_x_discrete(guide = guide_axis(angle = 45),limits =c('CT','3_in','3'))+
  geom_hline(yintercept=mean_WSA,linetype='dashed')+
  scale_color_manual(values=c('khaki4','burlywood4','burlywood4'))+
  xlab('')+
  stat_halfeye(data = AGGse_WSA, aes(x = ID, y = belowground, ymin=belowground-se,ymax=belowground+se), scale=0.3, alpha =1,color='black') + 
  theme(axis.text = element_text(size = 12),axis.title = element_text(size = 12),plot.title = element_text(size = 12))+
  ylab('belowground_no drought')+
  theme_bw()

p_belowground_d<- ggplot(data=df_3d,aes(x=ID,y=belowground,group=ID,color=ID))+
  geom_errorbar(data=AGGse_WSA,aes(x=as.numeric(factor(ID)),ymin=belowground-se,ymax=belowground+se),width=0.1,size=1,color='black')+
  geom_point(data=df_3d,aes(colour = factor(ID)),position=position_jitter(width=0.1),  size = 2.8,alpha=0.1,show.legend = FALSE) +
  scale_x_discrete(guide = guide_axis(angle = 45),limits =c('CT','3_in','3'))+
  geom_hline(yintercept=mean_WSA,linetype='dashed')+
  scale_color_manual(values=c('khaki4','burlywood4','burlywood4'))+
  xlab('') +
  stat_halfeye(data = AGGse_WSA, aes(x = ID, y = belowground, ymin=belowground-se,ymax=belowground+se), scale=0.3, alpha =1,color='black') + 
  theme(axis.text = element_text(size = 12),axis.title = element_text(size = 12),plot.title = element_text(size = 12))+
  ylab('belowground_drought')+
  theme_bw()
p_belowground_ct+p_belowground_d

#### respiration ####
setwd('C:/Huiying data/2nd_chapter/main_exp')
df<- read.csv('df_main.csv')
df_3<-df%>%
  filter(lv!=1,lv!=6,lv!=9)
df_3d<- df_3%>%
  filter(D==1)
multi.two.group.unpaired <- 
  df_3d %>%
  dabest(ID,respiration, 
         idx = list(c('CT','3_in','3')),
         paired = FALSE)
multi.two.group.unpaired.meandiff <- mean_diff(multi.two.group.unpaired)
multi.two.group.unpaired.meandiff %>% plot()

dat<-multi.two.group.unpaired.meandiff$result 
AGGse_WSA<-summarySE(df_3d, measurevar="respiration", groupvars=c("ID"))
dat_WSA<-dat[c(1,2,8,10,11)] 

calculate_WSA<- subset(df_3d,df_3d$ID=='CT')
mean_WSA<- mean(calculate_WSA$respiration)
dat_WSA$bca_ci_low<- c(dat_WSA$bca_ci_low+mean_WSA)
dat_WSA$bca_ci_high<- c(dat_WSA$bca_ci_high+mean_WSA)
dat_WSA$difference<- c(dat_WSA$difference+mean_WSA)
colnames(dat_WSA)[2] <- 'Treatment3'
df_3d$ID<-factor(df_3d$ID, levels=c('CT','3_in','3'))
AGGse_WSA$ID<- factor(AGGse_WSA$ID, levels=c('CT','3_in','3'))

p_respiration_ct<- ggplot(data=df_3d,aes(x=ID,y=respiration,group=ID,color=ID))+
  geom_errorbar(data=AGGse_WSA,aes(x=as.numeric(factor(ID)),ymin=respiration-se,ymax=respiration+se),width=0.1,size=1,color='black')+
  geom_point(data=df_3d,aes(colour = factor(ID)),position=position_jitter(width=0.1),  size = 2.8,alpha=0.1,show.legend = FALSE) +
  scale_x_discrete(guide = guide_axis(angle = 45),limits =c('CT','3_in','3'))+
  geom_hline(yintercept=mean_WSA,linetype='dashed')+
  scale_color_manual(values=c('khaki4','burlywood4','burlywood4'))+
  xlab('')+
  stat_halfeye(data = AGGse_WSA, aes(x = ID, y = respiration, ymin=respiration-se,ymax=respiration+se), scale=0.3, alpha =1,color='black') + 
  theme(axis.text = element_text(size = 12),axis.title = element_text(size = 12),plot.title = element_text(size = 12))+
  ylab('respiration_no drought')+
  theme_bw()

p_respiration_d<- ggplot(data=df_3d,aes(x=ID,y=respiration,group=ID,color=ID))+
  geom_errorbar(data=AGGse_WSA,aes(x=as.numeric(factor(ID)),ymin=respiration-se,ymax=respiration+se),width=0.1,size=1,color='black')+
  geom_point(data=df_3d,aes(colour = factor(ID)),position=position_jitter(width=0.1),  size = 2.8,alpha=0.1,show.legend = FALSE) +
  scale_x_discrete(guide = guide_axis(angle = 45),limits =c('CT','3_in','3'))+
  geom_hline(yintercept=mean_WSA,linetype='dashed')+
  scale_color_manual(values=c('khaki4','burlywood4','burlywood4'))+
  xlab('') +
  stat_halfeye(data = AGGse_WSA, aes(x = ID, y = respiration, ymin=respiration-se,ymax=respiration+se), scale=0.3, alpha =1,color='black') + 
  theme(axis.text = element_text(size = 12),axis.title = element_text(size = 12),plot.title = element_text(size = 12))+
  ylab('respiration_drought')+
  theme_bw()
p_respiration_ct+p_respiration_d

#### ace ####
setwd('C:/Huiying data/2nd_chapter/main_exp')
df<- read.csv('df_main.csv')
df_3<-df%>%
  filter(lv!=1,lv!=6,lv!=9)
df_3d<- df_3%>%
  filter(D==0)
multi.two.group.unpaired <- 
  df_3d %>%
  dabest(ID,ace, 
         idx = list(c('CT','3_in','3')),
         paired = FALSE)
multi.two.group.unpaired.meandiff <- mean_diff(multi.two.group.unpaired)
multi.two.group.unpaired.meandiff %>% plot()

dat<-multi.two.group.unpaired.meandiff$result 
AGGse_WSA<-summarySE(df_3d, measurevar="ace", groupvars=c("ID"))
dat_WSA<-dat[c(1,2,8,10,11)] 

calculate_WSA<- subset(df_3d,df_3d$ID=='CT')
mean_WSA<- mean(calculate_WSA$ace)
dat_WSA$bca_ci_low<- c(dat_WSA$bca_ci_low+mean_WSA)
dat_WSA$bca_ci_high<- c(dat_WSA$bca_ci_high+mean_WSA)
dat_WSA$difference<- c(dat_WSA$difference+mean_WSA)
colnames(dat_WSA)[2] <- 'Treatment3'
df_3d$ID<-factor(df_3d$ID, levels=c('CT','3_in','3'))
AGGse_WSA$ID<- factor(AGGse_WSA$ID, levels=c('CT','3_in','3'))

p_ace_ct<- ggplot(data=df_3d,aes(x=ID,y=ace,group=ID,color=ID))+
  geom_errorbar(data=AGGse_WSA,aes(x=as.numeric(factor(ID)),ymin=ace-se,ymax=ace+se),width=0.1,size=1,color='black')+
  geom_point(data=df_3d,aes(colour = factor(ID)),position=position_jitter(width=0.1),  size = 2.8,alpha=0.1,show.legend = FALSE) +
  scale_x_discrete(guide = guide_axis(angle = 45),limits =c('CT','3_in','3'))+
  geom_hline(yintercept=mean_WSA,linetype='dashed')+
  scale_color_manual(values=c('khaki4','burlywood4','burlywood4'))+
  xlab('')+
  ylim(0.2,1)+
  stat_halfeye(data = AGGse_WSA, aes(x = ID, y = ace, ymin=ace-se,ymax=ace+se), scale=0.3, alpha =1,color='black') + 
  theme(axis.text = element_text(size = 12),axis.title = element_text(size = 12),plot.title = element_text(size = 12))+
  ylab('ace_no drought')+
  theme_bw()

p_ace_d<- ggplot(data=df_3d,aes(x=ID,y=ace,group=ID,color=ID))+
  geom_errorbar(data=AGGse_WSA,aes(x=as.numeric(factor(ID)),ymin=ace-se,ymax=ace+se),width=0.1,size=1,color='black')+
  geom_point(data=df_3d,aes(colour = factor(ID)),position=position_jitter(width=0.1),  size = 2.8,alpha=0.1,show.legend = FALSE) +
  scale_x_discrete(guide = guide_axis(angle = 45),limits =c('CT','3_in','3'))+
  geom_hline(yintercept=mean_WSA,linetype='dashed')+
  scale_color_manual(values=c('khaki4','burlywood4','burlywood4'))+
  xlab('') +
  ylim(0.2,1)+
  stat_halfeye(data = AGGse_WSA, aes(x = ID, y = ace, ymin=ace-se,ymax=ace+se), scale=0.3, alpha =1,color='black') + 
  theme(axis.text = element_text(size = 12),axis.title = element_text(size = 12),plot.title = element_text(size = 12))+
  ylab('ace_drought')+
  theme_bw()
p_ace_ct+p_ace_d

#### phos ####
setwd('C:/Huiying data/2nd_chapter/main_exp')
df<- read.csv('df_main.csv')
df_3<-df%>%
  filter(lv!=1,lv!=6,lv!=9)
df_3d<- df_3%>%
  filter(D==1)
multi.two.group.unpaired <- 
  df_3d %>%
  dabest(ID,phos, 
         idx = list(c('CT','3_in','3')),
         paired = FALSE)
multi.two.group.unpaired.meandiff <- mean_diff(multi.two.group.unpaired)
multi.two.group.unpaired.meandiff %>% plot()

dat<-multi.two.group.unpaired.meandiff$result 
AGGse_WSA<-summarySE(df_3d, measurevar="phos", groupvars=c("ID"))
dat_WSA<-dat[c(1,2,8,10,11)] 

calculate_WSA<- subset(df_3d,df_3d$ID=='CT')
mean_WSA<- mean(calculate_WSA$phos)
dat_WSA$bca_ci_low<- c(dat_WSA$bca_ci_low+mean_WSA)
dat_WSA$bca_ci_high<- c(dat_WSA$bca_ci_high+mean_WSA)
dat_WSA$difference<- c(dat_WSA$difference+mean_WSA)
colnames(dat_WSA)[2] <- 'Treatment3'
df_3d$ID<-factor(df_3d$ID, levels=c('CT','3_in','3'))
AGGse_WSA$ID<- factor(AGGse_WSA$ID, levels=c('CT','3_in','3'))

p_phos_ct<- ggplot(data=df_3d,aes(x=ID,y=phos,group=ID,color=ID))+
  geom_errorbar(data=AGGse_WSA,aes(x=as.numeric(factor(ID)),ymin=phos-se,ymax=phos+se),width=0.1,size=1,color='black')+
  geom_point(data=df_3d,aes(colour = factor(ID)),position=position_jitter(width=0.1),  size = 2.8,alpha=0.1,show.legend = FALSE) +
  scale_x_discrete(guide = guide_axis(angle = 45),limits =c('CT','3_in','3'))+
  geom_hline(yintercept=mean_WSA,linetype='dashed')+
  scale_color_manual(values=c('khaki4','burlywood4','burlywood4'))+
  xlab('')+
  ylim(1,4)+
  stat_halfeye(data = AGGse_WSA, aes(x = ID, y = phos, ymin=phos-se,ymax=phos+se), scale=0.3, alpha =1,color='black') + 
  theme(axis.text = element_text(size = 12),axis.title = element_text(size = 12),plot.title = element_text(size = 12))+
  ylab('phos_no drought')+
  theme_bw()

p_phos_d<- ggplot(data=df_3d,aes(x=ID,y=phos,group=ID,color=ID))+
  geom_errorbar(data=AGGse_WSA,aes(x=as.numeric(factor(ID)),ymin=phos-se,ymax=phos+se),width=0.1,size=1,color='black')+
  geom_point(data=df_3d,aes(colour = factor(ID)),position=position_jitter(width=0.1),  size = 2.8,alpha=0.1,show.legend = FALSE) +
  scale_x_discrete(guide = guide_axis(angle = 45),limits =c('CT','3_in','3'))+
  geom_hline(yintercept=mean_WSA,linetype='dashed')+
  scale_color_manual(values=c('khaki4','burlywood4','burlywood4'))+
  xlab('') +  
  ylim(1,4)+
  stat_halfeye(data = AGGse_WSA, aes(x = ID, y = phos, ymin=phos-se,ymax=phos+se), scale=0.3, alpha =1,color='black') + 
  theme(axis.text = element_text(size = 12),axis.title = element_text(size = 12),plot.title = element_text(size = 12))+
  ylab('phos_drought')+
  theme_bw()
p_phos_ct+p_phos_d
