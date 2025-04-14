## exp main ##


library('ggplot2')
library('dabestr')
library('Rmisc')
library(tidyverse)

library(randomForest) #na.roughfix was used in this package
library(ggepi)
library(ggridges)
library(patchwork)
library(party)
library(caret)
library(dplyr)
library(tidyverse)

#### A_ bootstrap 5000 times to generate drought buffer plot ####

setwd('C:/Huiying data/2nd_chapter/main_exp')
df<- read.csv('df_rf.csv')
df<- filter(df,df$ID!='3_in')
df$ID=factor(df$ID, levels = c('CT','I1','I2','I3','C1','C2','C3','M1','M2','M3','3','6','9'))
responses = c('respiration','aboveground','belowground','ace','phos','cello','gluco','WSA','PH')
stressors = c('CT','I1','I2','I3','C1','C2','C3','M1','M2','M3','3','6','9')
treatment = as.vector(unique(df$ID))

  
n_iter<- 5000
BootStrap_mean = function(response, data=df, target = treatment, n_perm = n_iter){
  summary = list()
  for(treatment in target){
    bs = numeric(0)
      population_D <- data[data["ID"]==treatment& data["D"] == '1', response]
      population_CT <- data[data["ID"]==treatment& data["D"] == '0', response]
    size_D <- length(population_D)
    size_CT <- length(population_CT) 
    
    for (i in 1:n_iter) {
      k_CT = mean(sample(population_CT, size_CT, replace = T), na.rm = TRUE)
      k_D = mean(sample(population_D, size_D, replace = T), na.rm = TRUE)
      bs = append(bs, k_CT - k_D)
      i=i+1
    }
    summary[[treatment]] = c(quantile(bs, .025,na.rm = TRUE), mean(bs,na.rm = TRUE), quantile(bs, .975,na.rm = TRUE))
    names(summary[[treatment]]) = c("2.5%", "mean", "97.5%")
  }
  summary = t(data.frame(summary))
  summary = data.frame("target" = target, summary); row.names(summary) = c()
  return(summary)
}




response_mean_all = list()

EF     = list()
for (i_response in responses) {
  #bootstrap mean
  response_mean = BootStrap_mean(i_response)
  response_mean_all[[i_response]] = response_mean
  response_mean$target= factor(response_mean$target,levels=c('CT','I1','I2','I3','C1','C2','C3','M1','M2','M3','3','6','9'))
  EF[[i_response]] =  local({
    i_response = i_response
    Mycolor=c("#518463","#185050","#185050","#185050","#185050","#185050","#185050","#185050","#185050","#185050","#307068","#307068","#307068","#307068","#307068")
    ggplot() +
      coord_flip() +
      theme_bw()+
      theme(legend.position = 'none', axis.title.x=element_blank())+
      xlab(i_response) + 
      scale_fill_manual(values  = Mycolor)+
      scale_alpha_manual(values = c(rep(0.5,17)))+
      geom_estci(data=response_mean, aes(x = mean, y = target, xmin=X2.5., xmax=X97.5., color =target),  
                 xintercept=response_mean[1,"mean"],center.linecolour = "black",
                 size=0.6, ci.linesize = 0.5)+
      scale_color_manual(values  = Mycolor)
    
    
  })
}

EF[2]

#### till here, drought buffer ef plotted ####
#### B_ two set of raw data plot ####
library(gghalves)
df<- read.csv('df_main.csv')
df<- filter(df,df$ID!='3_in')
Mycolor=c("#518463","#4590BF")
df$D <- as.factor(df$D)
df$ID<- factor(df$ID,levels=c('CT','I1','I2','I3','C1','C2','C3','M1','M2','M3','3','6','9'))

## green no drought, blue drought ##
ggplot(df)+
  coord_flip()+
  theme_bw()+
  theme(legend.position = 'none', axis.title.x=element_blank())+
  geom_density_ridges(aes(x=WSA,y=ID,fill=D,group=interaction(ID,D)),jittered_points = TRUE, 
                      alpha = .5,
                      position = position_points_jitter(height = .2, yoffset = .15),
                      point_size = 1.5, point_alpha = .3, 
                      scale = .55)+
  scale_fill_manual(values  = Mycolor)

#### till here ####
#### C_ at 3 factor level, 3_in and 3_out analysis ####
library(ggridges)
library('Rmisc')
setwd('C:/Huiying data/2nd_chapter/main_exp')
df<- read.csv('df_main.csv')
df_3<-df%>%
  filter(lv!=1,lv!=6,lv!=9)

df_3d<- df_3%>%
  filter(D==1)
df_3ct<- df_3%>%
  filter(D==0)
shape_values <- c("triangle" = 17, "circle" = 16)

## significance calculation ##
library(tibble)
ph_1d<- aov(PH~ID, data =df_3d)
ph_d<- as.matrix(TukeyHSD(ph_1d)[["ID"]])
ph_d<- data.frame(ph_d,header=F)
ph_d<- ph_d[-1,]
ph_d$group<- c('drought')

ph_1ct<- aov(PH~ID, data =df_3ct)
ph_ct<- as.matrix(TukeyHSD(ph_1ct)[["ID"]])
ph_ct<- data.frame(ph_ct,header=F)
ph_ct<- ph_ct[-1,]
ph_ct$group<- c('no_drought')

ph<- rbind(ph_ct,ph_d)
ph$shap <- ifelse(ph$p.adj < 0.05, "triangle", "circle")
ph$ID<- c('3','3_in','3','3_in')


WSA_1d<- aov(WSA~ID, data =df_3d)
WSA_d<- as.matrix(TukeyHSD(WSA_1d)[["ID"]])
WSA_d<- data.frame(WSA_d,header=F)
WSA_d<- WSA_d[-1,]
WSA_d$group<- c('drought')

WSA_1ct<- aov(WSA~ID, data =df_3ct)
WSA_ct<- as.matrix(TukeyHSD(WSA_1ct)[["ID"]])
WSA_ct<- data.frame(WSA_ct,header=F)
WSA_ct<- WSA_ct[-1,]
WSA_ct$group<- c('no_drought')

WSA<- rbind(WSA_ct,WSA_d)
WSA$ID<- c('3','3_in','3','3_in')
WSA$shape <- ifelse(WSA$p.adj < 0.05, "triangle", "circle")


aboveground_1d<- aov(aboveground~ID, data =df_3d)
aboveground_d<- as.matrix(TukeyHSD(aboveground_1d)[["ID"]])
aboveground_d<- data.frame(aboveground_d,header=F)
aboveground_d<- aboveground_d[-1,]
aboveground_d$group<- c('drought')

aboveground_1ct<- aov(aboveground~ID, data =df_3ct)
aboveground_ct<- as.matrix(TukeyHSD(aboveground_1ct)[["ID"]])
aboveground_ct<- data.frame(aboveground_ct,header=F)
aboveground_ct<- aboveground_ct[-1,]
aboveground_ct$group<- c('no_drought')

aboveground<- rbind(aboveground_ct,aboveground_d)
aboveground$ID<- c('3','3_in','3','3_in')
aboveground$shape <- ifelse(aboveground$p.adj < 0.05, "triangle", "circle")

belowground_1d<- aov(belowground~ID, data =df_3d)
belowground_d<- as.matrix(TukeyHSD(belowground_1d)[["ID"]])
belowground_d<- data.frame(belowground_d,header=F)
belowground_d<- belowground_d[-1,]
belowground_d$group<- c('drought')

belowground_1ct<- aov(belowground~ID, data =df_3ct)
belowground_ct<- as.matrix(TukeyHSD(belowground_1ct)[["ID"]])
belowground_ct<- data.frame(belowground_ct,header=F)
belowground_ct<- belowground_ct[-1,]
belowground_ct$group<- c('no_drought')

belowground<- rbind(belowground_ct,belowground_d)
belowground$ID<- c('3','3_in','3','3_in')
belowground$shape <- ifelse(belowground$p.adj < 0.05, "triangle", "circle")


respiration_1d<- aov(respiration~ID, data =df_3d)
respiration_d<- as.matrix(TukeyHSD(respiration_1d)[["ID"]])
respiration_d<- data.frame(respiration_d,header=F)
respiration_d<- respiration_d[-1,]
respiration_d$group<- c('drought')

respiration_1ct<- aov(respiration~ID, data =df_3ct)
respiration_ct<- as.matrix(TukeyHSD(respiration_1ct)[["ID"]])
respiration_ct<- data.frame(respiration_ct,header=F)
respiration_ct<- respiration_ct[-1,]
respiration_ct$group<- c('no_drought')

respiration<- rbind(respiration_ct,respiration_d)
respiration$ID<- c('3','3_in','3','3_in')
respiration$shape <- ifelse(respiration$p.adj < 0.05, "triangle", "circle")

gluco_1d<- aov(gluco~ID, data =df_3d)
gluco_d<- as.matrix(TukeyHSD(gluco_1d)[["ID"]])
gluco_d<- data.frame(gluco_d,header=F)
gluco_d<- gluco_d[-1,]
gluco_d$group<- c('drought')

gluco_1ct<- aov(gluco~ID, data =df_3ct)
gluco_ct<- as.matrix(TukeyHSD(gluco_1ct)[["ID"]])
gluco_ct<- data.frame(gluco_ct,header=F)
gluco_ct<- gluco_ct[-1,]
gluco_ct$group<- c('no_drought')

gluco<- rbind(gluco_ct,gluco_d)
gluco$ID<- c('3','3_in','3','3_in')
gluco$shape <- ifelse(gluco$p.adj < 0.05, "triangle", "circle")

cello_1d<- aov(cello~ID, data =df_3d)
cello_d<- as.matrix(TukeyHSD(cello_1d)[["ID"]])
cello_d<- data.frame(cello_d,header=F)
cello_d<- cello_d[-1,]
cello_d$group<- c('drought')

cello_1ct<- aov(cello~ID, data =df_3ct)
cello_ct<- as.matrix(TukeyHSD(cello_1ct)[["ID"]])
cello_ct<- data.frame(cello_ct,header=F)
cello_ct<- cello_ct[-1,]
cello_ct$group<- c('no_drought')

cello<- rbind(cello_ct,cello_d)
cello$ID<- c('3','3_in','3','3_in')
cello$shape <- ifelse(cello$p.adj < 0.05, "triangle", "circle")

phos_1d<- aov(phos~ID, data =df_3d)
phos_d<- as.matrix(TukeyHSD(phos_1d)[["ID"]])
phos_d<- data.frame(phos_d,header=F)
phos_d<- phos_d[-1,]
phos_d$group<- c('drought')

phos_1ct<- aov(phos~ID, data =df_3ct)
phos_ct<- as.matrix(TukeyHSD(phos_1ct)[["ID"]])
phos_ct<- data.frame(phos_ct,header=F)
phos_ct<- phos_ct[-1,]
phos_ct$group<- c('no_drought')

phos<- rbind(phos_ct,phos_d)
phos$ID<- c('3','3_in','3','3_in')
phos$shape <- ifelse(phos$p.adj < 0.05, "triangle", "circle")

ace_1d<- aov(ace~ID, data =df_3d)
ace_d<- as.matrix(TukeyHSD(ace_1d)[["ID"]])
ace_d<- data.frame(ace_d,header=F)
ace_d<- ace_d[-1,]
ace_d$group<- c('drought')

ace_1ct<- aov(ace~ID, data =df_3ct)
ace_ct<- as.matrix(TukeyHSD(ace_1ct)[["ID"]])
ace_ct<- data.frame(ace_ct,header=F)
ace_ct<- ace_ct[-1,]
ace_ct$group<- c('no_drought')

ace<- rbind(ace_ct,ace_d)
ace$ID<- c('3','3_in','3','3_in')
ace$shape <- ifelse(ace$p.adj < 0.05, "triangle", "circle")

## ph ##
AGGse_PH_d<-summarySE(df_3d, measurevar="PH", groupvars=c("ID"))
AGGse_PH_d$D<- c('drought')
AGGse_PH_d$ID<- as.character(AGGse_PH_d$ID)
AGGse_PH_ct<-summarySE(df_3ct, measurevar="PH", groupvars=c("ID"))
AGGse_PH_ct$D<- c('no-drought')
AGGse_PH_ct$ID<- as.character(AGGse_PH_ct$ID)
AGGse_PH<- rbind(AGGse_PH_ct,AGGse_PH_d)
AGGse_PH$D<- factor(AGGse_PH$D,levels=c('drought','no-drought'))
AGGse_PH_ct$ID<- factor(AGGse_PH_ct$ID,levels=c('CT','3','3_in'))
AGGse_PH_d$ID<- factor(AGGse_PH_d$ID,levels=c('CT','3','3_in'))
df_3$ID<- factor(df_3$ID,levels=c('CT','3','3_in'))

calculate_PH<- subset(df_3ct,df_3ct$ID=='CT')
mean_PH<- mean(calculate_PH$PH)

Mycolor=c("#518463","#96C2D4")
df_3$D<- factor(df_3$D,levels=c('0','1'))

filtered_padj <- ph %>% 
  filter(group == "no_drought") %>% 
  select(ID,p.adj)

# Add filtered_padj as a new column to AGGse_PH_ct
AGGse_PH_ct <- left_join(AGGse_PH_ct, filtered_padj,by='ID')

filtered_padj_d <- ph%>% 
  filter(group == "drought") %>% 
  select(ID,p.adj)

# Add filtered_padj as a new column to AGGse_PH_ct
AGGse_PH_d <- left_join(AGGse_PH_d, filtered_padj_d,by='ID')

p_ph_0<- ggplot(df_3)+
  coord_flip()+
  theme_bw()+
  theme(axis.title.x=element_blank())+
  geom_density_ridges(aes(x=PH,y=ID,fill=D,group=interaction(ID,D)),jittered_points = TRUE, 
                      alpha = .5,
                      position = position_points_jitter(height = .1, yoffset = -.15),
                      point_size = 2, point_alpha = .2, 
                      scale = .55)+
  scale_fill_manual(values  = Mycolor)

p_ph<- p_ph_0+ geom_errorbar(data = AGGse_PH_ct,
              aes(y = ID, xmin = PH - se, xmax = PH + se), 
              position = position_nudge(y = -0.2), color = '#3D9F3C', 
              width = 0.05, size =1) +
  geom_errorbar(data = AGGse_PH_d,
                aes(y = ID, xmin = PH - se, xmax = PH + se), 
                position = position_nudge(y = -0.1), color = '#3492B2', 
                width = 0.05, size = 1)+
  geom_point(data = AGGse_PH_ct,
             aes(y = ID, x = PH, shape =ifelse(p.adj < 0.05, "triangle", "circle")),
             size = 2.5, position = position_nudge(y = -0.2))+
  geom_point(data = AGGse_PH_d,
             aes(y = ID, x = PH, shape =ifelse(p.adj < 0.05, "triangle", "circle")),
             size = 2.5, position = position_nudge(y = -0.1))+
  scale_shape_manual(values = shape_values)

## wsa ##
AGGse_WSA_d<-summarySE(df_3d, measurevar="WSA", groupvars=c("ID"))
AGGse_WSA_d$D<- c('drought')
AGGse_WSA_d$ID<- as.character(AGGse_WSA_d$ID)
AGGse_WSA_ct<-summarySE(df_3ct, measurevar="WSA", groupvars=c("ID"))
AGGse_WSA_ct$D<- c('no-drought')
AGGse_WSA_ct$ID<- as.character(AGGse_WSA_ct$ID)
AGGse_WSA<- rbind(AGGse_WSA_ct,AGGse_WSA_d)
AGGse_WSA$D<- factor(AGGse_WSA$D,levels=c('drought','no-drought'))
AGGse_WSA_ct$ID<- factor(AGGse_WSA_ct$ID,levels=c('CT','3','3_in'))
AGGse_WSA_d$ID<- factor(AGGse_WSA_d$ID,levels=c('CT','3','3_in'))
df_3$ID<- factor(df_3$ID,levels=c('CT','3','3_in'))

calculate_WSA<- subset(df_3d,df_3d$ID=='CT')
mean_WSA<- mean(calculate_WSA$WSA)
Mycolor=c("#518463","#4590BF")
Mycolor=c("#518463","#96C2D4")
df_3$D<- factor(df_3$D,levels=c('0','1'))

filtered_WSA <- WSA %>% 
  filter(group == "no_drought") %>% 
  select(ID,p.adj)

# Add filtered_padj as a new column to AGGse_WSA_ct
AGGse_WSA_ct <- left_join(AGGse_WSA_ct, filtered_padj,by='ID')

filtered_padj_d <- WSA%>% 
  filter(group == "drought") %>% 
  select(ID,p.adj)

# Add filtered_padj as a new column to AGGse_WSA_ct
AGGse_WSA_d <- left_join(AGGse_WSA_d, filtered_padj_d,by='ID')

p_WSA_0<- ggplot(df_3)+
  coord_flip()+
  theme_bw()+
  theme(legend.position = 'none', axis.title.x=element_blank())+
  geom_density_ridges(aes(x=WSA,y=ID,fill=D,group=interaction(ID,D)),jittered_points = TRUE, 
                      alpha = .5,
                      position = position_points_jitter(height = .1, yoffset = -.15),
                      point_size = 2, point_alpha = .2, 
                      scale = .55)+
  scale_fill_manual(values  = Mycolor)
p_WSA<- p_WSA_0+geom_errorbar(data = AGGse_WSA_ct, aes(y = ID, xmin = WSA - ci, xmax = WSA + ci), 
                                                             position = position_nudge(y = -0.2), color = '#3D9F3C', 
                                                             width = 0.05, size =1) +
  geom_errorbar(data = AGGse_WSA_d,
                aes(y = ID, xmin = WSA - ci, xmax = WSA + ci), 
                position = position_nudge(y = -0.1), color = '#3492B2', 
                width = 0.05, size = 1) +
  geom_point(data = AGGse_WSA_ct,
             aes(y = ID, x = WSA, shape =ifelse(p.adj < 0.05, "triangle", "circle")),
             size = 2.5, position = position_nudge(y = -0.2))+
  geom_point(data = AGGse_WSA_d,
             aes(y = ID, x = WSA, shape =ifelse(p.adj < 0.05, "triangle", "circle")),
             size = 2.5, position = position_nudge(y = -0.1))+
  scale_shape_manual(values = shape_values)

## respiration ##
AGGse_respiration_d<-summarySE(df_3d, measurevar="respiration", groupvars=c("ID"))
AGGse_respiration_d$D<- c('drought')
AGGse_respiration_d$ID<- as.character(AGGse_respiration_d$ID)
AGGse_respiration_ct<-summarySE(df_3ct, measurevar="respiration", groupvars=c("ID"))
AGGse_respiration_ct$D<- c('no-drought')
AGGse_respiration_ct$ID<- as.character(AGGse_respiration_ct$ID)
AGGse_respiration<- rbind(AGGse_respiration_ct,AGGse_respiration_d)
AGGse_respiration$D<- factor(AGGse_respiration$D,levels=c('drought','no-drought'))
AGGse_respiration_ct$ID<- factor(AGGse_respiration_ct$ID,levels=c('CT','3','3_in'))
AGGse_respiration_d$ID<- factor(AGGse_respiration_d$ID,levels=c('CT','3','3_in'))
df_3$ID<- factor(df_3$ID,levels=c('CT','3','3_in'))

calculate_respiration<- subset(df_3d,df_3d$ID=='CT')
mean_respiration<- mean(calculate_respiration$respiration)
Mycolor=c("#518463","#4590BF")
Mycolor=c("#518463","#96C2D4")
df_3$D<- factor(df_3$D,levels=c('0','1'))


filtered_respiration <- respiration %>% 
  filter(group == "no_drought") %>% 
  select(ID,p.adj)

# Add filtered_padj as a new column to AGGse_respiration_ct
AGGse_respiration_ct <- left_join(AGGse_respiration_ct, filtered_padj,by='ID')

filtered_padj_d <- respiration%>% 
  filter(group == "drought") %>% 
  select(ID,p.adj)

# Add filtered_padj as a new column to AGGse_respiration_ct
AGGse_respiration_d <- left_join(AGGse_respiration_d, filtered_padj_d,by='ID')


p_respiration_0<- ggplot(df_3)+
  coord_flip()+
  theme_bw()+
  theme(legend.position = 'none', axis.title.x=element_blank())+
  geom_density_ridges(aes(x=respiration,y=ID,fill=D,group=interaction(ID,D)),jittered_points = TRUE, 
                      alpha = .5,
                      position = position_points_jitter(height = .1, yoffset = -.15),
                      point_size = 2, point_alpha = .2, 
                      scale = .55)+
  scale_fill_manual(values  = Mycolor)
p_respiration<- p_respiration_0+geom_errorbar(data = AGGse_respiration_ct, aes(y = ID, xmin = respiration - ci, xmax = respiration + ci), 
                              position = position_nudge(y = -0.2), color = '#3D9F3C', 
                              width = 0.05, size =1) +
  geom_errorbar(data = AGGse_respiration_d,
                aes(y = ID, xmin = respiration - ci, xmax = respiration + ci), 
                position = position_nudge(y = -0.1), color = '#3492B2', 
                width = 0.05, size = 1) +
  geom_point(data = AGGse_respiration_ct,
             aes(y = ID, x = respiration, shape =ifelse(p.adj < 0.05, "triangle", "circle")),
             size = 2.5, position = position_nudge(y = -0.2))+
  geom_point(data = AGGse_respiration_d,
             aes(y = ID, x = respiration, shape =ifelse(p.adj < 0.05, "triangle", "circle")),
             size = 2.5, position = position_nudge(y = -0.1))+
  scale_shape_manual(values = shape_values)

## aboveground ##
AGGse_aboveground_d<-summarySE(df_3d, measurevar="aboveground", groupvars=c("ID"))
AGGse_aboveground_d$D<- c('drought')
AGGse_aboveground_d$ID<- as.character(AGGse_aboveground_d$ID)
AGGse_aboveground_ct<-summarySE(df_3ct, measurevar="aboveground", groupvars=c("ID"))
AGGse_aboveground_ct$D<- c('no-drought')
AGGse_aboveground_ct$ID<- as.character(AGGse_aboveground_ct$ID)
AGGse_aboveground<- rbind(AGGse_aboveground_ct,AGGse_aboveground_d)
AGGse_aboveground$D<- factor(AGGse_aboveground$D,levels=c('drought','no-drought'))
AGGse_aboveground_ct$ID<- factor(AGGse_aboveground_ct$ID,levels=c('CT','3','3_in'))
AGGse_aboveground_d$ID<- factor(AGGse_aboveground_d$ID,levels=c('CT','3','3_in'))
df_3$ID<- factor(df_3$ID,levels=c('CT','3','3_in'))

calculate_aboveground<- subset(df_3d,df_3d$ID=='CT')
mean_aboveground<- mean(calculate_aboveground$aboveground)
Mycolor=c("#518463","#4590BF")
Mycolor=c("#518463","#96C2D4")
df_3$D<- factor(df_3$D,levels=c('0','1'))


filtered_aboveground <- aboveground %>% 
  filter(group == "no_drought") %>% 
  select(ID,p.adj)

# Add filtered_padj as a new column to AGGse_aboveground_ct
AGGse_aboveground_ct <- left_join(AGGse_aboveground_ct, filtered_padj,by='ID')

filtered_padj_d <- aboveground%>% 
  filter(group == "drought") %>% 
  select(ID,p.adj)

# Add filtered_padj as a new column to AGGse_aboveground_ct
AGGse_aboveground_d <- left_join(AGGse_aboveground_d, filtered_padj_d,by='ID')

p_aboveground_0<- ggplot(df_3)+
  coord_flip()+
  theme_bw()+
  theme(legend.position = 'none', axis.title.x=element_blank())+
  geom_density_ridges(aes(x=aboveground,y=ID,fill=D,group=interaction(ID,D)),jittered_points = TRUE, 
                      alpha = .5,
                      position = position_points_jitter(height = .1, yoffset = -.15),
                      point_size = 2, point_alpha = .2, 
                      scale = .55)+
  scale_fill_manual(values  = Mycolor)
p_aboveground<- p_aboveground_0+geom_errorbar(data = AGGse_aboveground_ct, aes(y = ID, xmin = aboveground - ci, xmax = aboveground + ci), 
                                              position = position_nudge(y = -0.2), color = '#3D9F3C', 
                                              width = 0.05, size =1) +
  geom_errorbar(data = AGGse_aboveground_d,
                aes(y = ID, xmin = aboveground - ci, xmax = aboveground + ci), 
                position = position_nudge(y = -0.1), color = '#3492B2', 
                width = 0.05, size = 1) +
  geom_point(data = AGGse_aboveground_ct,
             aes(y = ID, x = aboveground, shape =ifelse(p.adj < 0.05, "triangle", "circle")),
             size = 2.5, position = position_nudge(y = -0.2))+
  geom_point(data = AGGse_aboveground_d,
             aes(y = ID, x = aboveground, shape =ifelse(p.adj < 0.05, "triangle", "circle")),
             size = 2.5, position = position_nudge(y = -0.1))+
  scale_shape_manual(values = shape_values)

## belowground ##
AGGse_belowground_d<-summarySE(df_3d, measurevar="belowground", groupvars=c("ID"))
AGGse_belowground_d$D<- c('drought')
AGGse_belowground_d$ID<- as.character(AGGse_belowground_d$ID)
AGGse_belowground_ct<-summarySE(df_3ct, measurevar="belowground", groupvars=c("ID"))
AGGse_belowground_ct$D<- c('no-drought')
AGGse_belowground_ct$ID<- as.character(AGGse_belowground_ct$ID)
AGGse_belowground<- rbind(AGGse_belowground_ct,AGGse_belowground_d)
AGGse_belowground$D<- factor(AGGse_belowground$D,levels=c('drought','no-drought'))
AGGse_belowground_ct$ID<- factor(AGGse_belowground_ct$ID,levels=c('CT','3','3_in'))
AGGse_belowground_d$ID<- factor(AGGse_belowground_d$ID,levels=c('CT','3','3_in'))
df_3$ID<- factor(df_3$ID,levels=c('CT','3','3_in'))

calculate_belowground<- subset(df_3d,df_3d$ID=='CT')
mean_belowground<- mean(calculate_belowground$belowground)
Mycolor=c("#518463","#4590BF")
Mycolor=c("#518463","#96C2D4")
df_3$D<- factor(df_3$D,levels=c('0','1'))

filtered_belowground <- belowground %>% 
  filter(group == "no_drought") %>% 
  select(ID,p.adj)

# Add filtered_padj as a new column to AGGse_belowground_ct
AGGse_belowground_ct <- left_join(AGGse_belowground_ct, filtered_padj,by='ID')

filtered_padj_d <- belowground%>% 
  filter(group == "drought") %>% 
  select(ID,p.adj)

# Add filtered_padj as a new column to AGGse_belowground_ct
AGGse_belowground_d <- left_join(AGGse_belowground_d, filtered_padj_d,by='ID')


p_belowground_0<- ggplot(df_3)+
  coord_flip()+
  theme_bw()+
  theme(legend.position = 'none', axis.title.x=element_blank())+
  geom_density_ridges(aes(x=belowground,y=ID,fill=D,group=interaction(ID,D)),jittered_points = TRUE, 
                      alpha = .5,
                      position = position_points_jitter(height = .1, yoffset = -.15),
                      point_size = 2, point_alpha = .2, 
                      scale = .55)+
  scale_fill_manual(values  = Mycolor)
p_belowground<- p_belowground_0+geom_errorbar(data = AGGse_belowground_ct, aes(y = ID, xmin = belowground - ci, xmax = belowground + ci), 
                                              position = position_nudge(y = -0.2), color = '#3D9F3C', 
                                              width = 0.05, size =1) +
  geom_errorbar(data = AGGse_belowground_d,
                aes(y = ID, xmin = belowground - ci, xmax = belowground + ci), 
                position = position_nudge(y = -0.1), color = '#3492B2', 
                width = 0.05, size = 1) +
  geom_point(data = AGGse_belowground_ct,
             aes(y = ID, x = belowground, shape =ifelse(p.adj < 0.05, "triangle", "circle")),
             size = 2.5, position = position_nudge(y = -0.2))+
  geom_point(data = AGGse_belowground_d,
             aes(y = ID, x = belowground, shape =ifelse(p.adj < 0.05, "triangle", "circle")),
             size = 2.5, position = position_nudge(y = -0.1))+
  scale_shape_manual(values = shape_values)

## phos ##
AGGse_phos_d<-summarySE(df_3d, measurevar="phos", groupvars=c("ID"))
AGGse_phos_d$D<- c('drought')
AGGse_phos_d$ID<- as.character(AGGse_phos_d$ID)
AGGse_phos_ct<-summarySE(df_3ct, measurevar="phos", groupvars=c("ID"))
AGGse_phos_ct$D<- c('no-drought')
AGGse_phos_ct$ID<- as.character(AGGse_phos_ct$ID)
AGGse_phos<- rbind(AGGse_phos_ct,AGGse_phos_d)
AGGse_phos$D<- factor(AGGse_phos$D,levels=c('drought','no-drought'))
AGGse_phos_ct$ID<- factor(AGGse_phos_ct$ID,levels=c('CT','3','3_in'))
AGGse_phos_d$ID<- factor(AGGse_phos_d$ID,levels=c('CT','3','3_in'))
df_3$ID<- factor(df_3$ID,levels=c('CT','3','3_in'))

calculate_phos<- subset(df_3d,df_3d$ID=='CT')
mean_phos<- mean(calculate_phos$phos)
Mycolor=c("#518463","#4590BF")
Mycolor=c("#518463","#96C2D4")
df_3$D<- factor(df_3$D,levels=c('0','1'))

filtered_phos <- phos %>% 
  filter(group == "no_drought") %>% 
  select(ID,p.adj)

# Add filtered_padj as a new column to AGGse_phos_ct
AGGse_phos_ct <- left_join(AGGse_phos_ct, filtered_padj,by='ID')

filtered_padj_d <- phos%>% 
  filter(group == "drought") %>% 
  select(ID,p.adj)

# Add filtered_padj as a new column to AGGse_phos_ct
AGGse_phos_d <- left_join(AGGse_phos_d, filtered_padj_d,by='ID')


p_phos_0<- ggplot(df_3)+
  coord_flip()+
  theme_bw()+
  theme(legend.position = 'none', axis.title.x=element_blank())+
  geom_density_ridges(aes(x=phos,y=ID,fill=D,group=interaction(ID,D)),jittered_points = TRUE, 
                      alpha = .5,
                      position = position_points_jitter(height = .1, yoffset = -.15),
                      point_size = 2, point_alpha = .2, 
                      scale = .55)+
  scale_fill_manual(values  = Mycolor)
p_phos<- p_phos_0+geom_errorbar(data = AGGse_phos_ct, aes(y = ID, xmin = phos - ci, xmax = phos + ci), 
                                              position = position_nudge(y = -0.2), color = '#3D9F3C', 
                                              width = 0.05, size =1) +
  geom_errorbar(data = AGGse_phos_d,
                aes(y = ID, xmin = phos - ci, xmax = phos + ci), 
                position = position_nudge(y = -0.1), color = '#3492B2', 
                width = 0.05, size = 1) +
  geom_point(data = AGGse_phos_ct,
             aes(y = ID, x = phos, shape =ifelse(p.adj < 0.05, "triangle", "circle")),
             size = 2.5, position = position_nudge(y = -0.2))+
  geom_point(data = AGGse_phos_d,
             aes(y = ID, x = phos, shape =ifelse(p.adj < 0.05, "triangle", "circle")),
             size = 2.5, position = position_nudge(y = -0.1))+
  scale_shape_manual(values = shape_values)

## ace ##
AGGse_ace_d<-summarySE(df_3d, measurevar="ace", groupvars=c("ID"))
AGGse_ace_d$D<- c('drought')
AGGse_ace_d$ID<- as.character(AGGse_ace_d$ID)
AGGse_ace_ct<-summarySE(df_3ct, measurevar="ace", groupvars=c("ID"))
AGGse_ace_ct$D<- c('no-drought')
AGGse_ace_ct$ID<- as.character(AGGse_ace_ct$ID)
AGGse_ace<- rbind(AGGse_ace_ct,AGGse_ace_d)
AGGse_ace$D<- factor(AGGse_ace$D,levels=c('drought','no-drought'))
AGGse_ace_ct$ID<- factor(AGGse_ace_ct$ID,levels=c('CT','3','3_in'))
AGGse_ace_d$ID<- factor(AGGse_ace_d$ID,levels=c('CT','3','3_in'))
df_3$ID<- factor(df_3$ID,levels=c('CT','3','3_in'))

calculate_ace<- subset(df_3d,df_3d$ID=='CT')
mean_ace<- mean(calculate_ace$ace)
Mycolor=c("#518463","#4590BF")
Mycolor=c("#518463","#96C2D4")
df_3$D<- factor(df_3$D,levels=c('0','1'))


filtered_ace <- ace %>% 
  filter(group == "no_drought") %>% 
  select(ID,p.adj)

# Add filtered_padj as a new column to AGGse_ace_ct
AGGse_ace_ct <- left_join(AGGse_ace_ct, filtered_padj,by='ID')

filtered_padj_d <- ace%>% 
  filter(group == "drought") %>% 
  select(ID,p.adj)

# Add filtered_padj as a new column to AGGse_ace_ct
AGGse_ace_d <- left_join(AGGse_ace_d, filtered_padj_d,by='ID')


p_ace_0<- ggplot(df_3)+
  coord_flip()+
  theme_bw()+
  theme(legend.position = 'none', axis.title.x=element_blank())+
  geom_density_ridges(aes(x=ace,y=ID,fill=D,group=interaction(ID,D)),jittered_points = TRUE, 
                      alpha = .5,
                      position = position_points_jitter(height = .1, yoffset = -.15),
                      point_size = 2, point_alpha = .2, 
                      scale = .55)+
  scale_fill_manual(values  = Mycolor)
p_ace<- p_ace_0+geom_errorbar(data = AGGse_ace_ct, aes(y = ID, xmin = ace - ci, xmax = ace + ci), 
                                position = position_nudge(y = -0.2), color = '#3D9F3C', 
                                width = 0.05, size =1) +
  geom_errorbar(data = AGGse_ace_d,
                aes(y = ID, xmin = ace - ci, xmax = ace + ci), 
                position = position_nudge(y = -0.1), color = '#3492B2', 
                width = 0.05, size = 1) +
  geom_point(data = AGGse_ace_ct,
             aes(y = ID, x = ace, shape =ifelse(p.adj < 0.05, "triangle", "circle")),
             size = 2.5, position = position_nudge(y = -0.2))+
  geom_point(data = AGGse_ace_d,
             aes(y = ID, x = ace, shape =ifelse(p.adj < 0.05, "triangle", "circle")),
             size = 2.5, position = position_nudge(y = -0.1))+
  scale_shape_manual(values = shape_values)

## cello ##
AGGse_cello_d<-summarySE(df_3d, measurevar="cello", groupvars=c("ID"))
AGGse_cello_d$D<- c('drought')
AGGse_cello_d$ID<- as.character(AGGse_cello_d$ID)
AGGse_cello_ct<-summarySE(df_3ct, measurevar="cello", groupvars=c("ID"))
AGGse_cello_ct$D<- c('no-drought')
AGGse_cello_ct$ID<- as.character(AGGse_cello_ct$ID)
AGGse_cello<- rbind(AGGse_cello_ct,AGGse_cello_d)
AGGse_cello$D<- factor(AGGse_cello$D,levels=c('drought','no-drought'))
AGGse_cello_ct$ID<- factor(AGGse_cello_ct$ID,levels=c('CT','3','3_in'))
AGGse_cello_d$ID<- factor(AGGse_cello_d$ID,levels=c('CT','3','3_in'))
df_3$ID<- factor(df_3$ID,levels=c('CT','3','3_in'))

calculate_cello<- subset(df_3d,df_3d$ID=='CT')
mean_cello<- mean(calculate_cello$cello)
Mycolor=c("#518463","#4590BF")
Mycolor=c("#518463","#96C2D4")
df_3$D<- factor(df_3$D,levels=c('0','1'))

filtered_cello <- cello %>% 
  filter(group == "no_drought") %>% 
  select(ID,p.adj)

# Add filtered_padj as a new column to AGGse_cello_ct
AGGse_cello_ct <- left_join(AGGse_cello_ct, filtered_padj,by='ID')

filtered_padj_d <- cello%>% 
  filter(group == "drought") %>% 
  select(ID,p.adj)

# Add filtered_padj as a new column to AGGse_cello_ct
AGGse_cello_d <- left_join(AGGse_cello_d, filtered_padj_d,by='ID')


p_cello_0<- ggplot(df_3)+
  coord_flip()+
  theme_bw()+
  theme(legend.position = 'none', axis.title.x=element_blank())+
  geom_density_ridges(aes(x=cello,y=ID,fill=D,group=interaction(ID,D)),jittered_points = TRUE, 
                      alpha = .5,
                      position = position_points_jitter(height = .1, yoffset = -.15),
                      point_size = 2, point_alpha = .2, 
                      scale = .55)+
  scale_fill_manual(values  = Mycolor)
p_cello<- p_cello_0+geom_errorbar(data = AGGse_cello_ct, aes(y = ID, xmin = cello - ci, xmax = cello + ci), 
                              position = position_nudge(y = -0.2), color = '#3D9F3C', 
                              width = 0.05, size =1) +
  geom_errorbar(data = AGGse_cello_d,
                aes(y = ID, xmin = cello - ci, xmax = cello + ci), 
                position = position_nudge(y = -0.1), color = '#3492B2', 
                width = 0.05, size = 1) +
  geom_point(data = AGGse_cello_ct,
             aes(y = ID, x = cello, shape =ifelse(p.adj < 0.05, "triangle", "circle")),
             size = 2.5, position = position_nudge(y = -0.2))+
  geom_point(data = AGGse_cello_d,
             aes(y = ID, x = cello, shape =ifelse(p.adj < 0.05, "triangle", "circle")),
             size = 2.5, position = position_nudge(y = -0.1))+
  scale_shape_manual(values = shape_values)

## gluco ##
AGGse_gluco_d<-summarySE(df_3d, measurevar="gluco", groupvars=c("ID"))
AGGse_gluco_d$D<- c('drought')
AGGse_gluco_d$ID<- as.character(AGGse_gluco_d$ID)
AGGse_gluco_ct<-summarySE(df_3ct, measurevar="gluco", groupvars=c("ID"))
AGGse_gluco_ct$D<- c('no-drought')
AGGse_gluco_ct$ID<- as.character(AGGse_gluco_ct$ID)
AGGse_gluco<- rbind(AGGse_gluco_ct,AGGse_gluco_d)
AGGse_gluco$D<- factor(AGGse_gluco$D,levels=c('drought','no-drought'))
AGGse_gluco_ct$ID<- factor(AGGse_gluco_ct$ID,levels=c('CT','3','3_in'))
AGGse_gluco_d$ID<- factor(AGGse_gluco_d$ID,levels=c('CT','3','3_in'))
df_3$ID<- factor(df_3$ID,levels=c('CT','3','3_in'))

calculate_gluco<- subset(df_3d,df_3d$ID=='CT')
mean_gluco<- mean(calculate_gluco$gluco)
Mycolor=c("#518463","#4590BF")
Mycolor=c("#518463","#96C2D4")
df_3$D<- factor(df_3$D,levels=c('0','1'))


filtered_gluco <- gluco %>% 
  filter(group == "no_drought") %>% 
  select(ID,p.adj)

# Add filtered_padj as a new column to AGGse_gluco_ct
AGGse_gluco_ct <- left_join(AGGse_gluco_ct, filtered_padj,by='ID')

filtered_padj_d <- gluco%>% 
  filter(group == "drought") %>% 
  select(ID,p.adj)

# Add filtered_padj as a new column to AGGse_gluco_ct
AGGse_gluco_d <- left_join(AGGse_gluco_d, filtered_padj_d,by='ID')


p_gluco_0<- ggplot(df_3)+
  coord_flip()+
  theme_bw()+
  theme(legend.position = 'none', axis.title.x=element_blank())+
  geom_density_ridges(aes(x=gluco,y=ID,fill=D,group=interaction(ID,D)),jittered_points = TRUE, 
                      alpha = .5,
                      position = position_points_jitter(height = .1, yoffset = -.15),
                      point_size = 2, point_alpha = .2, 
                      scale = .55)+
  scale_fill_manual(values  = Mycolor)
p_gluco<- p_gluco_0+geom_errorbar(data = AGGse_gluco_ct, aes(y = ID, xmin = gluco - ci, xmax = gluco + ci), 
                                  position = position_nudge(y = -0.2), color = '#3D9F3C', 
                                  width = 0.05, size =1) +
  geom_errorbar(data = AGGse_gluco_d,
                aes(y = ID, xmin = gluco - ci, xmax = gluco + ci), 
                position = position_nudge(y = -0.1), color = '#3492B2', 
                width = 0.05, size = 1) +
  geom_point(data = AGGse_gluco_ct,
             aes(y = ID, x = gluco, shape =ifelse(p.adj < 0.05, "triangle", "circle")),
             size = 2.5, position = position_nudge(y = -0.2))+
  geom_point(data = AGGse_gluco_d,
             aes(y = ID, x = gluco, shape =ifelse(p.adj < 0.05, "triangle", "circle")),
             size = 2.5, position = position_nudge(y = -0.1))+
  scale_shape_manual(values = shape_values)

library(ggpubr)
ggarrange(p_ph,p_WSA,p_aboveground,p_belowground,p_respiration,p_cello,p_gluco,p_phos,p_ace,ncol=3,nrow=3,common.legend = T, align = "hv",legend = "bottom")








#### first look at drought group ####
df<-df%>%
  filter(D==0)
df<-df%>%
 filter(D==1)

df$remark=factor(df$remark, levels = unique(df$remark))
responses = c('respiration','aboveground','belowground','ace','phos','cello','gluco','WSA','PH')
stressors = c('I1','I2','I3','C1','C2','C3','M1','M2','M3')
levels = c("1", '3', "6", "9")
treatment = as.vector(unique(df$remark))

n_iter   = 100      # permutaion size

##estimate mean and its 2.5%- 97.5% confidence interval, single stressor
BootStrap_mean = function(response, data=df, target = treatment, n_perm = n_iter){
  summary = list()
  for(treatment in target){
    bs = numeric(0)
    if (data$D == "1") {
      population_D <- data[data$remark %in% stressors, response]
    } else {
      population_CT <- data[data$remark == treatment, response]
    }
    
    size_D <- length(population_D) - sum(is.na(population_D))
    size_CT <- length(population_CT) - sum(is.na(population_CT))
    for(id in c(1:n_perm)){
      k = mean(sample(population, size, replace = T), na.rm = TRUE) #replace true means to put sampled data back
      bs = append(bs, k)  #append () is the function which will add elements to a vector.
    }
    summary[[treatment]] = c(quantile(bs, .025,na.rm = TRUE), mean(bs,na.rm = TRUE), quantile(bs, .975,na.rm = TRUE))
    names(summary[[treatment]]) = c("2.5%", "mean", "97.5%")
  }
  summary = t(data.frame(summary))
  summary = data.frame("target" = target, summary); row.names(summary) = c()
  return(summary)
}
#change 2
BootStrap_ES_rep = function(response, data=df, target = treatment, n_perm = n_iter){
  resampled = list()
  
  population_CT = data[data$remark=="CT", response]
  
  for(treatment in target){
    bs = numeric(0)
    if(treatment=="1") population_TR = data[data$remark%in%stressors, response]
    if(treatment!="1") population_TR = data[data$remark==treatment, response]
    size_CT = length(population_CT)-sum(is.na(population_CT))
    size_TR = length(population_TR)-sum(is.na(population_TR))
    
    for(id in c(1:n_perm)){
      k_CT = mean(sample(population_CT, size_CT, replace = T), na.rm = TRUE)
      k_TR = mean(sample(population_TR, size_TR, replace = T), na.rm = TRUE)
      bs = append(bs, k_TR - k_CT)
    }
    resampled[[treatment]] = bs
  }
  resampled[["CT"]] = rep(0, n_perm)
  return(resampled)
}
BootStrap_ES_summary = function(data){
  summary = list()
  p = 0
  summary[["CT"]] = c(0,0,0,1)
  target = names(data)
  
  for(treatment in target[-1]){
    bs = data[[treatment]]
    p = length(which(bs>0))/length(bs)
    p = min(p, 1-p)
    summary[[treatment]] = c(quantile(bs, .025,na.rm = TRUE), mean(bs,na.rm = TRUE), quantile(bs, .975,na.rm = TRUE), p)
  }
  summary = t(data.frame(summary))
  colnames(summary) = c("2.5%", "mean", "97.5%", "p_value")
  summary = data.frame(target, summary); row.names(summary) = c()
  
  return(summary)
}


response_mean_all = list()
response_ES_all   = list()
joint_ES_null_all = list()
ES_for_each_all   = list()
g_rawdata_all     = list()
g_ms_all          = list()
for (i_response in responses) {
  #bootstrap mean
  response_mean = BootStrap_mean(i_response)
  response_mean_all[[i_response]] = response_mean
  #effect size
  response_ES_bs  = BootStrap_ES_rep(i_response)
  response_ES     = BootStrap_ES_summary(response_ES_bs)
  response_ES_all[[i_response]]   = response_ES
  #Null model
#  Null_ES_bs0     = Null_distribution_rep(i_response)
 # Null_ES_bs      = Null_distribution_rep_transform(Null_ES_bs0)
 # joint_ES_null   = NHST_summary(Null_ES_bs, response_ES_bs)
  ##joint_ES_null_all[[i_response]] = bind_rows(joint_ES_null, .id = "column_label")
 # ES_plot         = NHST_summary_transform(joint_ES_null)
 # ES_for_each     = Expected_ES_for_each(Null_ES_bs0)
  #################################################################
  #------ Store the information for each response ----------------
  # Summary table combining the information from all response variables
  response_mean_all[[i_response]] = response_mean
  response_ES_all[[i_response]]   = response_ES
 # joint_ES_null_all[[i_response]] = bind_rows(joint_ES_null, .id = "column_label")
#  ES_for_each_all[[i_response]]   = ES_for_each
  
  
  g_rawdata_all[[i_response]] =  local({
    i_response = i_response
    response_mean = response_mean
    Mycolor=c("#518463","#185050","#185050","#185050","#185050","#185050","#185050","#185050","#185050","#185050","#307068","#307068","#307068","#307068","#307068")
    ggplot() +
      theme_bw()+
      theme(legend.position = 'none', axis.title.x=element_blank())+
      xlab(i_response) + 
      coord_flip() +
      stat_density_ridges(data=df, aes_string(x = i_response, y = "remark",color= "remark",fill= "remark"),
                          geom = "density_ridges_gradient",
                          rel_min_height = 0.01, 
                          jittered_points = TRUE, 
                          alpha = .5,
                          position = position_points_jitter(height = .2, yoffset = .15),
                          point_size = 1, point_alpha = .3, 
                          scale = .5) +
      scale_fill_manual(values  = Mycolor)+
      scale_alpha_manual(values = c(rep(0.5,17)))+
      geom_estci(data=response_mean, aes(x = mean, y = target, xmin=X2.5., xmax=X97.5., 
                                         xintercept=response_mean[1,"mean"], color =target), center.linecolour = "black",
                 size=0.6, ci.linesize = 0.5, position=position_nudge(y = -0.15))+
      scale_color_manual(values  = Mycolor)
    
    
  })
  # Ploting the number of stressors and ES relationship
#  g_ms_all[[i_response]] = local({
 #   i_response = i_response
 #   ct_value = response_mean[1,"mean"]
 #   ES_plot = ES_plot
 ##   for(i in 1:4) ES_plot[[i]][,2:4] = ES_plot[[i]][,2:4] + ct_value
  #  ggplot()+
  #    theme_bw()+
  #    theme(legend.position = 'none', axis.title.x=element_blank(), axis.title.y=element_blank())+
  #    theme(legend.position = "none")  +
  #    coord_flip() +
  #    scale_y_discrete(limits = factor(c("1","3",'6',"9"), levels=c("1","3",'6',"9"))) +
      ### Mean & CI ###
      # 3 assumptions
  #    geom_estci(data=ES_plot[["Additive"]], aes(x = Mean, y = Lv, xmin=Low, xmax=High, xintercept=ct_value),
               #  color="dark red", size=0.4, ci.linesize = 0.4, position=position_nudge(y = +0.1)) +
  #    geom_estci(data=ES_plot[["Multiplicative"]], aes(x = Mean, y = Lv, xmin=Low, xmax=High, xintercept=ct_value),
               #  color="dark green", size=0.4, ci.linesize = 0.4, position=position_nudge(y = +0.2)) +
  #    geom_estci(data=ES_plot[["Dominative"]], aes(x = Mean, y = Lv, xmin=Low, xmax=High, xintercept=ct_value),
              #   color="orange", size=0.4, ci.linesize = 0.4, position=position_nudge(y = +0.3)) +
      # Actual ES
  #    geom_estci(data=ES_plot[["Actual"]], aes(x = Mean, y = Lv, xmin=Low, xmax=High, xintercept=ct_value),
             #    size=0.6, ci.linesize = 0.6, position=position_nudge(y = 0))
 # })
}
#plotting###
####ct####
p_ace_raw<- g_rawdata_all[[4]]
p_phos_raw<- g_rawdata_all[[5]]
p_cello_raw<- g_rawdata_all[[6]]
p_gluco_raw<- g_rawdata_all[[7]]
p_respiration_raw<- g_rawdata_all[[1]]
p_abg_raw<- g_rawdata_all[[2]]
p_blg_raw<- g_rawdata_all[[3]]
p_wsa_raw<- g_rawdata_all[[8]]
p_ph_raw<- g_rawdata_all[[9]]
####drought####
dp_ace_raw<- g_rawdata_all[[4]]
dp_phos_raw<- g_rawdata_all[[5]]
dp_cello_raw<- g_rawdata_all[[6]]
dp_gluco_raw<- g_rawdata_all[[7]]
dp_respiration_raw<- g_rawdata_all[[1]]
dp_abg_raw<- g_rawdata_all[[2]]
dp_blg_raw<- g_rawdata_all[[3]]
dp_wsa_raw<- g_rawdata_all[[8]]
dp_ph_raw<- g_rawdata_all[[9]]


p_wsa_raw+p_lv_wsa+dp_lv_wsa+dp_wsa_raw+p_ph_raw+p_lv_ph+dp_lv_ph+dp_ph_raw+plot_layout(ncol=4,, widths=c(2,1,1,2))

p_abg_raw+p_lv_aboveground+dp_lv_aboveground+dp_abg_raw+p_blg_raw+p_lv_belowground+dp_lv_belowground+dp_blg_raw+plot_layout(ncol=4,, widths=c(2,1,1,2))

p_respiration_raw+p_lv_respiration+dp_lv_respiration+dp_respiration_raw+
  p_ace_raw+p_lv_ace+dp_lv_ace+dp_ace_raw+
  p_phos_raw+p_lv_phos+dp_lv_phos+dp_phos_raw+
  p_cello_raw+p_lv_cello+dp_lv_cello+dp_cello_raw+
  p_gluco_raw+p_lv_gluco+dp_lv_gluco+dp_gluco_raw+
  plot_layout(ncol=4,, widths=c(2,1,1,2))



#### D_ factor level effect_ random forest ####
library(ggplot2)
library(dplyr)
library(randomForest) #na.roughfix was used in this package
library(ggepi)
library(ggridges)
library(patchwork)
library(party)
library(caret)
library(dplyr)
library(tidyverse)
##### rf_ ct ####
setwd('C:/Huiying data/2nd_chapter/main_exp')
df<- read.csv('df_rf.csv')
df<- filter(df,df$ID!='3_in')

df<- filter(df,df$D=='0')

df$remark = factor(df$remark, levels = unique(df$remark))
responses = c('respiration','aboveground','belowground','ace','phos','cello','gluco','WSA','PH')
stressors = c('I1','I2','I3','C1','C2','C3','M1','M2','M3')
treatment = as.vector(unique(df$remark))
levels = c("1", "3", "6", "9")
n_iter = 100

##estimate mean and its 2.5%- 97.5% confidence interval, single stressor
BootStrap_mean = function(response, data=df, target = treatment, n_perm = n_iter){
  summary = list()
  for(treatment in target){
    bs = numeric(0)
    if(treatment=="1") population = data[data$remark%in%stressors, response]
    if(treatment!="1") population = data[data$remark==treatment, response]
    size = length(population)-sum(is.na(population)) #length indicates nr of columns
    for(id in c(1:n_perm)){
      k = mean(sample(population, size, replace = T), na.rm = TRUE) #replace true means to put sampled data back
      bs = append(bs, k)  #append () is the function which will add elements to a vector.
    }
    summary[[treatment]] = c(quantile(bs, .025,na.rm = TRUE), mean(bs,na.rm = TRUE), quantile(bs, .975,na.rm = TRUE))
    names(summary[[treatment]]) = c("2.5%", "mean", "97.5%")
  }
  summary = t(data.frame(summary))
  summary = data.frame("target" = target, summary); row.names(summary) = c()
  return(summary)
}
#change 2
BootStrap_ES_rep = function(response, data=df, target = treatment, n_perm = n_iter){
  resampled = list()
  population_CT = data[data$remark=="CT", response]
  for(treatment in target){
    bs = numeric(0)
    if(treatment=="1") population_TR = data[data$remark%in%stressors, response]
    if(treatment!="1") population_TR = data[data$remark==treatment, response]
    size_CT = length(population_CT)-sum(is.na(population_CT))
    size_TR = length(population_TR)-sum(is.na(population_TR))
    for(id in c(1:n_perm)){
      k_CT = mean(sample(population_CT, size_CT, replace = T), na.rm = TRUE)
      k_TR = mean(sample(population_TR, size_TR, replace = T), na.rm = TRUE)
      bs = append(bs, k_TR - k_CT)
    }
    resampled[[treatment]] = bs
  }
  resampled[["CT"]] = rep(0, n_perm)
  return(resampled)
}
BootStrap_ES_summary = function(data){
  summary = list()
  p = 0
  summary[["CT"]] = c(0,0,0,1)
  target = names(data)
  for(treatment in target[-1]){
    bs = data[[treatment]]
    p = length(which(bs>0))/length(bs)
    p = min(p, 1-p)
    summary[[treatment]] = c(quantile(bs, .025,na.rm = TRUE), mean(bs,na.rm = TRUE), quantile(bs, .975,na.rm = TRUE), p)
  }
  summary = t(data.frame(summary))
  colnames(summary) = c("2.5%", "mean", "97.5%", "p_value")
  summary = data.frame(target, summary); row.names(summary) = c()
  return(summary)
}

Null_distribution_rep = function(response, data=df, n_perm=n_iter){
  
  output = list()
  for(Lv in levels){
    
    resampled = list()
    
    # Checking which stressor combinations were jointly tested
    if(Lv=="1") combination = data[data$remark%in%stressors,c(1:9)]
    if(Lv!="1") combination = data[data["remark"]==Lv,c(1:9)]
    Level = sum(combination[1,])
    
    
    # Null distributions can be taken based on three different assumptions
    for(type in c("Additive", "Multiplicative", "Dominative")){
      
      population_CT = df[df$remark=="CT", response]
      size_CT = length(population_CT)-sum(is.na(population_CT)) ##subtract NA value
      
      # For each combination, bootstrap resampling is conducted
      for(j in c(1:nrow(combination))){
        bs = numeric(0)
        selected_stressors = stressors[which(combination[j,]==1)]
        sub_n_perm = ceiling(n_perm/nrow(combination))*5 #*5 increase the permutation number for sub-sampling
        
        # bootstrap resampling
        for(id in c(1:sub_n_perm)){
          each_effect = numeric(0)
          k_CT = mean(sample(population_CT, size_CT, replace = T),na.rm = TRUE)
          
          for(treatment in selected_stressors){
            population_TR = df[df$remark==treatment, response]
            size_TR = length(population_TR)
            k_TR = mean(sample(population_TR, size_TR, replace = T),na.rm = TRUE)
            
            # ES estimate depending on the type of null hypotheses
            if(type=="Additive")       each_effect = append(each_effect, (k_TR - k_CT))
            if(type=="Multiplicative") each_effect = append(each_effect, (k_TR - k_CT)/k_CT)
            if(type=="Dominative")      each_effect = append(each_effect, (k_TR - k_CT))
          }
          
          # Calculating an expected ES after collecting the ESs of all relevant single stressors
          if(type=="Additive")       joint_effect = sum(each_effect)
          if(type=="Multiplicative"){
            z = 1
            for(m in c(1:Level)) z = z * (1 + each_effect[m])
            joint_effect = (z - 1)*k_CT
          }
          if(type=="Dominative")      joint_effect = each_effect[which(max(abs(each_effect))==abs(each_effect))]
          
          bs = append(bs, joint_effect)
        }
        resampled[[type]][[j]] = bs
      }
      
    }
    output[[Lv]] = resampled
  }  
  return(output)
} 

Null_distribution_rep_transform = function(data){
  output = list()
  for(Lv in levels){
    for(type in c("Additive", "Multiplicative", "Dominative")){
      output[[Lv]][[type]] = sample(unlist(data[[Lv]][[type]]), n_iter, replace=F)
    }
  }
  return(output)
}

NHST_summary = function(null_data, Actual_data){
  output = list()
  for(Lv in levels){
    summary = list()
    summary[["Actual"]] = c(quantile(Actual_data[[Lv]], .025,na.rm = TRUE), mean(Actual_data[[Lv]],na.rm = TRUE), quantile(Actual_data[[Lv]], .975,na.rm = TRUE), 1)
    p = 0
    assumptions = c("Additive", "Multiplicative", "Dominative")
    
    for(i_assumption in assumptions){
      bs   = (Actual_data[[Lv]] - null_data[[Lv]][[i_assumption]])
      p = length(which(bs>0))/length(bs)
      p = min(p, 1-p)
      summary[[i_assumption]] = c(quantile(null_data[[Lv]][[i_assumption]], .025,na.rm = TRUE), mean(null_data[[Lv]][[i_assumption]],na.rm = TRUE), quantile(null_data[[Lv]][[i_assumption]], .975,na.rm = TRUE), p)
    }
    summary = t(data.frame(summary))
    colnames(summary) = c("2.5%", "mean", "97.5%", "p_value")
    summary = data.frame(ES = c("Actual", "Additive","Multiplicative","Dominative"), summary); row.names(summary) = c()
    
    output[[Lv]] = summary
  }
  
  return(output)
}

NHST_summary_transform = function(data){
  output = list()
  for(i in 1:4){
    summary = rbind(data[["1"]][i, 2:4], data[["3"]][i, 2:4], data[["6"]][i, 2:4],
                    data[["9"]][i, 2:4])
    summary = cbind(levels, summary)
    colnames(summary) = c("Lv", "Low", "Mean", "High")
    output[[c("Actual", "Additive", "Multiplicative", "Dominative")[i]]] = summary
  }
  return(output)
}

Expected_ES_for_each = function(data){
  output = numeric(0)
  for(type in c("Additive", "Multiplicative", "Dominative")){
    tmp = numeric(0)
    for(Lv in levels){
      n_len = length(data[[Lv]][[type]])
      for(i in 1:n_len){
        tmp = append(tmp, mean(data[[Lv]][[type]][[i]]))
      }
    }
    output = cbind(output,tmp)
  }
  colnames(output)= c("E1", "E2", "E3")
  return(output)
}



response_mean_all = list()
response_ES_all   = list()
joint_ES_null_all = list()
ES_for_each_all   = list()
g_rawdata_all     = list()
g_ms_all          = list()
for (i_response in responses) {
  #bootstrap mean
  response_mean = BootStrap_mean(i_response)
  response_mean_all[[i_response]] = response_mean
  #effect size
  response_ES_bs  = BootStrap_ES_rep(i_response)
  response_ES     = BootStrap_ES_summary(response_ES_bs)
  response_ES_all[[i_response]]   = response_ES
  #Null model
  Null_ES_bs0     = Null_distribution_rep(i_response)
  Null_ES_bs      = Null_distribution_rep_transform(Null_ES_bs0)
  
  joint_ES_null   = NHST_summary(Null_ES_bs, response_ES_bs)
  joint_ES_null_all[[i_response]] = bind_rows(joint_ES_null, .id = "column_label")
  
  ES_plot         = NHST_summary_transform(joint_ES_null)
  ES_for_each     = Expected_ES_for_each(Null_ES_bs0)
  ES_for_each_all[[i_response]]   = ES_for_each
  
  g_rawdata_all[[i_response]] =  local({
    i_response = i_response
    response_mean = response_mean
    
    ggplot() +
      theme_bw()+
      theme(legend.position = 'none', axis.title.x=element_blank())+
      xlab(i_response) + 
      coord_flip() +
      stat_density_ridges(data=df, aes_string(x = i_response, y = "remark"),
                          geom = "density_ridges_gradient",
                          rel_min_height = 0.01, 
                          jittered_points = TRUE, color="#00000000",
                          alpha = .5,
                          position = position_points_jitter(height = .2, yoffset = .15),
                          point_size = 1, point_alpha = .3, 
                          scale = .5) +
      geom_estci(data=response_mean, aes(x = mean, y = target, xmin=X2.5., xmax=X97.5., 
                                         xintercept=response_mean[1,"mean"]), center.linecolour = "black",
                 size=0.6, ci.linesize = 0.5, position=position_nudge(y = -0.15)) 
    
    
  })
  
  # Ploting the number of stressors and ES relationship
  
  g_ms_all[[i_response]] = local({
    i_response = i_response
    ct_value = response_mean[1,"mean"]
    ES_plot = ES_plot
    for(i in 1:4) ES_plot[[i]][,2:4] = ES_plot[[i]][,2:4] + ct_value
    
    ggplot()+
      theme_bw()+
      theme(legend.position = 'none', axis.title.x=element_blank(), axis.title.y=element_blank())+
      theme(legend.position = "none")  +
      coord_flip() +
      scale_y_discrete(limits = factor(c("1","3","6","9"), levels=c("1","3","6","9"))) +
      
      ### Mean & CI ###
      # 3 assumptions
      geom_estci(data=ES_plot[["Additive"]], aes(x = Mean, y = Lv, xmin=Low, xmax=High, xintercept=ct_value), 
                 color="#008900AA", size=0.4, ci.linesize = 0.4, position=position_nudge(y = +0.1)) +
      geom_estci(data=ES_plot[["Multiplicative"]], aes(x = Mean, y = Lv, xmin=Low, xmax=High, xintercept=ct_value), 
                 color="#ff0083AA", size=0.4, ci.linesize = 0.4, position=position_nudge(y = +0.2)) +
      geom_estci(data=ES_plot[["Dominative"]], aes(x = Mean, y = Lv, xmin=Low, xmax=High, xintercept=ct_value), 
                 color="#0000A0AA", size=0.4, ci.linesize = 0.4, position=position_nudge(y = +0.3)) +
      
      # Actual ES
      geom_estci(data=ES_plot[["Actual"]], aes(x = Mean, y = Lv, xmin=Low, xmax=High, xintercept=ct_value), 
                 size=0.6, ci.linesize = 0.6, position=position_nudge(y = 0))
  })
  
}

### from Mohan Bi ###
postResample <- function(pred, obs)
{
  isNA <- is.na(pred)
  pred <- pred[!isNA]
  obs <- obs[!isNA]
  if (!is.factor(obs) && is.numeric(obs))
  {
    if(length(obs) + length(pred) == 0)
    {
      out <- rep(NA, 3)
    } else {
      if(length(unique(pred)) < 2 || length(unique(obs)) < 2)
      {
        resamplCor <- NA
      } else {
        resamplCor <- try(cor(pred, obs, use = "pairwise.complete.obs"), silent = TRUE)
        if (inherits(resamplCor, "try-error")) resamplCor <- NA
      }
      mse <- mean((pred - obs)^2)
      mae <- mean(abs(pred - obs))
      out <- c(sqrt(mse), resamplCor^2, mae)
    }
    names(out) <- c("RMSE", "Rsquared", "MAE")
  } else {
    if(length(obs) + length(pred) == 0)
    {
      out <- rep(NA, 2)
    } else {
      pred <- factor(pred, levels = levels(obs))
      requireNamespaceQuietStop("e1071")
      out <- unlist(e1071::classAgreement(table(obs, pred)))[c("diag", "kappa")]
    }
    names(out) <- c("Accuracy", "Kappa")
  }
  if(any(is.nan(out))) out[is.nan(out)] <- NA
  out
}


df.rf = df[df[, "remark"] %in% levels,]

lv_list  = unique(df.rf[,"Lv"])
id_lv1   = which(df.rf[,"Lv"]==1)
id_lvh   = which(df.rf[,"Lv"]>1)

n_data   = nrow(df.rf)
n_lv1    = sum(df.rf[,"Lv"]==1)
n_lvh    = sum(df.rf[,"Lv"]!=1)
n_eachlv = 20
n_tree   = 100
n_iter2  = 100


rf.r2 = data.frame(matrix(NA, ncol=3, nrow=3*n_iter2*length(responses)))
rf.r2[,1] = rep(responses,each=3*n_iter2)
rf.r2[,2] = rep(c("Lv", "Lv+ID", "All"), n_iter2*length(responses))
rf.r2[,2] = factor(rf.r2[,2], levels=c("Lv", "Lv+ID", "All"))
colnames(rf.r2) = c("Response", "Model", "R2")

rf.pred = data.frame(matrix(NA, ncol=5, nrow=n_iter2*length(responses)))
rf.pred[,1] = rep(responses,each=n_iter2)
colnames(rf.pred) = c("Response", lv_list)

rf.vimp = data.frame(matrix(NA, ncol=14, nrow=n_iter2*length(responses)))
rf.vimp[,1] = rep(responses,each=n_iter2) 
colnames(rf.vimp) = c("Response", "Lv", stressors, "E1", "E2", "E3")

rf.prediction.all = list()

j = 0
for(i_response in responses){
  
  df.rf.tmp = cbind(df.rf, ES_for_each_all[[i_response]])
  eval(parse(text=(paste("fml      = formula(",i_response,"~", paste(c("Lv", stressors, colnames(ES_for_each_all[[i_response]])), collapse=" + "),")", sep=""))))
  eval(parse(text=(paste("fml.Lv   = formula(",i_response,"~Lv)", sep=""))))
  eval(parse(text=(paste("fml.LvID = formula(",i_response,"~", paste(c("Lv", stressors), collapse=" + "),")", sep=""))))
  
  for(i in 1:n_iter2){
    j = j + 1
    # bootstrap resampling
    set.seed(j)
    # take 10 sample from Lv1, and take 40 sample from the other levels for balanced resampling
    rid = c(sample(id_lv1, n_eachlv, replace=T), sample(id_lvh, n_lvh, replace=T))
    rdf = df.rf.tmp[rid,]
    rf_model      = tryCatch({
      cforest(fml,      data = rdf, control=cforest_control(ntree=n_tree, minsplit = 5, minbucket=2))},
      error = function(e) {cforest(fml.Lv,   data = rdf, control=cforest_control(ntree=n_tree, minsplit = 5, minbucket=2))})
    rf_model.Lv   = cforest(fml.Lv,   data = rdf, control=cforest_control(ntree=n_tree, minsplit = 5, minbucket=2))
    rf_model.LvID = cforest(fml.LvID, data = rdf, control=cforest_control(ntree=n_tree, minsplit = 5, minbucket=2))
    
    # shuffling corresponding variables for evaluating R2 reduction
    # rdf.test.LvID = rdf
    # rdf.test.LvID[,colnames(ES_for_each_all[[i_response]])] = apply(rdf.test.LvID[,colnames(ES_for_each_all[[i_response]])],2,sample)  
    
    # evaluating fitting performance
    rf_prdct.Lv   = tryCatch({predict(rf_model.Lv, OOB=T)}, error=function(e){rep(0,length(rid))})
    rf_prdct.LvID = tryCatch({predict(rf_model.LvID, OOB=T)}, error=function(e){rep(0,length(rid))})
    rf_prdct.All  = tryCatch({predict(rf_model, OOB=T)}, error=function(e){rep(0,length(rid))})
    
    rf.r2[3*j-2,3]    = postResample(rf_prdct.Lv,rdf[,i_response])[2]
    rf.r2[3*j-1,3]    = postResample(rf_prdct.LvID,rdf[,i_response])[2]
    rf.r2[3*j-0,3]    = postResample(rf_prdct.All,rdf[,i_response])[2]
    
    # variable importance
    set.seed(j)
    tmp.vimp = numeric(0)
    for(itmp in 1:5) tmp.vimp = rbind(tmp.vimp,varimp(rf_model))
    tmp.vimp = apply(tmp.vimp,2,mean)
    tmp.vimp = tmp.vimp/sum(tmp.vimp)
    rf.vimp[j, 2:(length(tmp.vimp)+1)] = tmp.vimp*rf.r2[2*j-0,3]*100
    
    # fitting curve
    tmp.curve = c()
    for(i_lv in lv_list){
      tmp.curve = append(tmp.curve, mean(rf_prdct.All[rdf[,"Lv"]==i_lv]))
    }
    rf.pred[j,2:5]    =  tmp.curve
  }
  
  rf_model      = cforest(fml,      data = df.rf.tmp, control=cforest_control(ntree=n_tree, minsplit = 5, minbucket=2))
  rf_model.Lv   = cforest(fml.Lv,   data = df.rf.tmp, control=cforest_control(ntree=n_tree, minsplit = 5, minbucket=2))
  rf_model.LvID = cforest(fml.LvID, data = df.rf.tmp, control=cforest_control(ntree=n_tree, minsplit = 5, minbucket=2))
  
  rf.prediction.all[[i_response]] = data.frame(
    Model     = rep(c("Lv", "LvID", "All"), each = nrow(df.rf.tmp)),
    Predicted = c(predict(rf_model.Lv, OOB=F), predict(rf_model.LvID, OOB=F), predict(rf_model, OOB=F)),
    Observed  = rep(df.rf.tmp[,i_response], 3))
  
}
rf.r2.summary = data.frame(matrix(NA,ncol=5,nrow=3*length(responses)))
colnames(rf.r2.summary) = c("Response","Model", "CI.low", "Mean", "CI.high")
rf.r2.summary[,1] = rep(responses,each=3)
rf.r2.summary[,2] = rep(c("Lv", "Lv+ID", "All"),length(responses))
rf.r2.summary[,2] = factor(rf.r2.summary[,2],levels=c("Lv", "Lv+ID", "All"))


rf.pred.summary = data.frame(matrix(NA,ncol=5,nrow=4*length(responses)))
colnames(rf.pred.summary) = c("Response","Lv","CI.low", "Mean", "CI.high")
rf.pred.summary[,1] = rep(responses,each=length(lv_list))
rf.pred.summary[,2] = rep(lv_list, length(responses))

rf.vimp.summary = data.frame(matrix(NA,ncol=5,nrow=13*length(responses)))
colnames(rf.vimp.summary) = c("Response","Variable","CI.low", "Mean", "CI.high")
rf.vimp.summary[,1] = rep(responses,each=13)
rf.vimp.summary[,2] = rep(colnames(rf.vimp)[2:14], length(responses))
rf.vimp.summary[,2] = factor(rf.vimp.summary[,2], levels = unique(rf.vimp.summary[,2]))

j  = 0
for(i_response in responses){
  jj = 0
  jjj = 0
  rf.r2.summary[3*j+1,3:5] = quantile(rf.r2[which(rf.r2[,1]==i_response&rf.r2[,2]=="Lv"),   3],c(.025,.50,.975), na.rm=T)
  rf.r2.summary[3*j+2,3:5] = quantile(rf.r2[which(rf.r2[,1]==i_response&rf.r2[,2]=="Lv+ID"),3],c(.025,.50,.975), na.rm=T)
  rf.r2.summary[3*j+3,3:5] = quantile(rf.r2[which(rf.r2[,1]==i_response&rf.r2[,2]=="All"),  3],c(.025,.50,.975), na.rm=T)
  
  for(i_lv in lv_list){
    jj = jj + 1
    rf.pred.summary[4*j+jj,3:5]     = quantile(rf.pred[which(rf.pred[,1]==i_response),as.character(i_lv)],c(.025,.50,.975), na.rm=T)
  }
  
  for(i_var in colnames(rf.vimp)[2:14]){
    jjj = jjj + 1
    rf.vimp.summary[13*j+jjj,3:5]     = quantile(rf.vimp[which(rf.vimp[,1]==i_response),as.character(i_var)],c(.025,.50,.975), na.rm=T)
  }
  
  j = j + 1
  
}


# write.csv(rf.r2.summary, "randomforest_r2_summary.csv")
rf.pred.summary<- na.omit(rf.pred.summary)

g_rf_all     = list()
g_vimp_all   = list()
g_r2_all     = list()
g_correl_all = list()
for(i in 1:(length(responses))){
  g_rf_all[[i]] =  local({
    i = i
    ggrf_select = rf.pred.summary[rf.pred.summary[,1]==responses[i],]
    ggdf_select = df.rf[,c("Lv",responses[i])]
    
    ggplot(data=ggrf_select)+
      geom_point(data=ggdf_select, aes_string(x="Lv",y=responses[i]))+
      geom_line(aes(x=Lv, y=Mean)) +
      geom_ribbon(aes(x=Lv,  ymax=CI.high, ymin=CI.low), colour = NA, fill="#999999",alpha=.5)+
      theme_bw()+
      theme(legend.position = 'none', axis.title.x=element_blank(),axis.title.y=element_blank())
  })  
  g_vimp_all[[i]] = local({
    i = i
    ggplot(data=rf.vimp.summary[rf.vimp.summary[,1]==responses[i],],aes(x=Variable, y=Mean))+
      geom_bar(stat="identity", fill="#999999", alpha=0.5) +
      xlab("Variability explained [%]")+
      geom_errorbar(aes(ymax=CI.high, ymin=CI.low), width=.2, position=position_dodge(width=0.0)) +
      theme_bw()+
      theme(legend.position = 'none', axis.title.x=element_blank(),axis.title.y=element_blank())
  })
  g_r2_all[[i]] = local({
    i = i
    rf.r2 = rf.r2
    rf.r2.summary = rf.r2.summary 
    ggplot(data=rf.r2[rf.r2[,1]==responses[i],],aes(x=Model,y=R2, fill=Model))+
      geom_violin( color="#00000000",alpha=.5,position=position_dodge(width=0.3),trim=F)+
      geom_pointrange(data=rf.r2.summary[rf.r2.summary[,1]==responses[i],], aes(y=Mean, ymax=CI.high, ymin=CI.low,color=Model), position=position_dodge(width=0.2)) +
      scale_fill_manual(values  = c('#999999',"#999999",  "#999999",  "#999999"))+ 
      scale_color_manual(values = c("#008900AA","#ff0083AA", "#0000A0AA", "#505050"))+ 
      theme_bw() + ylim(c(0,1.0))+
      theme(axis.title.y=element_text(size=8))+
      ylab('Variability explained (R2%)')+
      theme(legend.position = 'none', axis.title.x=element_blank())
  })
  
  g_correl_all[[i]] = local({
    i = i
    rf.prediction.all = rf.prediction.all
    ggplot(data = rf.prediction.all[[i]], aes(y=Predicted, x=Observed, group=Model, color=Model))+
      theme_bw()+
      geom_abline(slope=1, intercept=0) + geom_point() + geom_smooth(method="lm", fullrange=F,size= 1.5) +
      xlim(range(rf.prediction.all[[i]][,2:3])) + ylim(range(rf.prediction.all[[i]][,2:3])) +
      theme(legend.position = 'none', axis.title.x=element_blank(),axis.title.y=element_blank()) +
      scale_color_manual(values = c("#599a9780", "#1e3a6180", "#c9b75f80"))
  }) 
  
}  


g_rawdata_all[[1]]+g_ms_all[[1]]+g_vimp_all[[1]]+g_r2_all[[1]]+
  g_rawdata_all[[2]]+g_ms_all[[2]]+g_vimp_all[[2]]+g_r2_all[[2]]+
  g_rawdata_all[[3]]+g_ms_all[[3]]+g_vimp_all[[3]]+g_r2_all[[3]]+
  g_rawdata_all[[4]]+g_ms_all[[4]]+g_vimp_all[[4]]+g_r2_all[[4]]+
  plot_layout(ncol=4, widths=c(3,2,2,1))

g_rawdata_all[[1]]+g_ms_all[[1]]+g_rf_all[[1]]+g_r2_all[[1]]+
  g_rawdata_all[[2]]+g_ms_all[[2]]+g_rf_all[[2]]+g_r2_all[[2]]+
  g_rawdata_all[[8]]+g_ms_all[[8]]+g_rf_all[[8]]+g_r2_all[[8]]+
  g_rawdata_all[[9]]+g_ms_all[[9]]+g_rf_all[[9]]+g_r2_all[[9]]+
  plot_layout(ncol=4, widths=c(3,2,2,1))
g_rawdata_all[[4]]+g_ms_all[[4]]+g_rf_all[[4]]+g_r2_all[[4]]+
  g_rawdata_all[[5]]+g_ms_all[[5]]+g_rf_all[[5]]+g_r2_all[[5]]+
  g_rawdata_all[[6]]+g_ms_all[[6]]+g_rf_all[[6]]+g_r2_all[[6]]+
  g_rawdata_all[[7]]+g_ms_all[[7]]+g_rf_all[[7]]+g_r2_all[[7]]+
  plot_layout(ncol=4, widths=c(3,2,2,1))

#### rf_ drought #### 
setwd('C:/Huiying data/2nd_chapter/main_exp')
df<- read.csv('df_rf.csv')
df<- filter(df,df$ID!='3_in')

df<- filter(df,df$D=='1')

df$remark = factor(df$remark, levels = unique(df$remark))
responses = c('respiration','aboveground','belowground','ace','phos','cello','gluco','WSA','PH')
stressors = c('I1','I2','I3','C1','C2','C3','M1','M2','M3')
treatment = as.vector(unique(df$remark))
levels = c("1", "3", "6", "9")
n_iter = 100

##estimate mean and its 2.5%- 97.5% confidence interval, single stressor
BootStrap_mean = function(response, data=df, target = treatment, n_perm = n_iter){
  summary = list()
  for(treatment in target){
    bs = numeric(0)
    if(treatment=="1") population = data[data$remark%in%stressors, response]
    if(treatment!="1") population = data[data$remark==treatment, response]
    size = length(population)-sum(is.na(population)) #length indicates nr of columns
    for(id in c(1:n_perm)){
      k = mean(sample(population, size, replace = T), na.rm = TRUE) #replace true means to put sampled data back
      bs = append(bs, k)  #append () is the function which will add elements to a vector.
    }
    summary[[treatment]] = c(quantile(bs, .025,na.rm = TRUE), mean(bs,na.rm = TRUE), quantile(bs, .975,na.rm = TRUE))
    names(summary[[treatment]]) = c("2.5%", "mean", "97.5%")
  }
  summary = t(data.frame(summary))
  summary = data.frame("target" = target, summary); row.names(summary) = c()
  return(summary)
}
#change 2
BootStrap_ES_rep = function(response, data=df, target = treatment, n_perm = n_iter){
  resampled = list()
  population_CT = data[data$remark=="CT", response]
  for(treatment in target){
    bs = numeric(0)
    if(treatment=="1") population_TR = data[data$remark%in%stressors, response]
    if(treatment!="1") population_TR = data[data$remark==treatment, response]
    size_CT = length(population_CT)-sum(is.na(population_CT))
    size_TR = length(population_TR)-sum(is.na(population_TR))
    for(id in c(1:n_perm)){
      k_CT = mean(sample(population_CT, size_CT, replace = T), na.rm = TRUE)
      k_TR = mean(sample(population_TR, size_TR, replace = T), na.rm = TRUE)
      bs = append(bs, k_TR - k_CT)
    }
    resampled[[treatment]] = bs
  }
  resampled[["CT"]] = rep(0, n_perm)
  return(resampled)
}
BootStrap_ES_summary = function(data){
  summary = list()
  p = 0
  summary[["CT"]] = c(0,0,0,1)
  target = names(data)
  for(treatment in target[-1]){
    bs = data[[treatment]]
    p = length(which(bs>0))/length(bs)
    p = min(p, 1-p)
    summary[[treatment]] = c(quantile(bs, .025,na.rm = TRUE), mean(bs,na.rm = TRUE), quantile(bs, .975,na.rm = TRUE), p)
  }
  summary = t(data.frame(summary))
  colnames(summary) = c("2.5%", "mean", "97.5%", "p_value")
  summary = data.frame(target, summary); row.names(summary) = c()
  return(summary)
}

Null_distribution_rep = function(response, data=df, n_perm=n_iter){
  
  output = list()
  for(Lv in levels){
    
    resampled = list()
    
    # Checking which stressor combinations were jointly tested
    if(Lv=="1") combination = data[data$remark%in%stressors,c(1:9)]
    if(Lv!="1") combination = data[data["remark"]==Lv,c(1:9)]
    Level = sum(combination[1,])
    
    
    # Null distributions can be taken based on three different assumptions
    for(type in c("Additive", "Multiplicative", "Dominative")){
      
      population_CT = df[df$remark=="CT", response]
      size_CT = length(population_CT)-sum(is.na(population_CT)) ##subtract NA value
      
      # For each combination, bootstrap resampling is conducted
      for(j in c(1:nrow(combination))){
        bs = numeric(0)
        selected_stressors = stressors[which(combination[j,]==1)]
        sub_n_perm = ceiling(n_perm/nrow(combination))*5 #*5 increase the permutation number for sub-sampling
        
        # bootstrap resampling
        for(id in c(1:sub_n_perm)){
          each_effect = numeric(0)
          k_CT = mean(sample(population_CT, size_CT, replace = T),na.rm = TRUE)
          
          for(treatment in selected_stressors){
            population_TR = df[df$remark==treatment, response]
            size_TR = length(population_TR)
            k_TR = mean(sample(population_TR, size_TR, replace = T),na.rm = TRUE)
            
            # ES estimate depending on the type of null hypotheses
            if(type=="Additive")       each_effect = append(each_effect, (k_TR - k_CT))
            if(type=="Multiplicative") each_effect = append(each_effect, (k_TR - k_CT)/k_CT)
            if(type=="Dominative")      each_effect = append(each_effect, (k_TR - k_CT))
          }
          
          # Calculating an expected ES after collecting the ESs of all relevant single stressors
          if(type=="Additive")       joint_effect = sum(each_effect)
          if(type=="Multiplicative"){
            z = 1
            for(m in c(1:Level)) z = z * (1 + each_effect[m])
            joint_effect = (z - 1)*k_CT
          }
          if(type=="Dominative")      joint_effect = each_effect[which(max(abs(each_effect))==abs(each_effect))]
          
          bs = append(bs, joint_effect)
        }
        resampled[[type]][[j]] = bs
      }
      
    }
    output[[Lv]] = resampled
  }  
  return(output)
} 

Null_distribution_rep_transform = function(data){
  output = list()
  for(Lv in levels){
    for(type in c("Additive", "Multiplicative", "Dominative")){
      output[[Lv]][[type]] = sample(unlist(data[[Lv]][[type]]), n_iter, replace=F)
    }
  }
  return(output)
}

NHST_summary = function(null_data, Actual_data){
  output = list()
  for(Lv in levels){
    summary = list()
    summary[["Actual"]] = c(quantile(Actual_data[[Lv]], .025,na.rm = TRUE), mean(Actual_data[[Lv]],na.rm = TRUE), quantile(Actual_data[[Lv]], .975,na.rm = TRUE), 1)
    p = 0
    assumptions = c("Additive", "Multiplicative", "Dominative")
    
    for(i_assumption in assumptions){
      bs   = (Actual_data[[Lv]] - null_data[[Lv]][[i_assumption]])
      p = length(which(bs>0))/length(bs)
      p = min(p, 1-p)
      summary[[i_assumption]] = c(quantile(null_data[[Lv]][[i_assumption]], .025,na.rm = TRUE), mean(null_data[[Lv]][[i_assumption]],na.rm = TRUE), quantile(null_data[[Lv]][[i_assumption]], .975,na.rm = TRUE), p)
    }
    summary = t(data.frame(summary))
    colnames(summary) = c("2.5%", "mean", "97.5%", "p_value")
    summary = data.frame(ES = c("Actual", "Additive","Multiplicative","Dominative"), summary); row.names(summary) = c()
    
    output[[Lv]] = summary
  }
  
  return(output)
}

NHST_summary_transform = function(data){
  output = list()
  for(i in 1:4){
    summary = rbind(data[["1"]][i, 2:4], data[["3"]][i, 2:4], data[["6"]][i, 2:4],
                    data[["9"]][i, 2:4])
    summary = cbind(levels, summary)
    colnames(summary) = c("Lv", "Low", "Mean", "High")
    output[[c("Actual", "Additive", "Multiplicative", "Dominative")[i]]] = summary
  }
  return(output)
}

Expected_ES_for_each = function(data){
  output = numeric(0)
  for(type in c("Additive", "Multiplicative", "Dominative")){
    tmp = numeric(0)
    for(Lv in levels){
      n_len = length(data[[Lv]][[type]])
      for(i in 1:n_len){
        tmp = append(tmp, mean(data[[Lv]][[type]][[i]]))
      }
    }
    output = cbind(output,tmp)
  }
  colnames(output)= c("E1", "E2", "E3")
  return(output)
}



response_mean_all = list()
response_ES_all   = list()
joint_ES_null_all = list()
ES_for_each_all   = list()
g_rawdata_all     = list()
g_ms_all          = list()
for (i_response in responses) {
  #bootstrap mean
  response_mean = BootStrap_mean(i_response)
  response_mean_all[[i_response]] = response_mean
  #effect size
  response_ES_bs  = BootStrap_ES_rep(i_response)
  response_ES     = BootStrap_ES_summary(response_ES_bs)
  response_ES_all[[i_response]]   = response_ES
  #Null model
  Null_ES_bs0     = Null_distribution_rep(i_response)
  Null_ES_bs      = Null_distribution_rep_transform(Null_ES_bs0)
  
  joint_ES_null   = NHST_summary(Null_ES_bs, response_ES_bs)
  joint_ES_null_all[[i_response]] = bind_rows(joint_ES_null, .id = "column_label")
  
  ES_plot         = NHST_summary_transform(joint_ES_null)
  ES_for_each     = Expected_ES_for_each(Null_ES_bs0)
  ES_for_each_all[[i_response]]   = ES_for_each
  
  g_rawdata_all[[i_response]] =  local({
    i_response = i_response
    response_mean = response_mean
    
    ggplot() +
      theme_bw()+
      theme(legend.position = 'none', axis.title.x=element_blank())+
      xlab(i_response) + 
      coord_flip() +
      stat_density_ridges(data=df, aes_string(x = i_response, y = "remark"),
                          geom = "density_ridges_gradient",
                          rel_min_height = 0.01, 
                          jittered_points = TRUE, color="#00000000",
                          alpha = .5,
                          position = position_points_jitter(height = .2, yoffset = .15),
                          point_size = 1, point_alpha = .3, 
                          scale = .5) +
      geom_estci(data=response_mean, aes(x = mean, y = target, xmin=X2.5., xmax=X97.5., 
                                         xintercept=response_mean[1,"mean"]), center.linecolour = "black",
                 size=0.6, ci.linesize = 0.5, position=position_nudge(y = -0.15)) 
    
    
  })
  
  # Ploting the number of stressors and ES relationship
  
  g_ms_all[[i_response]] = local({
    i_response = i_response
    ct_value = response_mean[1,"mean"]
    ES_plot = ES_plot
    for(i in 1:4) ES_plot[[i]][,2:4] = ES_plot[[i]][,2:4] + ct_value
    
    ggplot()+
      theme_bw()+
      theme(legend.position = 'none', axis.title.x=element_blank(), axis.title.y=element_blank())+
      theme(legend.position = "none")  +
      coord_flip() +
      scale_y_discrete(limits = factor(c("1","3","6","9"), levels=c("1","3","6","9"))) +
      
      ### Mean & CI ###
      # 3 assumptions
      geom_estci(data=ES_plot[["Additive"]], aes(x = Mean, y = Lv, xmin=Low, xmax=High, xintercept=ct_value), 
                 color="#008900AA", size=0.4, ci.linesize = 0.4, position=position_nudge(y = +0.1)) +
      geom_estci(data=ES_plot[["Multiplicative"]], aes(x = Mean, y = Lv, xmin=Low, xmax=High, xintercept=ct_value), 
                 color="#ff0083AA", size=0.4, ci.linesize = 0.4, position=position_nudge(y = +0.2)) +
      geom_estci(data=ES_plot[["Dominative"]], aes(x = Mean, y = Lv, xmin=Low, xmax=High, xintercept=ct_value), 
                 color="#0000A0AA", size=0.4, ci.linesize = 0.4, position=position_nudge(y = +0.3)) +
      
      # Actual ES
      geom_estci(data=ES_plot[["Actual"]], aes(x = Mean, y = Lv, xmin=Low, xmax=High, xintercept=ct_value), 
                 size=0.6, ci.linesize = 0.6, position=position_nudge(y = 0))
  })
  
}

### from Mohan Bi ###
postResample <- function(pred, obs)
{
  isNA <- is.na(pred)
  pred <- pred[!isNA]
  obs <- obs[!isNA]
  if (!is.factor(obs) && is.numeric(obs))
  {
    if(length(obs) + length(pred) == 0)
    {
      out <- rep(NA, 3)
    } else {
      if(length(unique(pred)) < 2 || length(unique(obs)) < 2)
      {
        resamplCor <- NA
      } else {
        resamplCor <- try(cor(pred, obs, use = "pairwise.complete.obs"), silent = TRUE)
        if (inherits(resamplCor, "try-error")) resamplCor <- NA
      }
      mse <- mean((pred - obs)^2)
      mae <- mean(abs(pred - obs))
      out <- c(sqrt(mse), resamplCor^2, mae)
    }
    names(out) <- c("RMSE", "Rsquared", "MAE")
  } else {
    if(length(obs) + length(pred) == 0)
    {
      out <- rep(NA, 2)
    } else {
      pred <- factor(pred, levels = levels(obs))
      requireNamespaceQuietStop("e1071")
      out <- unlist(e1071::classAgreement(table(obs, pred)))[c("diag", "kappa")]
    }
    names(out) <- c("Accuracy", "Kappa")
  }
  if(any(is.nan(out))) out[is.nan(out)] <- NA
  out
}


df.rf = df[df[, "remark"] %in% levels,]

lv_list  = unique(df.rf[,"Lv"])
id_lv1   = which(df.rf[,"Lv"]==1)
id_lvh   = which(df.rf[,"Lv"]>1)

n_data   = nrow(df.rf)
n_lv1    = sum(df.rf[,"Lv"]==10)
n_lvh    = sum(df.rf[,"Lv"]!=1)
n_eachlv = 20
n_tree   = 100
n_iter2  = 100


rf.r2 = data.frame(matrix(NA, ncol=3, nrow=3*n_iter2*length(responses)))
rf.r2[,1] = rep(responses,each=3*n_iter2)
rf.r2[,2] = rep(c("Lv", "Lv+ID", "All"), n_iter2*length(responses))
rf.r2[,2] = factor(rf.r2[,2], levels=c("Lv", "Lv+ID", "All"))
colnames(rf.r2) = c("Response", "Model", "R2")

rf.pred = data.frame(matrix(NA, ncol=5, nrow=n_iter2*length(responses)))
rf.pred[,1] = rep(responses,each=n_iter2)
colnames(rf.pred) = c("Response", lv_list)

rf.vimp = data.frame(matrix(NA, ncol=14, nrow=n_iter2*length(responses)))
rf.vimp[,1] = rep(responses,each=n_iter2) 
colnames(rf.vimp) = c("Response", "Lv", stressors, "E1", "E2", "E3")

rf.prediction.all = list()

j = 0
for(i_response in responses){
  
  df.rf.tmp = cbind(df.rf, ES_for_each_all[[i_response]])
  eval(parse(text=(paste("fml      = formula(",i_response,"~", paste(c("Lv", stressors, colnames(ES_for_each_all[[i_response]])), collapse=" + "),")", sep=""))))
  eval(parse(text=(paste("fml.Lv   = formula(",i_response,"~Lv)", sep=""))))
  eval(parse(text=(paste("fml.LvID = formula(",i_response,"~", paste(c("Lv", stressors), collapse=" + "),")", sep=""))))
  
  for(i in 1:n_iter2){
    j = j + 1
    # bootstrap resampling
    set.seed(j)
    # take 10 sample from Lv1, and take 40 sample from the other levels for balanced resampling
    rid = c(sample(id_lv1, n_eachlv, replace=T), sample(id_lvh, n_lvh, replace=T))
    rdf = df.rf.tmp[rid,]
    rf_model      = tryCatch({
      cforest(fml,      data = rdf, control=cforest_control(ntree=n_tree, minsplit = 5, minbucket=2))},
      error = function(e) {cforest(fml.Lv,   data = rdf, control=cforest_control(ntree=n_tree, minsplit = 5, minbucket=2))})
    rf_model.Lv   = cforest(fml.Lv,   data = rdf, control=cforest_control(ntree=n_tree, minsplit = 5, minbucket=2))
    rf_model.LvID = cforest(fml.LvID, data = rdf, control=cforest_control(ntree=n_tree, minsplit = 5, minbucket=2))
    
    # shuffling corresponding variables for evaluating R2 reduction
    # rdf.test.LvID = rdf
    # rdf.test.LvID[,colnames(ES_for_each_all[[i_response]])] = apply(rdf.test.LvID[,colnames(ES_for_each_all[[i_response]])],2,sample)  
    
    # evaluating fitting performance
    rf_prdct.Lv   = tryCatch({predict(rf_model.Lv, OOB=T)}, error=function(e){rep(0,length(rid))})
    rf_prdct.LvID = tryCatch({predict(rf_model.LvID, OOB=T)}, error=function(e){rep(0,length(rid))})
    rf_prdct.All  = tryCatch({predict(rf_model, OOB=T)}, error=function(e){rep(0,length(rid))})
    
    rf.r2[3*j-2,3]    = postResample(rf_prdct.Lv,rdf[,i_response])[2]
    rf.r2[3*j-1,3]    = postResample(rf_prdct.LvID,rdf[,i_response])[2]
    rf.r2[3*j-0,3]    = postResample(rf_prdct.All,rdf[,i_response])[2]
    
    # variable importance
    set.seed(j)
    tmp.vimp = numeric(0)
    for(itmp in 1:5) tmp.vimp = rbind(tmp.vimp,varimp(rf_model))
    tmp.vimp = apply(tmp.vimp,2,mean)
    tmp.vimp = tmp.vimp/sum(tmp.vimp)
    rf.vimp[j, 2:(length(tmp.vimp)+1)] = tmp.vimp*rf.r2[2*j-0,3]*100
    
    # fitting curve
    tmp.curve = c()
    for(i_lv in lv_list){
      tmp.curve = append(tmp.curve, mean(rf_prdct.All[rdf[,"Lv"]==i_lv]))
    }
    rf.pred[j,2:5]    =  tmp.curve
  }
  
  rf_model      = cforest(fml,      data = df.rf.tmp, control=cforest_control(ntree=n_tree, minsplit = 5, minbucket=2))
  rf_model.Lv   = cforest(fml.Lv,   data = df.rf.tmp, control=cforest_control(ntree=n_tree, minsplit = 5, minbucket=2))
  rf_model.LvID = cforest(fml.LvID, data = df.rf.tmp, control=cforest_control(ntree=n_tree, minsplit = 5, minbucket=2))
  
  rf.prediction.all[[i_response]] = data.frame(
    Model     = rep(c("Lv", "LvID", "All"), each = nrow(df.rf.tmp)),
    Predicted = c(predict(rf_model.Lv, OOB=F), predict(rf_model.LvID, OOB=F), predict(rf_model, OOB=F)),
    Observed  = rep(df.rf.tmp[,i_response], 3))
  
}
rf.r2.summary = data.frame(matrix(NA,ncol=5,nrow=3*length(responses)))
colnames(rf.r2.summary) = c("Response","Model", "CI.low", "Mean", "CI.high")
rf.r2.summary[,1] = rep(responses,each=3)
rf.r2.summary[,2] = rep(c("Lv", "Lv+ID", "All"),length(responses))
rf.r2.summary[,2] = factor(rf.r2.summary[,2],levels=c("Lv", "Lv+ID", "All"))


rf.pred.summary = data.frame(matrix(NA,ncol=5,nrow=4*length(responses)))
colnames(rf.pred.summary) = c("Response","Lv","CI.low", "Mean", "CI.high")
rf.pred.summary[,1] = rep(responses,each=length(lv_list))
rf.pred.summary[,2] = rep(lv_list, length(responses))

rf.vimp.summary = data.frame(matrix(NA,ncol=5,nrow=13*length(responses)))
colnames(rf.vimp.summary) = c("Response","Variable","CI.low", "Mean", "CI.high")
rf.vimp.summary[,1] = rep(responses,each=13)
rf.vimp.summary[,2] = rep(colnames(rf.vimp)[2:14], length(responses))
rf.vimp.summary[,2] = factor(rf.vimp.summary[,2], levels = unique(rf.vimp.summary[,2]))

j  = 0
for(i_response in responses){
  jj = 0
  jjj = 0
  rf.r2.summary[3*j+1,3:5] = quantile(rf.r2[which(rf.r2[,1]==i_response&rf.r2[,2]=="Lv"),   3],c(.025,.50,.975), na.rm=T)
  rf.r2.summary[3*j+2,3:5] = quantile(rf.r2[which(rf.r2[,1]==i_response&rf.r2[,2]=="Lv+ID"),3],c(.025,.50,.975), na.rm=T)
  rf.r2.summary[3*j+3,3:5] = quantile(rf.r2[which(rf.r2[,1]==i_response&rf.r2[,2]=="All"),  3],c(.025,.50,.975), na.rm=T)
  
  for(i_lv in lv_list){
    jj = jj + 1
    rf.pred.summary[4*j+jj,3:5]     = quantile(rf.pred[which(rf.pred[,1]==i_response),as.character(i_lv)],c(.025,.50,.975), na.rm=T)
  }
  
  for(i_var in colnames(rf.vimp)[2:14]){
    jjj = jjj + 1
    rf.vimp.summary[13*j+jjj,3:5]     = quantile(rf.vimp[which(rf.vimp[,1]==i_response),as.character(i_var)],c(.025,.50,.975), na.rm=T)
  }
  
  j = j + 1
  
}


# write.csv(rf.r2.summary, "randomforest_r2_summary.csv")
rf.pred.summary<- na.omit(rf.pred.summary)

g_rf_all     = list()
g_vimp_all   = list()
g_r2_all     = list()
g_correl_all = list()
for(i in 1:(length(responses))){
  g_rf_all[[i]] =  local({
    i = i
    ggrf_select = rf.pred.summary[rf.pred.summary[,1]==responses[i],]
    ggdf_select = df.rf[,c("Lv",responses[i])]
    
    ggplot(data=ggrf_select)+
      geom_point(data=ggdf_select, aes_string(x="Lv",y=responses[i]))+
      geom_line(aes(x=Lv, y=Mean)) +
      geom_ribbon(aes(x=Lv,  ymax=CI.high, ymin=CI.low), colour = NA, fill="#999999",alpha=.5)+
      theme_bw()+
      theme(legend.position = 'none', axis.title.x=element_blank(),axis.title.y=element_blank())
  })  
  g_vimp_all[[i]] = local({
    i = i
    ggplot(data=rf.vimp.summary[rf.vimp.summary[,1]==responses[i],],aes(x=Variable, y=Mean))+
      geom_bar(stat="identity", fill="#999999", alpha=0.5) +
      xlab("Variability explained [%]")+
      geom_errorbar(aes(ymax=CI.high, ymin=CI.low), width=.2, position=position_dodge(width=0.0)) +
      theme_bw()+
      theme(legend.position = 'none', axis.title.x=element_blank(),axis.title.y=element_blank())
  })
  g_r2_all[[i]] = local({
    i = i
    rf.r2 = rf.r2
    rf.r2.summary = rf.r2.summary 
    ggplot(data=rf.r2[rf.r2[,1]==responses[i],],aes(x=Model,y=R2, fill=Model))+
      geom_violin( color="#00000000",alpha=.5,position=position_dodge(width=0.3),trim=F)+
      geom_pointrange(data=rf.r2.summary[rf.r2.summary[,1]==responses[i],], aes(y=Mean, ymax=CI.high, ymin=CI.low,color=Model), position=position_dodge(width=0.2)) +
      scale_fill_manual(values  = c('#999999',"#999999",  "#999999",  "#999999"))+ 
      scale_color_manual(values = c("#008900AA","#ff0083AA", "#0000A0AA", "#505050"))+ 
      theme_bw() + ylim(c(0,1.0))+
      theme(axis.title.y=element_text(size=8))+
      ylab('Variability explained (R2%)')+
      theme(legend.position = 'none', axis.title.x=element_blank())
  })
  
  g_correl_all[[i]] = local({
    i = i
    rf.prediction.all = rf.prediction.all
    ggplot(data = rf.prediction.all[[i]], aes(y=Predicted, x=Observed, group=Model, color=Model))+
      theme_bw()+
      geom_abline(slope=1, intercept=0) + geom_point() + geom_smooth(method="lm", fullrange=F,size= 1.5) +
      xlim(range(rf.prediction.all[[i]][,2:3])) + ylim(range(rf.prediction.all[[i]][,2:3])) +
      theme(legend.position = 'none', axis.title.x=element_blank(),axis.title.y=element_blank()) +
      scale_color_manual(values = c("#599a9780", "#1e3a6180", "#c9b75f80"))
  }) 
  
}  

#### poca for single factors ####

setwd('C:/Huiying data/2nd_chapter/main_exp')
df<- read.csv('df_main.csv')

df_single<- df %>%
  filter(lv == "1")
df_single$Lv<- NULL

colnames(df_single)[23]<- 'group'
df_single_d<-  df_single %>%
  filter(D == "1")
df_single_ct<-  df_single %>%
  filter(D == "0")

columns_to_summarize <- colnames(df_single_ct)[12:20]
standardized_sig_facts <- df_single_ct %>%
  group_by(group) %>%
  summarise(across(all_of(columns_to_summarize), ~ mean(.x, na.rm = TRUE), .names = "mean_{.col}")) %>%
  as.data.frame() %>%
  column_to_rownames(var = "group") %>%
  scale()

single2<- data.frame(standardized_sig_facts)
a_single2<- data.frame(t(single2[-1]))

library('vegan')
dist<- vegdist(single2,method='euclidean',diag=T,upper=T)
dist<- as.matrix(dist)

pcoa<- cmdscale(dist,eig=T)
eig<- summary(eigenvals(pcoa))
aixs<- paste0('PCoA',1:ncol(eig))
eig<- data.frame(aixs,t(eig)[,-3])
pco1<- round(eig[1,3]*100,3)
pco2<- round(eig[2,3]*100,3)
xlab<- paste0('PCoA1(',pco1,'%)')
ylab<- paste0('PCoA2(',pco2,'%)')
pcoa_points<- as.data.frame(pcoa$points)

pcoa_points<- data.frame(pcoa_points,group=c('organic','organic','organic','inorganic','inorganic','inorganic','microbial','microbial','microbial'))

pcoa_points<- data.frame(pcoa_points,group=c('organic','organic','organic','organic','organic','organic','inorganic','inorganic','inorganic','inorganic','inorganic','inorganic','microbial','microbial','microbial','microbial','microbial','microbial'))

ggplot(pcoa_points,aes(V1,V2)) +
  geom_point(size=4,aes(color=group))+
  #geom_polygon(data = group_border, aes(fill = group),alpha=0.6)+
  #填充颜色，设置颜色透明度  
  geom_text(aes(label=rownames(pcoa_points)), size=4, vjust=1, hjust=0)+
  #添加样本名称,名称位置大小  
  labs(x=xlab,y=ylab)+ 
  #添加x、y轴标题和图标题 
  #stat_ellipse(aes(fill = group), geom = 'polygon', level = 0.95, alpha = 0.1, show.legend = FALSE) +
  #添加置信椭圆  
  #theme(plot.title = element_text(size = 12,hjust = 0.5))+  
  geom_hline(yintercept=0, linetype=4) +        
  #添加原点垂直辅助线  
  geom_vline(xintercept=0 ,linetype=4)+ 
  #添加原点水平辅助线  
  theme_bw()+  
  theme(plot.title = element_text(hjust = 0.5))+  
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())
