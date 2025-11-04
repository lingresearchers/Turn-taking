# Turn-taking predicts vocabulary acquisition (BUCLD)

install.packages("xlsx")
library(xlsx)
data<-read.xlsx2("C:/Users/lvfeu/Desktop/PhD/University of Zurich/Paper 1/Kuhl dataset.xlsx", sheetName="Sheet1") 
names(data)
data$Subject.ID<-as.vector(as.numeric(data$Subject.ID))
data$LM_6m_babbling_pct<-as.numeric(data$LM_6m_babbling_pct) # percentage child babbling


data$LA6mAvgD1D2_AWC.Proj<-as.numeric(data$LA6mAvgD1D2_AWC.Proj) # AWC 12-hour projection
data$LM_6m_parentese_pct<-as.numeric(data$LM_6m_parentese_pct) # percentage CDS (100 30-sec clips with highest AWC)

data$LM_6m_standard_pct<-as.numeric(data$LM_6m_standard_pct) # percentage ADS
data$LA6mAvgD1D2_CTC.Proj<-as.numeric(data$LA6mAvgD1D2_CTC.Proj) # Conversational turn count
data$SES<-as.numeric(data$SES) # SES (Hollingshead Index between 8 and 66)
data$LA10mAvgD1D2_AWC.Proj<-as.numeric(data$LA10mAvgD1D2_AWC.Proj)
data$LM10m_parentese_pct<-as.numeric(data$LM10m_parentese_pct)
data$LM10m_standard<-as.numeric(data$LM10m_standard)


data$LA10mAvgD1D2_CTC.Proj<-as.numeric(data$LA10mAvgD1D2_CTC.Proj)
data$LA14mAvgD1D2_AWC.Proj<-as.numeric(data$LA14mAvgD1D2_AWC.Proj)
data$LM14m_parentese_pct<-as.numeric(data$LM14m_parentese_pct)
data$LM14m_standard<-as.numeric(data$LM14m_standard)
data$LA14mAvgD1D2_CTC.Proj<-as.numeric(data$LA14mAvgD1D2_CTC.Proj)
data$LA18mAvgD1D2_AWC.Proj<-as.numeric(data$LA18mAvgD1D2_AWC.Proj)
data$LM_18m_parentese_pct<-as.numeric(data$LM_18m_parentese_pct)

data$LM_18m_standard_pct<-as.numeric(data$LM_18m_standard_pct)
data$LA18mAvgD1D2_CTC.Proj<-as.numeric(data$LA18mAvgD1D2_CTC.Proj)
data$LM_18m_baby_words<-as.numeric(data$LM_18m_baby_words) # percentage child English words
data$VOCAB_18mo_CDI<-as.numeric(data$VOCAB_18mo_CDI) # production at the end
str(data) # seems right

library(tidyverse)

# Average for parentese (CDS)
select_vars <- c("LM_6m_parentese_pct", "LM10m_parentese_pct", "LM14m_parentese_pct", "LM_18m_parentese_pct")
data<-data %>% mutate(avg.CDS = rowMeans(dplyr::select(., select_vars)))

# missing values (data for 18m only missing)
data$avg.CDS[42]<-mean(c(data$LM_6m_parentese_pct[42], data$LM10m_parentese_pct[42], data$LM14m_parentese_pct[42]))

# Average for standard (ADS)
select_vars2 <- c("LM_6m_standard_pct", "LM10m_standard", "LM14m_standard", "LM_18m_standard_pct")
data<-data %>% mutate(avg.ADS = rowMeans(dplyr::select(., select_vars2)))

# Average for CTC
select_vars3 <- c("LA6mAvgD1D2_CTC.Proj", "LA10mAvgD1D2_CTC.Proj", "LA14mAvgD1D2_CTC.Proj", "LA18mAvgD1D2_CTC.Proj")
data<-data %>% mutate(avg.CTC = rowMeans(dplyr::select(., select_vars3)))

# missing values
data$avg.CTC[42]<-mean(c(data$LA6mAvgD1D2_CTC.Proj[42], data$LA10mAvgD1D2_CTC.Proj[42], data$LA14mAvgD1D2_CTC.Proj[42]))
# missing values (data for 10m only missing)
data$avg.CTC[71]<-mean(c(data$LA6mAvgD1D2_CTC.Proj[71], data$LA14mAvgD1D2_CTC.Proj[71], data$LA18mAvgD1D2_CTC.Proj[71] ))

str(data)

# Include N from another corpus if necessary

data<-data %>% dplyr::add_row("Subject.ID"=72, "SES" = 49.5, "VOCAB_18mo_CDI" = 58, "avg.CTC"=525, "avg.CDS" = 43.8, "avg.ADS" = 56.2)

# New dataframe instead of "attach(data)"

Participant<-data$`Subject.ID`
avg.CDS<-data$avg.CDS
# avg.ADS<-data$avg.ADS
avg.CTC<-data$avg.CTC
SES<-data$SES
VOCAB_18mo_CDI<-data$VOCAB_18mo_CDI

data<-data.frame(Participant, VOCAB_18mo_CDI, avg.CDS, avg.CTC, SES)


# HGLM

data<-na.omit(data) 
str(data)

library(hglm)
turns.pois<-hglm(fixed = VOCAB_18mo_CDI ~ -1 + avg.CDS + avg.CTC*SES,
                 random = ~ 1|Participant,
                 family = poisson(link = log),
                 rand.family = gaussian(link = identity), data=data, calc.like = TRUE)
summary(turns.pois) 

lrt(turns.pois)
plot(turns.pois)

# Predictions for plotting

data$Prediction<-round(exp(turns.pois$fixef[1]*data$avg.CDS +
                             turns.pois$fixef[2]*data$avg.CTC +
                             turns.pois$fixef[3]*data$SES +
                             turns.pois$fixef[4]*(data$SES*data$avg.CTC)))


exp(-1.116e-02) # CDS: 0.989
exp(1.151e-02) #CTC: 1.012
exp(6.770e-02) #SES: 1.07
exp(-1.591e-04) #Interaction: 0.9998

# order data by vocabulary

data<-data[order(data$VOCAB_18mo_CDI),]
data$Participant<-1:70

# plot

library(ggplot2)
library(ggthemes) 


gghglm<-ggplot(data, aes(x = Participant)) +
  geom_point(aes(y = VOCAB_18mo_CDI, color = "Vocabulary observed"), size=2)+
  geom_point(aes(y = Prediction, color= "Vocabulary predicted"), size=2) +
  geom_smooth(aes(y = Prediction, color = "Vocabulary predicted"), method = "loess") +
  ylab("Vocabulary at 18mo") + xlab("Participants ordered by vocabulary size") +
  scale_color_manual(values = c("black", "red")) +
  scale_fill_manual(values = c("black", "red")) +
  guides(color=guide_legend("Values"))+
  theme_economist_white() +
  theme(axis.text = element_text(size = 15))  +
  theme(axis.title = element_text(size = 20, face="bold")) +
  theme(legend.position = c(0.2, 0.8),
        axis.line = element_line(color="black", size = 0.4) +
          theme_economist()  ) 

ggsave(path = "C:/Users/lvfeu/Desktop/PhD/University of Zurich/Paper 1", width = 16, height = 9, device='pdf', , filename="Turn_taking_Feurstein.pdf", dpi=700)

range(data$VOCAB_18mo_CDI)
mean(data$VOCAB_18mo_CDI)
sd(data$VOCAB_18mo_CDI)

range(data$SES)
mean(data$SES)
sd(data$SES)

range(data$avg.CDS)
mean(data$avg.CDS)
sd(data$avg.CDS)

range(data$avg.CTC)
mean(data$avg.CTC)
sd(data$avg.CTC)

# Variable importance

install.packages("randomForest")
library(randomForest)
#set.seed(71)
rf <-randomForest(VOCAB_18mo_CDI~avg.CTC+avg.CDS+SES,data=data, importance=TRUE, ntree=100000) 
print(rf)
importance(rf)
varImpPlot(rf)

vi_rfo <- rf$variable.importance
install.packages("vip")
library(vip)
vi(rf)
p2 <- vip(rf) + ggtitle("Random forest")

# plot
library(ggplot2) # for theme_light() function
vip(rf, num_features = 5, geom = "point", horizontal = FALSE,
    aesthetics = list(color = "red", shape = 17, size = 8)) +
  theme_light() + ggtitle("Random forest")


