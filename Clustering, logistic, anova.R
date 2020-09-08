# start!
## 데이터 삭제 
setwd('c:/Users/USER/Desktop/문화관광 공모전/data')
library(dplyr)
library(magrittr)
d <- haven::read_sav('국민여가활동조사.sav')
forcluster <- data.frame(data.table::fread('rawdata.csv'))
forcluster%<>%mutate(본인소득=d$Q45A1)
forcluster %<>%mutate(가구소득 = forcluster$Q45A2)
for(i in 1:10498) {
  if(forcluster$가구소득[i]==1){
    forcluster$가구소득[i]<-0
  } else if(forcluster$가구소득[i]==2) {
    forcluster$가구소득[i]<-500000
  } else if(forcluster$가구소득[i]==3) {
    forcluster$가구소득[i]<-1500000
  } else if(forcluster$가구소득[i]==4) {
    forcluster$가구소득[i]<-2500000
  } else if(forcluster$가구소득[i]==5) {
    forcluster$가구소득[i]<-3500000
  } else if(forcluster$가구소득[i]==6) {
    forcluster$가구소득[i]<-4500000
  } else if(forcluster$가구소득[i]==7) {
    forcluster$가구소득[i]<-5500000
  } else if(forcluster$가구소득[i]==8) {
    forcluster$가구소득[i]<-6500000
  } else if(forcluster$가구소득[i]==9) {
    forcluster$가구소득[i]<-7500000
  } else if(forcluster$가구소득[i]==10) {
    forcluster$가구소득[i]<-8500000
  } else if(forcluster$가구소득[i]==11) {
    forcluster$가구소득[i]<-9500000
  } else {
    forcluster$가구소득[i]<-10000000
  }
}
for(i in 1:10498) {
  if(forcluster$본인소득[i]==1){
    forcluster$본인소득[i]<-0
  } else if(forcluster$본인소득[i]==2) {
    forcluster$본인소득[i]<-500000
  } else if(forcluster$본인소득[i]==3) {
    forcluster$본인소득[i]<-1500000
  } else if(forcluster$본인소득[i]==4) {
    forcluster$본인소득[i]<-2500000
  } else if(forcluster$본인소득[i]==5) {
    forcluster$본인소득[i]<-3500000
  } else if(forcluster$본인소득[i]==6) {
    forcluster$본인소득[i]<-4500000
  } else if(forcluster$본인소득[i]==7) {
    forcluster$본인소득[i]<-5500000
  } else if(forcluster$본인소득[i]==8) {
    forcluster$본인소득[i]<-6500000
  } else if(forcluster$본인소득[i]==9) {
    forcluster$본인소득[i]<-7500000
  } else if(forcluster$본인소득[i]==10) {
    forcluster$본인소득[i]<-8500000
  } else if(forcluster$본인소득[i]==11) {
    forcluster$본인소득[i]<-9500000
  } else {
    forcluster$본인소득[i]<-10000000
  }
}
forcluster%<>%mutate(한달평균지출액수=forcluster$Q9,만나이 = forcluster$Q36,하루평균소요시간 = (forcluster$Q13A1A1*5 + forcluster$Q13A1A1*2)/7)
forcluster%<>% filter(한달평균지출액수<500000,만나이<75,가구소득<6500000,하루평균소요시간<8)
data <- forcluster

## 클러스터링
library(cluster)
library(purrr)
library(ggplot2)
library(clValid)
scdata <- scale(data%>%select(만나이, 가구소득, 한달평균지출액수, 하루평균소요시간), center=T,scale=T)
vadata <- clValid(scdata, 2:10, clMethods = 'kmeans', validation = 'internal', maxitems = nrow(scdata))
summary(vadata)

attach(clusterdata)
clusterdata <-  data%>%select(만나이, 가구소득, 한달평균지출액수, 하루평균소요시간)

########클러스터링 잘 됨!! 코드 절대 버리지 말기!
set.seed(10000)
kmodel <- kmeans(scale(data%>%select(만나이, 가구소득, 한달평균지출액수, 하루평균소요시간)), center=4)
cldata <- data%>%select(만나이, 가구소득, 한달평균지출액수, 하루평균소요시간)
results<- cldata%>%
  mutate(group = kmodel$cluster)%>%
  group_by(group) %>%
  do(the_summary = summary(.))
results$the_summary
cldata%<>%
  mutate(cluster = kmodel$cluster)%>%
  mutate(본인소득=forcluster$본인소득)
table(cldata$cluster)

hist(as.matrix(cldata%>%filter(cluster==1)%>%select(본인소득)))
hist(as.matrix(cldata%>%filter(cluster==2)%>%select(본인소득)))
hist(as.matrix(cldata%>%filter(cluster==3)%>%select(본인소득)))
hist(as.matrix(cldata%>%filter(cluster==4)%>%select(본인소득)))

data$Q30 %>% table()
data$Q30 <- cut(data$Q30,breaks=c(-Inf,4.1,Inf),labels=F)
data%<>%mutate(Q35 = ifelse(data$Q35==2, 2, 1))
data%<>%mutate(group= kmodel$cluster)
logdata<- data%>%select(Q2A2, Q5, Q9, Q13A1A1, Q13A1A2, Q14, Q19, Q24A1, Q24A2, Q35, group, Q30)
colnames(logdata) <- c('동반자유형', '지속활동여부', '한달지출', '평일시간', '휴일시간', 
                       '휴가여부', '생활권시설이용여부', '스마트평일', '스마트주말', 
                       '결혼여부', '그룹', '만족도')

for(k in c(1,2,6,7,10,11,12)){
  logdata[,k] <- as.factor(logdata[,k])
}
str(logdata)  

group1 <- logdata%>%filter(그룹==1)
group2 <- logdata%>%filter(그룹==2)
group3 <- logdata%>%filter(그룹==3)
group4 <- logdata%>%filter(그룹==4)

group1%<>%select(-그룹)
group2%<>%select(-그룹)
group3%<>%select(-그룹)
group4%<>%select(-그룹) # 로지스틱을 위한 전처리 끝!

library(rgl)
colors <- c("#BEF781","#F78181","#585858",'#81BEF7')
colors <- colors[as.numeric(cldata$cluster)]
plot3d(cldata$만나이,cldata$가구소득,cldata$한달평균지출액수, col=colors)
ggplot(aes(x = 만나이, y = 가구소득), data = cldata) +
  geom_point(aes(color = cluster))

#####################################################################
## binary logistic ##
group1 <- read.csv('log1.csv')
group2 <- read.csv('log2.csv')
group3 <- read.csv('log3.csv')
group4 <- read.csv('log4.csv')
for (k in c(2,6,7,10)){group1[,k] <- ifelse(group1[,k]==2, 0, 1)} ; group1[,11] <- ifelse(group1[,11]==1,0,1)
for (k in c(2,6,7,10)){group2[,k] <- ifelse(group2[,k]==2, 0, 1)} ; group2[,11] <- ifelse(group2[,11]==1,0,1)
for (k in c(2,6,7,10)){group3[,k] <- ifelse(group3[,k]==2, 0, 1)} ; group3[,11] <- ifelse(group3[,11]==1,0,1)
for (k in c(2,6,7,10)){group4[,k] <- ifelse(group4[,k]==2, 0, 1)} ; group4[,11] <- ifelse(group4[,11]==1,0,1)

for(k in c(1,2,6,7,10,11)){group1[,k] <- as.factor(group1[,k])}
for(k in c(1,2,6,7,10,11)){group2[,k] <- as.factor(group2[,k])}
for(k in c(1,2,6,7,10,11)){group3[,k] <- as.factor(group3[,k])}
for(k in c(1,2,6,7,10,11)){group4[,k] <- as.factor(group4[,k])}
library(lmtest)

fit1 <- glm(만족도~.,data=group1, family='binomial')
fit2 <- glm(만족도~.,data=group2, family='binomial')
fit3 <- glm(만족도~.,data=group3, family='binomial')
fit4 <- glm(만족도~.,data=group4, family='binomial')
summary(fit1)
summary(fit2)
summary(fit3)
summary(fit4)

null1 <- glm(만족도~1, data=group1, family= 'binomial')
full1 <- glm(만족도~., data=group1, family= 'binomial')
stepfit1<- step(null1, scope=list(lower=null1,upper=full1), direction = 'both', trace = F)
summary(stepfit1)
finalfit1 <- stepfit1
lrtest(null1, finalfit1)
lrtest(finalfit1,full1)
summary(finalfit1)
exp(coef(finalfit1))
car::vif(finalfit1)

null2 <- glm(만족도~1, data=group2, family= 'binomial')
full2 <- glm(만족도~., data=group2, family= 'binomial')
stepfit2<- step(null2, scope=list(lower=null2,upper=full2), direction = 'both')
summary(stepfit2)
finalfit2 <- stepfit2
finalfit2 <- glm(만족도~동반자유형+지속활동여부+휴가여부, data=group2,family='binomial')
summary(finalfit2)
lrtest(null2, finalfit2)
lrtest(finalfit2,full2)
exp(coef(finalfit2))
car::vif(finalfit2)

null3 <- glm(만족도~1, data=group3, family= 'binomial')
full3 <- glm(만족도~., data=group3, family= 'binomial')
stepfit3<- step(null3, scope=list(lower=null3,upper=full3), direction = 'both')
summary(stepfit3)
finalfit3 <- glm(만족도~평일시간+생활권시설이용여부+한달지출+결혼여부+스마트주말+동반자유형, data=group3, family='binomial')
lrtest(null3, finalfit3)
lrtest(finalfit3,full3)
summary(finalfit3)
exp(coef(finalfit3))
car::vif(finalfit3)

null4 <- glm(만족도~1, data=group4, family= 'binomial')
full4 <- glm(만족도~., data=group4, family= 'binomial')
stepfit4 <- step(null4, scope=list(lower=null4,upper=full4), direction = 'both')
summary(stepfit4)
finalfit4 <- glm(만족도~평일시간+생활권시설이용여부+휴가여부+한달지출+지속활동여부+동반자유형, data=group4, family='binomial')
lrtest(null4, finalfit4)
lrtest(finalfit4,full4)
summary(finalfit4)
exp(coef(finalfit4))
car::vif(finalfit4)

## anova
setwd('c:/Users/USER/Desktop/data')
library(dplyr)
library(magrittr)
data <- data.table::fread('aovdata.csv')
data%<>%mutate(group=as.factor(group))
aovmodel1 <- aov(만나이~group, data=data) # one-way anova
aovmodel2 <- aov(한달평균지출액수~group, data=data)
aovmodel3 <- aov(가구소득~group, data=data)
summary(aovmodel)
oneway.test(만나이~group,data=data,var.equal=F) # welch's anova
t.test(data%>%filter(group==1)%>%select(가구소득), data%>%filter(group==4)%>%select(가구소득),paired =FALSE, var.equal = TRUE, conf.level = 0.95)
t.test(data%>%filter(group==1)%>%select(한달평균지출액수), data%>%filter(group==4)%>%select(한달평균지출액수),paired =FALSE, var.equal = TRUE, conf.level = 0.95)
library(multcomp)
pairwise.t.test(data$만나이,data$group, p.adj='bonf')
pairwise.t.test(data$가구소득,data$group, p.adj='bonf')
##

group1 <- data.table::fread('group1_all.csv')
group4 <- data.table::fread('group4_all.csv')

hist(group1$만나이)
hist(group4$만나이)
