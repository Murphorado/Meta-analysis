setwd("C:/Users/murph/OneDrive - University of Edinburgh/Masters/Eco Evo Responses to Climate Change/Meta-analysis")
install.packages("metafor",dependencies=TRUE,repos="https://cloud.r-project.org")
library(metafor)
library(dplyr)
install.packages("weightr")
library(weightr)

install.packages("devtools")
library(devtools)

devtools::install_github("dsquintana/metameta")
library(metameta)

dat_ci <- read.csv("dat_ci.csv", sep=",")
View(dat_ci)
power <- mapower_ul(dat=dat_ci, observed_es=1.34, name = "dataset")
power_dat <- power$dat
power_dat <- power_dat %>% select(1:8)
power_dat

power_med <- power$power_median_dat

list_power <- list(power_med)

fp <- firepower(list_power)
fp

library(readxl)
dataset <- read_excel("Data.xlsx")
View(dataset)
par(mfrow=c(1,1))

plot(dataset$effect_size,(1/dataset$sampling_variance),xlab="Hedge's d",ylab="Precision, (1/vd)")

par(mfrow=c(1,2))
plot(dataset$effect_size,dataset$n,xlab="Hedge's d",ylab="Sample size")
plot(dataset$effect_size,(1/dataset$sampling_variance),xlab="Hedge's d",ylab="Precision, (1/vd)")

sigd<-which(dataset$p_value<0.05)
plot(dataset$effect_size,dataset$n,xlab="Hedge's d",ylab="Sample size")
points(dataset$effect_size[sigd],dataset$n[sigd],pch=16,col="red")
abline(v=1.34,lty=2)
plot(dataset$effect_size,(1/dataset$sampling_variance),xlab="Hedge's d",ylab="Precision, (1/vd)")
points(dataset$effect_size[sigd],(1/dataset$sampling_variance[sigd]),pch=16,col="red")
abline(v=1.34,lty=2)

dataset$vd2<-dataset$sampling_variance^2

meta<-rma(yi=effect_size, vi=sampling_variance, slab=paste(authors,year,sep=", "), data=dataset)
summary(meta)

meta2 <- rma(yi=effect_size, vi=sampling_variance, mods = ~ lux_difference, data=dataset)
summary(meta2)

inf <- influence(meta)
print(inf)

forest(meta, cex.lab=0.8, cex.axis=0.8, addfit=TRUE, shade="zebra",order="obs")
text(-5, 11, "Author(s), Year", pos=2, cex=0.8)
text(5, 11, "Hedge's d [95% CI]", pos=4, cex=0.8)
funnel(meta)

meta3 <- rma(yi=effect_size, vi=sampling_variance, mods = ~ species, data=dataset)
meta3

meta4 <- rma(yi=effect_size, vi=sampling_variance, mods = ~ sex, data=dataset)
meta4

meta5 <- rma.mv(yi=effect_size, V=sampling_variance, mods=~lux_difference,random=~1|species,data=dataset)
meta5

confint(meta)



forest(meta,cex.lab=0.8,cex.axis=0.8,addfit=TRUE,shade="zebra",order="obs")
funnel(meta)

regtest(meta)

wf <- weightfunct(dataset$effect_size, dataset$sampling_variance, table=TRUE)
wf




dataset<-dataset[is.na(dataset$effect_size)==FALSE,]
dataset$publish<-0
#
dataset$publish[dataset$p_value<=0.025]<-1
largesamplesize<-intersect(which(dataset$p_value>0.05),which(dataset$n>30))
retainlarge<-largesamplesize[as.logical(rbinom(length(largesamplesize),prob=0.75,size=1))]
dataset$publish[retainlarge]<-1
smallsamplesize<-intersect(which(dataset$p_value>0.05),which(dataset$n<=30))
retainsmall<-smallsamplesize[as.logical(rbinom(length(smallsamplesize),prob=0.25,size=1))]
dataset$publish[retainsmall]<-1


plot(dataset$effect_size,(1/dataset$sampling_variance),xlab="Hedge's d",ylab="Precision, (1/sampling variance)",main="Before")
plot(dataset$effect_size[dataset$publish==1],(1/dataset$sampling_variance[dataset$publish==1]),xlab="Hedge's d",ylab="Precision, (1/sampling variance)",main="After")



install.packages("metaDigitise")
library(metaDigitise)
data2 <- metaDigitise("C:/Users/murph/OneDrive - University of Edinburgh/Masters/Eco Evo Responses to Climate Change/Meta-analysis/Graphs")
View(data2)
