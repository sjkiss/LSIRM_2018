## ----setup---------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, warning=F, message=F,  results='hide', fig.width=4, fig.height=3, fig.align='center')

## ----read-in-------------------------------------------------------------
#ces<-rio::import('https://github.com/sjkiss/LSIRM_2018/blob/master/data/ces_data.sav?raw=true')
library(haven)
library(labelled)
ces<-read_sav('data/ces15_data.sav')


## ---- examine-data-------------------------------------------------------
#Different ways of summarizing the data set
#Check the first few rows
head(ces)
#The last few rows
tail(ces)
#Check the structure of the object
str(ces)
#Summarize the data set
summary(ces)
#See the variables names
names(ces)



## ----get-labels----------------------------------------------------------
var_label(ces)
val_labels(ces)


## ----assignment-2-solution-----------------------------------------------
vars<-var_label(ces)
vals<-val_labels(ces)

## ----assignment-3-solution-----------------------------------------------
vals

## ---- eval=F, echo=T-----------------------------------------------------
## Recode(x, "old value=new value")

## ----results='markup', echo=T--------------------------------------------
#load the car library
library(car)
#Recode 1
test<-car::Recode(as.numeric(ces$p_incineq),"3:1000=NA", as.numeric=T)
#Recode 2
test2<-Recode(as.numeric(ces$p_incineq), "3=NA; 4=NA; 998=NA; 1000=NA", as.numeric=T)
#Summary test
summary(test)
#summary test 2
summary(test2)


## ---- results='hide', echo=T---------------------------------------------
ces$inequal<-Recode(as.numeric(ces$p_incineq), "3:1000=NA")


## ----sex-gender----------------------------------------------------------
#recode sex into gender
ces$gender<-Recode(ces$sex_r, "1='M' ; 2='F'", levels=c('M', 'F'), as.factor=T)


## ----assignment-4-solution-----------------------------------------------
#education
vals$education
#education missing values
ces$education<-Recode(as.numeric(ces$education), "12:1000=NA")
#degree
#check vals
vals
#Recode eduation to degree
ces$degree<-car::Recode(ces$education, "1:7='No degree' ; 8:11='Degree'", levels=c('no degree', 'degree'), as.factor=T)

#Income missing values
vals$income
ces$income<-Recode(as.numeric(ces$income_full), "6:1000=NA")
#Recode p_selfplace
ces$ideology<-Recode(as.numeric(ces$p_selfplace), "1000=NA")
str(ces)


## ----summary-ces---------------------------------------------------------
summary(ces)

## ----results='markup'----------------------------------------------------
library(psych)
desc<-describe(ces)

## ----load-tidyverse------------------------------------------------------
library(tidyverse)

library(ggplot2)
library(dplyr)

## ---- make-plot, fig.cap='No plot!'--------------------------------------


ggplot(ces, aes(x=income))


## ----income-bar,fig.cap='Histogram', fig.align='center'------------------
#Add barplot

  ggplot(ces, aes(x=income))+
  geom_bar()


## ----assignment-6-solution-----------------------------------------------

plot1<-  ggplot(ces, aes(x=ideology))+geom_density()


## ----show-aesthetics-----------------------------------------------------
plot1+geom_density(col='red')
plot1+geom_density(size=3)

## ----linetype-aes, fig.cap='Changing linetype on a densit plot does not do anything.'----
plot1+geom_density(linetype=3)

## ---- make-bivariate-plot, fig.cap='A basic scatterplot'-----------------


  ggplot(ces, aes(x=income, y=ideology))+
  geom_point()

## ----assignment-solution-7-----------------------------------------------
#Scatterplot
plot2<-ggplot(ces, aes(x=education, y=ideology))+geom_point()
plot2
#Boxplot
plot3<-ggplot(ces, aes(x=as.factor(education) ,y=ideology))+geom_boxplot()
plot3

## ---- plot-4,include=T---------------------------------------------------
#Make plot 4, save as plot4
plot4<-ggplot(ces, aes(x=income, y=ideology, group=gender))+geom_point(aes(col=gender))
#Print plot4
plot4


## ----assignment-8-solution-----------------------------------------------
plot4a<-ggplot(ces, aes(x=income, y=ideology, group=gender))+geom_point(aes(shape=gender))
plot4a

## ----plot4b--------------------------------------------------------------
plot4b<-plot4+geom_jitter(aes(col=gender))

plot4b

## ---- plot4b-smooth------------------------------------------------------
plot4b<-plot4+geom_smooth(method='lm', aes(col=gender))
plot4b

## ----assignment-9-solution-----------------------------------------------
plot5<-ggplot(ces, aes(x=income, y=ideology))+geom_point(aes(col=degree))+geom_smooth(aes(col=degree), method='lm')
plot5

## ------------------------------------------------------------------------
plot5+labs(x='Income', y='Ideology', title='Ideology on Income, CES 2015')

## ----correlation---------------------------------------------------------
names(ces)
str(ces)
cor(ces$ideology, ces$income, use='complete.obs')


## ---- table-1, results='markup'------------------------------------------
table(ces$gender, ces$inequal)


## ----assignment-10-solution----------------------------------------------

ces$inequal
#Check values
vals
ces$inequal_cat<-Recode(ces$inequal, "1='Yes' ; 2='No'", as.factor=T)
#Crosstab
tab2<-table(ces$gender, ces$inequal_cat)

## ----chi-sq-prop-table, results='markup'---------------------------------

#Chi-sq.test
chisq.test(tab2)
#For prop.table you can produce either row (1) or column (2) percentgates
#Compare
prop.table(tab2, 1)
#With
prop.table(tab2, 2)

## ------------------------------------------------------------------------
library(cowplot)
plot_grid(plot1, plot2, plot3, plot4)

## ----lm-model1, results='markup'-----------------------------------------
#Fit linear model
mod1<-lm(ideology ~ income, data=ces)

## ----mod2, results='markup'----------------------------------------------
#Model 2
mod2<-lm(ideology~income+education, data=ces)
#summarize
summary(mod2)

## ----mod3, results='markup'----------------------------------------------
#Model 3
mod3<-lm(ideology~income+education+income:education, data=ces)
#Summarize
summary(mod3)

## ---- update-mod2--------------------------------------------------------
#New model
mod3a<-update(mod2, .~.+income:education, data=ces)
#Compare
summary(mod3)
summary(mod3a)

## ----assignment-11-solution----------------------------------------------
m1<-lm(p_gap~income, data=ces)
m2<-update(m1, .~+ideology, data=ces)
m3<-update(m2, .~+education, data=ces)
m4<-update(m3, .~+gender, data=ces)
m5<-update(m1, .~.+ideology+education+gender, data=ces)
m6<-update(m5, .~.+gender:income+gender:ideology+gender:education, data=ces)
summary(m1)
summary(m2)
summary(m3)
summary(m4)
summary(m5)
summary(m6)

## ----report-regressions, results='asis'----------------------------------
#load library
library(stargazer)
stargazer(m1, m2, m3, m4, m5, m6, type='text')

## ----export-regressions, results='hide'----------------------------------
stargazer(m1, m2, m3, m4, m5, m6, type='html', out='model_results.html')

## ----assignment-12-solution, results='asis'------------------------------
#update m5
m7<-update(m5, .~.+ideology:gender, data=ces)
summary(m7)

## ------------------------------------------------------------------------

library(ggeffects)

## ----predicted-values, results='markup'----------------------------------
m7.preds<-ggpredict(m7, terms=c('ideology [0,3, 5,7,10]', 'gender'))
#summarize
m7.preds
ggplot(m7.preds)
ggplot(m7.preds, aes(x=x, y=predicted, col=group))+geom_point()+geom_line()+labs(x='Ideology')

## ----assignment-13-solution----------------------------------------------
m7.preds2<-ggpredict(m7, terms=c('ideology [0,3, 5,7,10]', 'income', 'gender'))
#plot
ggplot(m7.preds2, aes(x=x, y=predicted, col=group))+facet_grid(~facet)+geom_point()+geom_line()+labs(x='Ideology', col='Income')


