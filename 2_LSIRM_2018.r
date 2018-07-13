## ----setup---------------------------------------------------------------

knitr::opts_chunk$set(echo = TRUE, warning=F, message=F,  results='hide', fig.width=4, fig.height=3, fig.align='center')


## ----read-in-------------------------------------------------------------
#read-in
#load haven
library(haven)
#load labelled
library(labelled)

ces<-read_sav('https://github.com/sjkiss/LSIRM_2018/raw/master/data/ces15_data.sav')


## ---- examine-data-------------------------------------------------------
###examine-data
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
#get-labels
var_label(ces)
val_labels(ces)


## ----assignment-2-solution-----------------------------------------------
vars<-var_label(ces)
vals<-val_labels(ces)

## ----assignment-3-solution, results='markup'-----------------------------
vals

## ---- eval=F, echo=T-----------------------------------------------------
## Recode(x, "old value=new value")

## ----results='markup', echo=T--------------------------------------------
#load the car library
library(car)
#Recode 1
test<-Recode(ces$p_incineq,"3:1000=NA")
#Recode 2
test2<-Recode(ces$p_incineq, "3=NA; 4=NA; 998=NA; 1000=NA")
#Summary test
summary(test)
#class test
class(test)
#summary test 2
summary(test2)


## ---- results='hide', echo=T---------------------------------------------

ces$inequal<-Recode(ces$p_incineq, "1='Yes' ; 2='No'; 3:1000=NA", levels=c('No', 'Yes'), as.factor=T)
vals

## ----assignment-4-solution-----------------------------------------------
#education
vals$education
#education missing values
ces$education<-Recode(ces$education, "12:1000=NA")
#degree
#check vals
vals
#Recode eduation to degree
ces$degree<-Recode(ces$education, "1:7='No degree' ; 8:11='Degree'", levels=c('No degree', 'Degree'), as.factor=T)

#Income missing values
ces$income<-Recode(ces$income_full, "6:1000=NA")

#Recode p_selfplace
ces$ideology<-Recode(ces$p_selfplace, "1000=NA")

#recode gender
ces$gender<-Recode(ces$sex_r, "1='M' ; 2='F'", levels=c('M', 'F'), as.factor=T)


## ----summary-ces---------------------------------------------------------
#check summary
summary(ces)
#check structure
str(ces)

## ----results='markup'----------------------------------------------------
library(psych)
desc<-describe(ces)

## ----load-tidyverse------------------------------------------------------
#load full tidyverse
library(tidyverse)


## ---- make-plot, fig.cap='No plot!'--------------------------------------


ggplot(ces, aes(x=income))


## ----income-bar,fig.cap='Histogram', fig.align='center'------------------
#Add barplot

  ggplot(ces, aes(x=income))+
  geom_bar()


## ----assignment-6-solution-----------------------------------------------
#save plot1
plot1<- ggplot(ces, aes(x=ideology))+geom_density()

#print
plot1

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
plot4<-ggplot(ces, aes(x=income, y=ideology,col=gender))+geom_point()
#Print plot4
plot4


## ------------------------------------------------------------------------
plot4<-ggplot(na.omit(ces), aes(x=income, y=ideology,col=gender))+geom_point()
#Print plot4
plot4

## ----assignment-8-solution-----------------------------------------------
plot4a<-ggplot(na.omit(ces), aes(x=income, y=ideology, shape=gender))+geom_point()
plot4a

## ----plot4b--------------------------------------------------------------
plot4b<-plot4+geom_jitter(aes(col=gender))

plot4b

## ---- plot4b-smooth------------------------------------------------------
plot4b<-plot4+geom_smooth(method='lm', aes(col=gender))
plot4b

## ----show-facet----------------------------------------------------------

plot4b+facet_grid(~degree)


## ----assignment-9-solution-----------------------------------------------
plot5<-ggplot(na.omit(ces), aes(x=income, y=ideology))+geom_point(aes(col=degree))+geom_smooth(aes(col=degree), method='lm')
plot5

## ------------------------------------------------------------------------
plot5+labs(x='Income', y='Ideology', title='Ideology on Income, CES 2015')

## ----show-cowplot--------------------------------------------------------
library(cowplot)
plot_grid(plot1, plot2, plot3, plot4)

## ----correlation---------------------------------------------------------
names(ces)
str(ces)
cor(ces$ideology, ces$income, use='complete.obs')


## ----tab1, results='markup', echo=T--------------------------------------
#save the table in table1
tab1<-table(ces$gender, ces$inequal)
#print table1
tab1

## ----tab1-output, results='hide', echo=T,eval=F--------------------------
## #Note load library
## library(xtable)
## #Print html table, note if using a PC you must change this.
## print(xtable(tab1, caption='Inequality is a problem by gender'),  type='html', file='~/Desktop/tab1.html')

## ----chi-sq-prop-table, results='markup'---------------------------------

#Chi-sq.test
chisq.test(tab1)
#For prop.table you can produce either row (1) or column (2) percentgates
#Compare
prop.table(tab1, 1)
#With
prop.table(tab1, 2)

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
#Check 
ces$p_gap
ces$gap<-Recode(ces$p_gap, "1=4; 2=3; 3=2; 4=1; 6:1000=NA")
#MODEL
m1<-lm(gap~income, data=ces)
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
#load broom
library(broom)
#Augment the model m7 with the diagnostic data, store the full data frame in m7.diag
m7.diag<-augment(m7)

## ----fitted-residuals----------------------------------------------------
ggplot(m7.diag, aes(x=.fitted, y=.resid))+geom_point()+geom_smooth(method='loess')

## ----qq-line-------------------------------------------------------------

ggplot(m7.diag)+geom_qq(aes(sample=m7.diag$.std.resid))


## ----cooks---------------------------------------------------------------
ggplot(m7.diag, aes(x=.fitted, y=.cooksd))+geom_point()+geom_smooth(method='loess')

## ----load-ggeffects------------------------------------------------------

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


