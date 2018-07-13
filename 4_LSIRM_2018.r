## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, warning=F, message=F, results='hide', fig.width=4, fig.height=3, fig.align='center')


## ----load-libraries------------------------------------------------------

library(haven)
library(labelled)
library(tidyverse)
library(car)

#read in data 
ces_full<-read_sav('https://github.com/sjkiss/LSIRM_2018/raw/master/data/CES15_CPS%2BPES_Web_SSI%20Full.SAV')

## ----inspect-------------------------------------------------------------
head(ces_full)
str(ces_full)


ces.vars<-var_label(ces_full)
ces.vals<-val_labels(ces_full)

## ------------------------------------------------------------------------
library(questionr)
#Sex
lookfor(ces_full, 'sex')
# 
lookfor(ces_full, 'leader')


## ----select-help---------------------------------------------------------
?select

## ----select--------------------------------------------------------------
#OPtion 1
ces_full %>% 
  select(contains('emecon'))
#Optino 2
ces_full %>% 
select(p_emecon_angr:p_emecon_hope)

## ------------------------------------------------------------------------
describe(ces_full, 'economy')

## ----recode-test---------------------------------------------------------

#Take the data.frame
ces_full %>% 
  #select only the emecon variables
  select(contains('emecon')) %>% 
  #We only want to mutate a subset of variables, so we select 
  mutate_all(funs(Recode(., "4:1000=NA"))) %>% 
  #Do a summary
  summary()

## ------------------------------------------------------------------------
ces_full %>% 
  #select only the emecon variables
  select(contains('emecon')) %>% 
  #We only want to mutate a subset of variables, so we select 
  mutate_all(funs(Recode(., "4:1000=NA"))) %>% 
  as_factor()

## ----add-prefix----------------------------------------------------------
ces_full %>% 
  #select only the emecon variables
  select(contains('emecon')) %>% 
  #We only want to mutate a subset of variables, so we select 
  mutate_all(funs(out=Recode(., "4:1000=NA"))) %>% 
  as_factor() %>% 
  select(contains('out')) -> econ_feeling
#Check 
econ_feeling
#Check
summary(econ_feeling)

## ----assignment-4-solution-----------------------------------------------
#take the data frame
econ_feeling %>% 
  #Gather 
  gather(Emotion,Frequency) %>% 
  #Filter out missing values
  na.omit() %>% 
  #count
  count(Emotion, Frequency) %>% 
  #plot
  ggplot(. , aes(x=Frequency, y=n, fill=Frequency))+geom_col(position='dodge')+facet_grid(~Emotion)


## ----rename--------------------------------------------------------------
#See Rename variable, store back in econ_feeling
econ_feeling<-econ_feeling %>% 
  rename(., Angry=p_emecon_angr_out, Enthusiastic=p_emecon_enth_out,Fear= p_emecon_fear_out, Hope=p_emecon_hope_out)
#Replot
econ_feeling %>% 
  #Gather 
  gather(Emotion,Frequency) %>% 
  #Filter out missing values
  na.omit() %>% 
  #count
  count(Emotion, Frequency) %>% 
  #plot
  ggplot(. , aes(x=Frequency, y=n, fill=Frequency))+geom_col(position='dodge')+facet_grid(~Emotion)


## ----select-economy------------------------------------------------------
library(psych)
#We have the variables of interests as factors
econ_feeling %>% 
  #
  mutate_all(funs(as.numeric(.))) %>% 
  cor(., use='complete.obs')


## ----check-levels--------------------------------------------------------
str(econ_feeling)
#Check value labels from the underlying variable
val_labels(ces_full$p_emecon_angr)
#Check levels of recoded variable
levels(econ_feeling$p_emecon_fear_out)
#Table recoded variable
table(econ_feeling$p_emecon_angr_out)


## ----visualize-correlation-matrix----------------------------------------
#You will need to install this package
#install.packages('GGally')
#load library
library(GGally)
#Start with econ_feeling
econ_feeling %>% 
  #convert to numric
  mutate_all(funs(as.numeric(.))) %>% 
  ggpairs(.)

## ---- results='hide'-----------------------------------------------------
source('https://raw.githubusercontent.com/sjkiss/LSIRM_2018/master/3_LSIRM_2018.r')

## ----bind-columns--------------------------------------------------------
#Take the data frame from yesterda
ces %>% 
  #bind columns to it from econ_feeling
  bind_cols(., econ_feeling) -> ces

## ----dput----------------------------------------------------------------
dput(ces[1:5,1:5])

## ----rnorm---------------------------------------------------------------
#Make variable 1
var1<-rnorm(100, mean=5, sd=1)

## ----sample--------------------------------------------------------------
var2<-sample(c('M', 'F'), replace=T, size=100)
var3<-sample(c('young', 'old', 'older', 'oldest'), replace=T, size=100)

## ----combine-------------------------------------------------------------
help.df<-data.frame(var1, var2, var3)
#show
str(help.df)

