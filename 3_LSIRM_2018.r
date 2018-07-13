## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, warning=F, message=F, results='hide', fig.width=4, fig.height=3, fig.align='center')


## ----read-in-------------------------------------------------------------
source('https://raw.githubusercontent.com/sjkiss/LSIRM_2018/master/2_LSIRM_2018.r')



## ----table1, results='markup'--------------------------------------------

#print table1
table1


## ----show-long-data, results='markup'------------------------------------
#Show table2
table2

## ----gather-table-1, results='markup', echo=T----------------------------

  #Gather the data frame, name new columns Variable, Value, do not touch country and year
gather(table1, Variable, Value, cases, population) 


## ---- show-wide-data, results='markup'-----------------------------------
#print vars
print(vars)

## ----examine-personal-federal,  echo=T, results='markup'-----------------
#Show pers_ret
head(ces$pers_ret)
#Show pers_fdpol
head(ces$pers_fdpol)

## ----assignment-3-solution-----------------------------------------------
ces$personal<-Recode(ces$pers_ret,
                     "1='Better' ; 2='Worse' ; 3='Same'", 
                     levels=c('Worse', 'Same' ,'Better'), as.factor=T)

#federal policies
ces$federal<-Recode(ces$pers_fdpol,
                    "1='Better' ; 2='Worse' ; 3='Same'", 
                    levels=c('Worse', 'Same' ,'Better'), as.factor=T)


## ----assignment-4-solution-----------------------------------------------
str(ces)
#Examine variables
ces$personal
ces$federal

## ----plot-finances-------------------------------------------------------
#Store the results of the command in out. Note, this can be added at the end, once you know you have the command correct. 
# Start with the data frame

  #Gather, name the new variables, then select the variables to be gathered
out<-gather(ces, Finances, Value, personal, federal )

#names
names(out)
#

#plot
ggplot(out, aes(x=Value))+geom_bar()+facet_grid(~Finances)

## ----show-subset-select, results='markup'--------------------------------
subset(ces, select=c('gender', 'ideology'))

## ---- results='markup'---------------------------------------------------
#This selects only males
subset(ces, select='gender',gender=='M')

## ---- tidyverse-filter, results='markup',echo=T, include=T---------------
filter(ces, gender=='M')

## ---- tidyverse-select, results='markup'---------------------------------
select(ces, c(gender, ideology))

## ----filter-men-select-ideology------------------------------------------
ces %>% 
  filter(gender=='M') %>% 
  select(gender, ideology) 

## ----plot-male-ideology--------------------------------------------------

#Start with a data frame
ces %>% 
  #Filter out men
  filter(gender=='M') %>% 
  #select the variable you want
  select(ideology) %>% 
  #Do something you want
  ggplot(., aes(x=ideology))+geom_bar()

## ----assignment-5--------------------------------------------------------
#check out
str(out)

#Start with out as a data frame
out %>% 
  #filter those with degrees and 'Men'. Dont' bother storing it. Check `vals` if necessary. 
  filter(degree=='No degree' & gender=='M')

## ----select-example, results='markup'------------------------------------

out<-ces %>% 
  select(gender, degree, p_votechce)
#Check the results 
out

## ----assignment-6-solution-----------------------------------------------
ces$vote<-Recode(ces$p_votechce, "1='Conservative' ; 2='Liberal' ; 3='NDP'; 4='BQ' ; else=NA", levels=c('Conservative', 'Liberal', 'NDP', 'BQ'), as.factor=T)


## ----select-variables, results='markup'----------------------------------
#We will start with `ces`
out<-ces %>% 
  #select gender, degree and vote
  select(gender, degree, vote) %>% 
  #Form groups of gender and degree
  group_by(gender, degree, vote)
#print
out

## ----summarize-degree-gender-vote----------------------------------------
#Call out, which we stored above
out<-out %>% 
  #Summarize groups, provide a name for the new variable
  summarize(number=n()) %>% 
#Filter out missing values
na.omit()

## ----plot-degree-gender-vote---------------------------------------------
out %>% 
  ggplot(., aes(x=degree, y=number, fill=vote))+geom_col(position='dodge')+facet_grid(~gender)

## ----change-colors-------------------------------------------------------

  out %>% 
  ggplot(., aes(x=degree, y=number, fill=vote))+geom_col(position='dodge')+facet_grid(~gender)+scale_fill_manual(values=c('blue', 'red', 'orange', 'cyan'))+labs(title='Vote By Degree Status And Education')


## ----assignment-7-solution-----------------------------------------------
ces %>% 
select(degree, vote, income, inequal_cat) %>% 
  group_by(degree, vote, income, inequal_cat) %>% 
  summarize(n=n()) %>% 
  na.omit() %>% 
  ggplot(., aes(x=income, y=n, fill=inequal_cat))+geom_col(position='dodge')+facet_grid(degree~vote)

## ----percentage-degree-gender-vote, results='markup'---------------------
#Start with ces
ces %>% 
  #Select variables of interest
  select(gender, degree, vote) %>% 
  #group
  group_by(gender, degree, vote) %>% 
  #summarize
  summarise(n=n()) %>% 
  #na.omit
  na.omit() %>% 
 #Here is our mutate command
mutate(pct=n/sum(n))

## ----final-mutate--------------------------------------------------------
#Start with ces
ces %>% 
  #Select variables of interest
  select(gender, degree, vote) %>% 
  #group
  group_by(gender, degree, vote) %>% 
  #summarize
  summarise(n=n()) %>% 
  #na.omit
  na.omit() %>% 
 #Here is our mutate command, pay attention to the brackets
mutate(pct=(n/sum(n))*100) %>% 
  #plot, note the new y-axis-value
  ggplot(., aes(x=degree, y=pct, fill=vote))+geom_col(position='dodge')+facet_grid(~gender)+scale_fill_manual(values = c('blue', 'red', 'orange', 'cyan'))


## ----assignment-8-solution-----------------------------------------------
#Start with the data frame
ces %>% 
  #select variables of interest
select(degree, vote, income, inequal_cat) %>% 
  #form the groups
  group_by(degree, vote, income, inequal_cat) %>% 
  #and count
  summarize(n=n()) %>% 
  #eliminate missing values
  na.omit() %>% 
  #form new groups for mutating
  group_by(degree, vote, income) %>% 
  #mutate
  mutate(pct=(n/sum(n)*100)) %>% 
  #plot
  ggplot(., aes(x=income, y=pct, fill=inequal_cat))+geom_col(position='dodge')+facet_grid(degree~vote)

## ----assignment-9-solution-----------------------------------------------
#Check vals to figure out recode.
vals
#Take the data frame
ces %>% 
  #select variables of interest
select(degree, vote, income, inequal_cat) %>% 
  #mutate income
  mutate(income2=Recode(income, "1='0-$30K' ; 2:4='$30K-$110K' ; 5='$110K+'", levels=c('0-$30K', '$30K-$110K' , '$110K+'), as.factor=T)) %>% 
  #form the groups
  group_by(degree, vote, income2, inequal_cat) %>% 
  #and count
  summarize(n=n()) %>% 
  #eliminate missing values
  na.omit() %>% 
  #form new groups for mutating
  group_by(degree, vote, income2) %>% 
  #mutate
  mutate(pct=(n/sum(n)*100)) %>% 
  #plot
  ggplot(., aes(x=income2, y=pct, fill=inequal_cat))+geom_col(position='dodge')+facet_grid(degree~vote)

## ----assignment-10-solution----------------------------------------------
ces %>% 
  #name the grouping variable
  group_by(gender, degree) %>% 
  #Then summarize each group by calculating its mean
  summarize(
    avg=mean(ideology, na.rm=T)
  ) %>% 
na.omit() %>% 
  ggplot(., aes(x=gender, y=avg, col=degree))+geom_point()

## ----show-standard-errors------------------------------------------------
ces %>% 
  #name the grouping variable
  group_by(gender, degree) %>% 
  na.omit() %>% 
  #Then summarize each group by calculating its mean
  summarize(
    #Calculate the average
    avg=mean(ideology),
    #Calculate the standard deviation
    sd=sd(ideology),
    #calculate the number of respondents in each group with n()
  freq=n(), 
  #Calculate the standard error
  se=sd/sqrt(freq)
  ) 

## ----show-plot-with-errorbars--------------------------------------------
ces %>% 
  #name the grouping variable
  group_by(gender, degree) %>% 
  na.omit() %>% 
  #Then summarize each group by calculating its mean
  summarize(
    #Calculate the average
    avg=mean(ideology),
    #Calculate the standard deviation
    sd=sd(ideology),
    #calculate the number of respondents in each group with n()
  freq=n(), 
  #Calculate the standard error
  se=sd/sqrt(freq)
  ) %>% 
  ggplot(., aes(x=gender, y=avg))+geom_point(aes(col=degree))+geom_linerange(aes(ymin=avg-(1.96*se), ymax=avg+(1.96*se), col=degree))


## ----check-dependent-variable, results='markup'--------------------------
#Check variables 
head(ces$inequal_cat)


## ----reverse-reference category, eval=F----------------------------------
## #Option One
## #Change the order of levels with levels()
## levels(ces$inequal_cat)<-c('Yes', 'No')
## #Show
## head(ces$inequal_cat)
## 
## ## Use relevel() command, #specify the reference level.
## ces$inequal_cat<-relevel(ces$inequal_cat, 'No')
## #Show
## head(ces$inequal_cat)

## ----logistic-regression-example, results='markup'-----------------------
#Show model
logistic1<-glm(inequal_cat ~ degree+gender, data=ces, family='binomial')
#summary
summary(logistic1)


## ----show-gender-income-inequality, results='markup'---------------------
#
ces %>% 
  #select the three variables of interest
  select(gender, degree, inequal_cat) %>% 
  #Note the function count()
  count(gender, degree, inequal_cat) %>% 
  na.omit() %>% 
  group_by(gender, degree) %>% 
  #mutate
  mutate(pct=n/sum(n)) 
names(ces)

## ----assignment-11-solution, results='markup'----------------------------
names(ces)
#Check vote
head(ces$vote)
#
ces$conservative<-Recode(ces$vote, "'Liberal'='Non-Conservative' ; 'NDP'='Non-Conservative' ; 'BQ'='Non-Conservative' ; 'Conservative' ='Conservative'", levels=c('Non-Conservative', 'Conservative'))
#Fit the first model
mod.con<-glm(conservative ~ gender, data=ces, family='binomial')
#UPdate
mod.con2<-update(mod.con, .~.+degree)
mod.con3<-update(mod.con2, .~.+income)
#Show
stargazer(mod.con, mod.con2, mod.con3, type='text')

#Print out 
#PC users will have to modify the path name for their desktop. 
stargazer(mod.con, mod.con2, mod.con3, type='text', out='~/Desktop/conservative_models')

## ----get-logistic-predicted-values---------------------------------------
ggpredict(mod.con3, terms=c('income', 'degree', 'gender')) %>% 
  ggplot(., aes(x=x, y=predicted, col=facet))+facet_grid(~group)+geom_point()+geom_line()


