## ----setup, echo=F, results='hide'------------------------------
knitr::opts_chunk$set(echo = TRUE, message=F, warning=F, fig.align='center', results='markup')

## ----print-example----------------------------------------------
print('Hello world')

## ----save-objects-----------------------------------------------
abc<-print('Hello world')


## ----print-saved-objects----------------------------------------
abc

## ----install-tidyverse, eval=F----------------------------------
## install.packages('tidyverse')

## ----load-tidyverse---------------------------------------------
library(tidyverse)

## ----numeric----------------------------------------------------
num<-1


## ----character--------------------------------------------------
char<-'1'

## ----num-char---------------------------------------------------
num
char

## ----check-class------------------------------------------------
class(num)
class(char)

## ----make-vectors-----------------------------------------------
gender<-c(0,1,0)
age<-c(18,22,33)
lucky<-c(1,2,3)

## ----assignment-1-solution, results='hide'----------------------
matrix1<-matrix(c(age, gender, lucky), nrow=3, ncol=3)


## ----assignment-2-solution, results='hide'----------------------
#The number in the square brackets indicates which element of the vector gender to return 
age[1]
gender[1]

## ----assignment-3-solution, eval=F------------------------------
## matrix1[1,]
## matrix1[2,]
## matrix1[,1]

## ----assignment-4-solution--------------------------------------
#First through third
matrix1[1:3,]
#first and third
matrix1[c(1,3),]


## ----assignment-5-solution--------------------------------------
df<-data.frame(age=age, gender=gender, lucky=lucky)

## ----access-individual-variables--------------------------------
df$age
df$gender

## ----summary-commands-------------------------------------------
#summarize the first few rows of a data frame
head(df)
#Examine the structure of a data frame
str(df)
#Summary statistics of a data frame.
summary(df)
#Access one variable of a data frame
df$age

## ----individual-variables---------------------------------------
#Check class of each variable
class(df$age)
class(df$gender)
class(df$lucky)

## ----load-car---------------------------------------------------
library(car)

## ----Recode-gender----------------------------------------------
Recode(df$gender, "0='male' ; 1='female'", as.factor=TRUE)

## ----Recode-gender-2--------------------------------------------
df$gender2<-Recode(df$gender, 
                   "0='male' ; 1='female'", 
                   as.factor=TRUE)

## ----check-levels-gender-2--------------------------------------
levels(df$gender2)


## ----make-lists-------------------------------------------------
list1<-list(age, gender,lucky)


## ----change-classes---------------------------------------------
#print the numeric variable num as a character
as.character(num)
#print the character variable char as a number
as.numeric(char)
#Convert both to a factor (categorical variable)
as.factor(num)
as.factor(char)

## ----maths------------------------------------------------------
#Addition
x<-1+2
x
y<-2+1
#Division
x/y
#Multiplication
x*y
#Exponentiation
x^2
#Square Root
sqrt(9)

#Use brackets as in regular math
(x+1)/y

## ----calculate-mean---------------------------------------------
#Calculate mean
mean(age)
#Calculate median
median(age)
#get the maximum values
max(age)

## ----function-examples, eval=F----------------------------------
## #Note: the capitalization patterns for functions.
## #Words are not separated with periods or underscores, but with capitalized names.
## #The arguments that each function takes are specified in parentheses.
## functionName<-function(x,y,z){ #The actual commands are opened with a curly brace command
##   #The function commands are entered here
## }# The function is closed here
## 

## ----assignment-6-solution--------------------------------------
#Defie the function
myfunction<-function(i) {
  #square i
  i<-(i^2)/3
  print(i)  
}
#Print the number 7
myfunction(7)


