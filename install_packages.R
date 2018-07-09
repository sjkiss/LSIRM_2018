#Necessary libraries

to.install<-c('pastecs', 'haven', 'labelled', 'car', 'psych', 'ggplot2', 'dplyr', 'tidyr', 'Hmisc', 'devtools', 'xtable', 'effects', 'stargazer')

#Packages that have not been installed
new.packages <- to.install[!(to.install %in% installed.packages()[,"Package"])]
#If there are packages that must be installed, this installs them
if(length(new.packages)) install.packages(new.packages, repos='http://cran.utstat.utoronto.ca/')
#This loads packages
loaded.packages<-lapply(to.install, require, character.only=T)
loaded.packages
#Note: after running this command, TRUE should be printed out 13 times on the console. If not, please contact Simon Kiss at skiss@wlu.ca
#One more check
print('Hello World')
#One last check
plot(seq(1,10,1), seq(1,10,1), main='If you can read this, you are set up.')
