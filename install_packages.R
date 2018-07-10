#Necessary libraries

to.install<-c('car', 'haven', 'labelled', 'foreign',  'psych', 'tidyverse', 'ggeffects', 'xtable', 'stargazer', 'questionr', 'rio', 'devtools', 'effects', 'scales', 'cowplot')

#Packages that have not been installed
new.packages <- to.install[!(to.install %in% installed.packages()[,"Package"])]
#If there are packages that must be installed, this installs them
if(length(new.packages)) install.packages(new.packages, repos='http://cran.utstat.utoronto.ca/')

#Save installed package names
out<-to.install[to.install %in% installed.packages()[,'Package']]



#You should see 'Hello World' after this. 
#print('Hello World')

#print out
cat(out)

print(out)

out

#One last check
plot(seq(1,10,1), seq(1,10,1), main='If you can read this, you are set up.')
