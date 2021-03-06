library(tidyverse)
library(rio)
ces15<-import('~/OneDrive - Wilfrid Laurier University/canadian_politics/canadian_election_studies/CES15/CES15_CPS+PES_Web_SSI Full.dta')
library(questionr)
lookfor(ces15, 'income')
lookfor(ces15, 'left')
lookfor(ces15, 'education')
lookfor(ces15, 'vote')
lookfor(ces15, 'year')
lookfor(ces15, 'financial')
lookfor(ces15, 'rich')
lookfor(ces15, 'sex')

out<-ces15 %>% 
  select(p_incineq, income_full, p_selfplace, education, p_votechce, pers_ret, pers_fdpol, p_gap, sex_r) 

export(out, file='data/ces15_data.sav', format='sav')

