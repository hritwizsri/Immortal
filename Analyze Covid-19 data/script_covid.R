rm(list=ls()) #remove all variables stored previously 
library(Hmisc)
  data <- read.csv("C:/Users/hritw/OneDrive/Desktop/Covid_R/COVID19_line_list_data.csv")
  describe(data) 
  # cleaned up death data column
  data$death_dummy<- as.integer(data$death!=0)
  #death rate
  sum(data$death_dummy)/nrow(data)
  #Age
  dead= subset(data,death_dummy==1)
  alive=subset(data,death_dummy!=1)
  mean(dead$age,na.rm=TRUE)
  mean(alive$age,na.rm=TRUE)
  # is this is statically significant?
  t.test(alive$age,dead$age,alternative = "two.sided", conf.level = 0.99)
   
  # normally is the p-value is <0.05,we reject the null hypothesis
  # p-value is 0 which is < 0.05 so it statically correct but not bychance 
  # that no. of people olader age died more in caparison of younge people

  
  #-----GENDER----
  man=subset(data,gender=="male")
  woman=subset(data,gender=="female")
  mean(man$death_dummy,na.rm="TRUE")
  mean(woman$death_dummy,na.rm="TRUE")
  # is this is statically significant?
  t.test(man$death_dummy,woman$death_dummy,alternative = "two.sided",conf.level = 0.95)
  # men have higher death rate than women