
#Perform the below operations:
#  a. Pre-process the passenger names to come up with a list of
#titles that represent families and represent using appropriate
#visualization graph.

#Importing the titanic dataset into R
library(xlsx)
titanicdf<-read.xlsx("titanic3.xls",1)
#Converting the name column to char
titanicdf$name<-as.character(titanicdf$name)

#Extracting the family name (first name) and adding it to a new column family
for (i in seq (1:1309)) {
  titanicdf$family[i]<- strsplit(titanicdf$name, ",")[[i]][1]
  }
View(titanicdf$family)

#Count the frequency of common names in family column.
 library(plyr)
family_frequency<- count(titanicdf,"family")
barplot(family_frequency$freq)

#b. Represent the proportion of people survived by family size
#using a graph.
library(sqldf)

survived<- sqldf("SELECT * 
      FROM titanicdf 
      WHERE survived = '1'") 
survived_family_freq <- count(survived$family)
barplot(survived_family_freq$freq)

#c. Impute the missing values in Age variable using Mice library,
#create two different graphs showing Age distribution before
#and after imputation
library(mice)

#Removing columns 1,3,8,9,10,12,13,14,15

mini_titanic <- titanicdf[-c(1,3,8,9,10,12,13,14,15)]

md.pattern(mini_titanic)

library(dplyr) 
mini_titanic <- mini_titanic %>%
  mutate(
    survived = as.factor(survived),
    sex = as.factor(sex),
    age = as.numeric(age),
    sibsp = as.factor(sibsp),
    parch = as.factor(parch),
    embarked = as.factor(embarked)
  )
str(mini_titanic)


#running the mice function
temp_mini_titanic <- mice(mini_titanic,m=5,maxit=50,seed=500)
summary(temp_mini_titanic)

#check for imputed data in a field
temp_mini_titanic$imp$age

#Now we can get back the completed dataset using the complete() function.
completed_mini_titanic <- complete(temp_mini_titanic,1)
md.pattern(completed_mini_titanic)

#Inspecting the distribution of original and imputed data
#before imputation - Age 

#The density of the imputed data for each imputed dataset 
#is showed in magenta while the density of the observed data is showed in blue
#after imputation of age
densityplot(temp_mini_titanic)
#before imputation of age
densityplot(mini_titanic$age)


