
# Load the dataset
dolphins=read.csv("dolphins.csv",header = T)
str(dolphins)
head(dolphins)

# Perform the summary statistics
# Categorical variables: create a function that performs the test then call it on the variables
 
mytable=function(x){
  result=table(x)
  return(result)
}
lapply(dolphins[,c(2:5)], mytable)

# Continuos variables 
summary(dolphins[,c(6:22)])

# For bottlenose dolphin cases, the Mann-Whitney U-test was used to
# compare the median toxicant and nutrient concentrations between:
# adult males versus adult females, juveniles versus adults,
# Florida-stranded versus North Carolina-stranded, 
# confirmed/suspected Morbillivirus+ versus uninfected. 

mydifference=function(y){
result=wilcox.test(dolphins$Hg~y,data = dolphins)  
  return(result)
}
lapply(dolphins[,c(2:5)], mydifference)


# Difference in Hg based on Sex
# Since the p-value is greater than 0.05, we fail to reject the null hypothesis.
# We do not have sufficient evidence to say that the amount of Mercury (Hg)
 # in the Male is different from the Female dolphins.



# Difference in Hg based on Age_class
# Since the p-value is less than 0.05, we reject the null hypothesis.
# We conclude that the amount of Mercury (Hg) in the Juvenile is different
# from the adult dolphins.

# Difference in Hg based on Location
# Since the p-value is greater than 0.05, we fail to reject the null hypothesis.
# We do not have sufficient evidence to say that the amount of Mercury (Hg)
# in the Florida-stranded is different from the North Carolina-stranded dolphins 


# Difference in Hg based on Mobilivirus
# Since the p-value is greater than 0.05, we fail to reject the null hypothesis.
# We do not have sufficient evidence to say that the amount of Mercury (Hg)
# in the confirmed/suspected Morbillivirus dolphins is different from the 
# uninfected dolphins. 





# -----------------------------------------------------------------------------------
# MODELLING
# Before we fit a GLM model we handle the missing values first using multiple imputation method

# Check the number of missing
sum(is.na(dolphins)) #all missing values
colnames(dolphins)[colSums(is.na(dolphins)) > 0] #columns with missing values



# Pattern of Missing Data Exploration
# Before moving on to determining the specifics of multiple imputation,
# we should first explore and see the pattern of missing data in our dataset.
p_missing <- unlist(lapply(dolphins, function(x) sum(is.na(x))))/nrow(dolphins)
sort(p_missing[p_missing > 0], decreasing = TRUE)

# Drop the variables that have more than 25% missing values,
#they will interfere with the model

# Select out variables that could cause problems in the imputation process, 
# include the ID also

library(tidyverse)
dolphins <- dolphins %>% 
  dplyr::select(-'ID',-'PCB_1268',-'DEP', -'NPE',-'Triclosan',-'BPA',-'Atrazine')

sum(is.na(dolphins))

# Perform the imputation
# ------------------------------------------------------------------
# We run the mice code with 0 iterations 

library(dplyr)
library(mice)
set.seed(145)
imp <- mice(dolphins, maxit=0)

# Extract predictorMatrix and methods of imputation 
predM = imp$predictorMatrix
meth = imp$method
head(predM)


# With this command, we tell mice to impute the dolphins data, create 5
# datasets, use predM as the predictor matrix and don't print the imputation
# process. If you would like to see the process, set print as TRUE

imp2 <- mice(dolphins, maxit = 5, 
             predictorMatrix = predM, 
             method = meth, print =  FALSE)


# The imputation created 5 datasets with different plausible values
# for missing values. You can look at imputed datasets and values
# with the following commands:
  
# Look at head and tail of imputed values for china_econ variable 
head(imp2$imp$Hg)
tail(imp2$imp$Hg)

# Can also extract the first imputed, complete dataset and look at the first
# rows using the complete function
dolphincomp <- mice::complete(imp2, 1)
head(dolphincomp)


# First, turn the datasets into long format
dolimp_long <- mice::complete(imp2, action="long", include = TRUE)
dim(dolimp_long)

head()

# Convert back to mids type - mice can work with this type
dolimp_long_mids<-as.mids(dolimp_long)
head(dolimp_long_mids)



# Meeting the Assumptions of Logistic regression model
# 1. The dependent variable is binnary (all the demographic groups are binary)

# 2. Observations to be independent of each other
# 3.Linear relation between dependent and indpendent variable
# 4. Large dataset of more than 30 rows



#GLM model
# Demographic group: Morbillivirus
fitimp <- with(dolimp_long_mids, glm(Morbillivirus~Hg+Cu+Se+Cd+Tl+Pb+Mn+Co+Zn+As+Fe,family = 'binomial'))
sum_model=summary(pool(fitimp))
sum_model

# Demographic group: Sex
fitimp2 <- with(dolimp_long_mids, glm(Sex~Hg+Cu+Se+Cd+Tl+Pb+Mn+Co+Zn+As+Fe,family = 'binomial'))
sum_model2=summary(pool(fitimp2))
sum_model2

# Demographic group: Morbillivirus
fitimp3 <- with(dolimp_long_mids, glm(Age_Class~Hg+Cu+Se+Cd+Tl+Pb+Mn+Co+Zn+As+Fe,family = 'binomial'))
sum_model3=summary(pool(fitimp3))
sum_model3

# Demographic group: Location
fitimp4 <- with(dolimp_long_mids, glm(Location~Hg+Cu+Se+Cd+Tl+Pb+Mn+Co+Zn+As+Fe,family = 'binomial'))
sum_model4=summary(pool(fitimp4))
sum_model4


# Regression equation coefficients
# -----------------------------------------
# Create a dataframe of the odds ratios
odds_ratios=exp(sum_model$estimate) #convert the log odds to odds ratios
df_odd=as.data.frame(odds_ratios) #create the dataframe
df_odd$independent_variable=sum_model$term #add the names of the independent variables
df

df_odd


odds_ratios2=exp(sum_model2$estimate) #convert the log odds to odds ratios
df_odd2=as.data.frame(odds_ratios2) #create the dataframe
df_odd2$independent_variable=sum_model2$term #add the names of the independent variables
df_odd2


odds_ratios3=exp(sum_model3$estimate) #convert the log odds to odds ratios
df_odd3=as.data.frame(odds_ratios3) #create the dataframe
df_odd3$independent_variable=sum_model3$term #add the names of the independent variables
df_odd3


odds_ratios4=exp(sum_model4$estimate) #convert the log odds to odds ratios
df_odd4=as.data.frame(odds_ratios4) #create the dataframe
df_odd4$independent_variable=sum_model4$term #add the names of the independent variables
df_odd4




# Comparison of toxin concentration in each demographic groups
### group mtcars by cylinders and return some averages

Doll_loc <- dolphincomp %>%
  select(Location,Hg,Cu,Se,Cd,Tl,Pb,Mn,Co,Zn,As) %>%
  group_by(Location) %>%
  summarise(Hg = mean(Hg), Cu = mean(Cu),Se = mean(Se),
            Cd = mean(Cd), Tl = mean(Tl),Pb = mean(Pb),
            Mn = mean(Mn), Co = mean(Co),Zn = mean(Zn),
            As = mean(As))

Doll_loc


Doll_sex <- dolphincomp %>%
  select(Sex,Hg,Cu,Se,Cd,Tl,Pb,Mn,Co,Zn,As) %>%
  group_by(Sex) %>%
  summarise(Hg = mean(Hg), Cu = mean(Cu),Se = mean(Se),
            Cd = mean(Cd), Tl = mean(Tl),Pb = mean(Pb),
            Mn = mean(Mn), Co = mean(Co),Zn = mean(Zn),
            As = mean(As))

Doll_sex



Doll_age <- dolphincomp %>%
  select(Age_Class,Hg,Cu,Se,Cd,Tl,Pb,Mn,Co,Zn,As) %>%
  group_by(Age_Class) %>%
  summarise(Hg = mean(Hg), Cu = mean(Cu),Se = mean(Se),
            Cd = mean(Cd), Tl = mean(Tl),Pb = mean(Pb),
            Mn = mean(Mn), Co = mean(Co),Zn = mean(Zn),
            As = mean(As))
Doll_age


Doll_morbillivirus <- dolphincomp %>%
  select(Morbillivirus,Hg,Cu,Se,Cd,Tl,Pb,Mn,Co,Zn,As) %>%
  group_by(Morbillivirus) %>%
  summarise(Hg = mean(Hg), Cu = mean(Cu),Se = mean(Se),
            Cd = mean(Cd), Tl = mean(Tl),Pb = mean(Pb),
            Mn = mean(Mn), Co = mean(Co),Zn = mean(Zn),
            As = mean(As))
Doll_morbillivirus

