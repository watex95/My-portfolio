
# 
# # FINANCIAL AID DATA
# financial_aid<-read.csv("financial_aid.csv",header = T)
# 
# #Combine by summing the data over the years
# library(tidyverse)
# attach(financial_aid)
# financial_aid=financial_aid %>%
#   mutate(Total_loan = select(.,c(X2012.Loan,X2013.Loan,X2014.Loan,
#            X2015.Loan,X2016.Loan,X2017.Loan)) %>%
#            rowSums(na.rm = TRUE))
# 
# financial_aid<-financial_aid%>%
#   mutate(Total_grant=select(.,c(X2012.Grant,X2013.Grant,X2014.Grant,
#           X2015.Grant,X2016.Grant,X2017.Grant))%>%
#            rowSums(na.rm = TRUE))
# 
# financial_aid<-financial_aid%>%
#   mutate(Total_scholarship=select(.,c(X2012.Scholarship,X2013.Scholarship,X2014.Scholarship,
#             X2015.Scholarship,X2016.Scholarship,X2017.Scholarship))%>%
#            rowSums(na.rm = TRUE))
# 
# financial_aid<-financial_aid%>%
#   mutate(Total_WorkStudy=select(.,c(X2012.Work.Study,X2013.Work.Study,
#     X2014.Work.Study,X2015.Work.Study,X2016.Work.Study,X2017.Work.Study))%>%
#            rowSums(na.rm = TRUE))
# colnames(financial_aid)
# 
# financial_aid<-financial_aid[,-c(9:32)]
# colnames(financial_aid)
# 
# 
# # Load the train labels and the TestIDs
# train_labels=read.csv("DropoutTrainLabels.csv",header = T)
# testIDs=read.csv("TestIDs.csv",header = T)
# 
# 
# # JOIN the train labels and the financial aid data
# library(dplyr)
# financial_aid_train=left_join(train_labels,financial_aid,by="StudentID")
# colnames(financial_aid_train)
# # JOIN the test IDs and the financial aid data
# financial_aid_test=left_join(testIDs,financial_aid,by="StudentID")
# colnames(financial_aid_test)
# 
# 
# #check for duplicated StudentID (TRUE=No duplicates, FALSE=duplicates present)
# length(unique(financial_aid_train$StudentID)) == nrow(financial_aid_train)
# 
# 
# # STATIC DATA
# #STATIC data merged through ETL 
# #load all files and merge them
# myETL=function(mypath){
#   filenames = list.files(path=mypath, full.names=TRUE)
#   file_load = function(x){read.csv(file=x,header=T)}
#   datalist = lapply(filenames, file_load)
#   data2 = do.call(rbind, lapply(datalist, as.data.frame))
#   return(data2)
# }
# # call the function
# mergedStatic<-myETL("D:/KAZI/UPWORK/KAGGLE COMPETITION/Student Retention Challenge Data/Student Static Data")
# 
# #check for duplicated StudentID (TRUE=No duplicates, FALSE=duplicates present)
# length(unique(mergedStatic$StudentID)) == nrow(mergedStatic)
# 
# # JOIN the train labels and the static data
# library(dplyr)
# static_train=left_join(train_labels,mergedStatic,by="StudentID")
# 
# # JOIN the test IDs and the static data
# static_test=left_join(testIDs,mergedStatic,by="StudentID")
# 
# 
# 
# 
# # PROGRESS DATA
# #Progress data merged through ETL 
# #load all files and merge them
# myETL=function(mypath){
# filenames = list.files(path=mypath, full.names=TRUE)
# file_load = function(x){read.csv(file=x,header=T)}
# datalist = lapply(filenames, file_load)
# data2 = do.call(rbind, lapply(datalist, as.data.frame))
# return(data2)
# }
# 
# # call the function
# mergedProgress<-myETL("D:/KAZI/UPWORK/KAGGLE COMPETITION/Student Retention Challenge Data/Student Progress Data")
# 
# colnames(mergedProgress)
# 
# #check for duplicated StudentID (TRUE=No duplicates, FALSE=duplicates present)
# length(unique(mergedProgress$StudentID)) == nrow(mergedProgress)
# 
# library(plyr)
# # summarise the data to get rid of duplicated StudentID
# prog1=ddply(mergedProgress,.(StudentID),summarize,
#             CompleteDevMath=mean(CompleteDevMath),CompleteDevEnglish=mean(CompleteDevEnglish),
#             Major1=mean(Major1),Major2=mean(Major2),Complete1=mean(Complete1),
#             Complete2=mean(Complete2),CompleteCIP1=mean(CompleteCIP1),
#             CompleteCIP2=mean(CompleteCIP2),TransferIntent=mean(TransferIntent),
#             DegreeTypeSought=mean(DegreeTypeSought),TermGPA=mean(CumGPA),
#             CumGPA=mean(CumGPA),number=length(StudentID))
# dim(prog1)
# prog1<-prog1[,-14]
# dim(prog1)
# colnames(prog1)
# 
# # JOIN the train labels and the progress data
# library(dplyr)
# progress_train=left_join(train_labels,prog1,by="StudentID")
# # JOIN the test IDs and the progress data
# progress_test=left_join(testIDs,prog1,by="StudentID")
# 
# dim(progress_train)
# 
# # Check to ensure the datasets are of the same sizes before merging them
# dim(financial_aid_train)
# dim(financial_aid_test)
# 
# dim(static_train)
# dim(static_test)
# 
# dim(progress_train)
# dim(progress_test)
# 
# 
# 
# 
# # TRAINING DATASET
# library(dplyr)
# join1<-inner_join(financial_aid_train,static_train,by="StudentID")
# TRAIN_DATA=inner_join(join1,progress_train,by="StudentID")
# dim(TRAIN_DATA)
# colnames(TRAIN_DATA)
# 
# # TESTING DATASET
# join2<-inner_join(financial_aid_test,static_test,by="StudentID")
# TEST_DATA<-inner_join(join2,progress_test,by="StudentID")
# dim(TEST_DATA)
# colnames(TEST_DATA)
# 
# 
# # Clean the train data by removing extra dropout variables (Dropout.y,Dropout)
# TRAIN_DATA<-select(TRAIN_DATA,-c("Dropout.y"))
# TRAIN_DATA<-select(TRAIN_DATA,-c("Dropout"))
# 
# # Rename the droput.x to just dropout for consistency
# library(dplyr)
# colnames(TRAIN_DATA)[colnames(TRAIN_DATA)=="Dropout.x"] <- "Dropout"
# colnames(TRAIN_DATA)
# 
# dim(TRAIN_DATA)
# dim(TEST_DATA)
# 
# 
# # convert empty spaces and (-1) to NULL in order to facilitate imputation later on
# str(TRAIN_DATA)
# TRAIN_DATA[TRAIN_DATA=="-1"]<-NA
# TRAIN_DATA[TRAIN_DATA==""]<-NA
# 
# # convert the  categorical variables with integer format to  factors 
# colnames(TRAIN_DATA)
# 
# names=c('DegreeTypeSought','TransferIntent','GatewayEnglishStatus',
#         'GatewayMathStatus','EngPlacement','MathPlacement','HighDeg',
#         'EnrollmentStatus','HSDip','Gender','Campus','CohortTerm',
#         'cohort_term','Dropout','CompleteDevEnglish','CompleteDevMath','Zip',
#         'BirthMonth'
#         )
# 
# 
# TRAIN_DATA[,names]<-lapply(TRAIN_DATA[,names] , factor)
# str(TRAIN_DATA)
# colnames(TRAIN_DATA)
# 
# summary(TRAIN_DATA)
# 
# # DROP EMPTY COLUMNS
# drop.cols <- c('HSGPAWtd', 'FirstGen','TransferIntent','Campus',
#                'Address2','Major2')
# TRAIN_DATA<-TRAIN_DATA %>% select(-drop.cols)
# dim(TRAIN_DATA)
# 
# TEST_DATA<-TEST_DATA %>% select(-drop.cols)
# dim(TEST_DATA)
# 
# 
# 
# 
# # EXPLORATORY DATA ANALYSIS
# # Descriptive statistics
# 
# library(dplyr)
# TRAIN_DATA %>% group_by(Dropout) %>% summarise(Mean_loan = mean(Total_loan))
# ggplot(data = TRAIN_DATA, mapping = aes(x =Dropout,y=Total_loan))+
#   geom_boxplot()
# 
# TRAIN_DATA %>% group_by(Dropout) %>% summarise(Mean_grant = mean(Total_grant))
# ggplot(data = TRAIN_DATA, mapping = aes(x =Dropout,y=Total_grant))+
#   geom_boxplot()
# 
# ggplot(data = TRAIN_DATA, mapping = aes(x =Dropout,y=Total_scholarship))+
#   geom_boxplot()
# 
# ggplot(data = TRAIN_DATA, mapping = aes(x =Dropout,y=Total_WorkStudy))+
#   geom_boxplot()
# 
# 
# # # correlation matrix of the financial aid data
# # Load the GGally package
# library(GGally)
# # Create a scatter plot matrix
# vars <- c("Total_loan", "Total_grant", "Total_WorkStudy", "Total_scholarship")
# ggpairs(TRAIN_DATA[vars])
# 
# 
# # # correlation matrix of the student performance
# # Load the GGally package
# library(GGally)
# # # # Create a scatter plot matrix
# vars <- c("Adjusted.Gross.Income","Parent.Adjusted.Gross.Income","CumLoanAtEntry",
#           "TermGPA")
# ggpairs(TRAIN_DATA[vars])
#  
#  
# 
# 
# table(TRAIN_DATA$Marital.Status,TRAIN_DATA$Dropout,exclude = NULL)
# ggplot(TRAIN_DATA,aes(x = Marital.Status)) +
#   geom_bar(aes(fill=TRAIN_DATA$Dropout))+
#   xlab("Marital.Status") +  
#   ggtitle("Marital.Status in relation to dropout") +
#   labs(fill="Dropout")
# 
# 
# attach(TRAIN_DATA)
# TRAIN_DATA$Dropout=as.factor(as.character(TRAIN_DATA$Dropout))
# 
# ggplot(TRAIN_DATA,aes(x = Gender)) +
#   geom_bar(aes(fill=TRAIN_DATA$Dropout))+
#   xlab("Gender") +  
#   ggtitle("Gender in relation to dropout") +
#   labs(fill="Dropout")
# 
# str(TRAIN_DATA)
# class(Gender)
# 
# 
# 
# # # Hypotheses tests 
# # # ----------------------------
# # # Test for association
# # # The gender of a student is a factor in rate of dropout
# # attach(TRAIN_DATA)
# # chisq.test(Dropout,Gender)
# # 
# # # The dropout of students is not related to Marital.Status of a student 
# # chisq.test(Marital.Status,Dropout)
# # 
# # # The dropout of students is not related to the housing of a student 
# # chisq.test(Housing,Dropout)
# # 
# # #The dropout of students is not related to the Father.s.Highest.Grade.Level 
# # chisq.test(Father.s.Highest.Grade.Level,Dropout)
# # 
# # # The dropout of students is not related to the Mother.s.Highest.Grade.Level 
# # chisq.test(Mother.s.Highest.Grade.Level,Dropout)
# # 
# # # The dropout of students is not related to GatewayMathStatus of the student
# # chisq.test(GatewayEnglishStatus,Dropout)
# # 
# # 
# # 
# # # which columns have missing data
# # colnames(TRAIN_DATA)[colSums(is.na(TRAIN_DATA)) > 0]
# 
# 
# # IMPUTATION OF MISSING values
# # ----------------------------------
# # Which columns have missing data
# colnames(TRAIN_DATA)[colSums(is.na(TRAIN_DATA)) > 0]
# colnames(TEST_DATA)[colSums(is.na(TEST_DATA)) > 0]
# 
# 
# # # CONTINUOUS VARIABLES
# # # First check for normal distribution in order to impute with mean or median
# library("ggpubr")
# ggdensity(TRAIN_DATA$Adjusted.Gross.Income,
#           main = "Density plot of tooth length",
#           xlab = "Tooth length")
# hist(Adjusted.Gross.Income) #histogram
# library(ggpubr)
# ggqqplot(Adjusted.Gross.Income) #qqplot
# #
# #
# library("ggpubr")
# ggdensity(TRAIN_DATA$Adjusted.Gross.Income,
#           main = "Density plot of tooth length",
#           xlab = "Tooth length")
# hist(Adjusted.Gross.Income) #histogram
# library(ggpubr)
# ggqqplot(Adjusted.Gross.Income) #qqplot
# #
# # # Parent.Adjusted.Gross.Income
# ggdensity(TRAIN_DATA$Parent.Adjusted.Gross.Income,
#           main = "Density plot of tooth length",
#           xlab = "Tooth length")
# hist(Parent.Adjusted.Gross.Income) #histogram
# library(ggpubr)
# ggqqplot(Parent.Adjusted.Gross.Income) #qqplot
# #
# # # HSGPAUnwtd
# # ggdensity(TRAIN_DATA$HSGPAUnwtd,
# #           main = "Density plot of tooth length",
# #           xlab = "Tooth length")
# # hist(HSGPAUnwtd) #histogram
# # library(ggpubr)
# # ggqqplot(HSGPAUnwtd) #qqplot
# #
# # # HSGPAWtd
# # ggdensity(TRAIN_DATA$HSGPAWtd,
# #           main = "Density plot of tooth length",
# #           xlab = "Tooth length")
# # hist(HSGPAWtd) #histogram
# # library(ggpubr)
# # ggqqplot(HSGPAWtd) #qqplot
# 
# 
# 
# library(ggpubr)
# 
# ggdensity(TRAIN_DATA$TermGPA, 
#           main = "Density plot of tooth length",
#           xlab = "Tooth length")
# # Histogram
# hist(TRAIN_DATA$TermGPA) #histogram
# 
# # QQplot
# ggqqplot(TRAIN_DATA$TermGPA) #qqplot
# 
# 
# 
# ##### Imputation of multiple rows (i.e. the whole data frame) #####
# # using median
# 
# 
# 
# 
# 
# 
# for(i in 1:nrow(TRAIN_DATA)) {
#   TRAIN_DATA[i, ][is.na(TRAIN_DATA[i,])]<-median(as.numeric(TRAIN_DATA[i,]), na.rm = TRUE)
# }
# 
# colnames(TRAIN_DATA)[colSums(is.na(TRAIN_DATA))>0]
# 
# 
# 
# 
# 
# 
# # IMPUTE CATEGORICAL VARAIBLES USNIG  MODE
# 
# val<-unique(TRAIN_DATA$Marital.Status[!is.na(TRAIN_DATA$Marital.Status)]) # Values in vec_miss
# mode <- val[which.max(tabulate(match(TRAIN_DATA$Marital.Status, val)))] # Mode of vec_miss
# TRAIN_DATA$Marital.Status[is.na(TRAIN_DATA$Marital.Status)]<-mode # Impute by mode
# 
# val<-unique(TRAIN_DATA$Father.s.Highest.Grade.Level[!is.na(TRAIN_DATA$Father.s.Highest.Grade.Level)])
# mode<-val[which.max(tabulate(match(TRAIN_DATA$Father.s.Highest.Grade.Level, val)))] 
# TRAIN_DATA$Father.s.Highest.Grade.Level[is.na(TRAIN_DATA$Father.s.Highest.Grade.Level)]<-mode 
# 
# val<-unique(TRAIN_DATA$Mother.s.Highest.Grade.Level[!is.na(TRAIN_DATA$Mother.s.Highest.Grade.Level)])
# mode<-val[which.max(tabulate(match(TRAIN_DATA$Mother.s.Highest.Grade.Level, val)))] 
# TRAIN_DATA$Mother.s.Highest.Grade.Level[is.na(TRAIN_DATA$Mother.s.Highest.Grade.Level)]<-mode 
# 
# val<-unique(TRAIN_DATA$Housing[!is.na(TRAIN_DATA$Housing)])
# mode<-val[which.max(tabulate(match(TRAIN_DATA$Housing, val)))] 
# TRAIN_DATA$Housing[is.na(TRAIN_DATA$Housing)]<-mode
# 
# 
# val<-unique(TRAIN_DATA$EngPlacement[!is.na(TRAIN_DATA$EngPlacement)])
# mode<-val[which.max(tabulate(match(TRAIN_DATA$EngPlacement, val)))] 
# TRAIN_DATA$EngPlacement[is.na(TRAIN_DATA$EngPlacement)]<-mode
# 
# val<-unique(TRAIN_DATA$MathPlacement[!is.na(TRAIN_DATA$MathPlacement)])
# mode<-val[which.max(tabulate(match(TRAIN_DATA$MathPlacement, val)))] 
# TRAIN_DATA$MathPlacement[is.na(TRAIN_DATA$MathPlacement)]<-mode
# 
# val<-unique(TRAIN_DATA$CompleteDevEnglish[!is.na(TRAIN_DATA$MathPlacement)])
# mode<-val[which.max(tabulate(match(TRAIN_DATA$CompleteDevEnglish, val)))] 
# TRAIN_DATA$CompleteDevEnglish[is.na(TRAIN_DATA$CompleteDevEnglish)]<-mode
# 
# val<-unique(TRAIN_DATA$CompleteDevMath[!is.na(TRAIN_DATA$CompleteDevMath)])
# mode<-val[which.max(tabulate(match(TRAIN_DATA$CompleteDevMath, val)))] 
# TRAIN_DATA$CompleteDevMath[is.na(TRAIN_DATA$CompleteDevMath)]<-mode
# 
# val<-unique(TRAIN_DATA$Address1[!is.na(TRAIN_DATA$Address1)])
# mode<-val[which.max(tabulate(match(TRAIN_DATA$Address1, val)))] 
# TRAIN_DATA$Address1[is.na(TRAIN_DATA$Address1)]<-mode 
# 
# val<-unique(TRAIN_DATA$State[!is.na(TRAIN_DATA$State)])
# mode<-val[which.max(tabulate(match(TRAIN_DATA$State, val)))] 
# TRAIN_DATA$State[is.na(TRAIN_DATA$State)]<-mode 
# 
# val<-unique(TRAIN_DATA$City[!is.na(TRAIN_DATA$City)])
# mode<-val[which.max(tabulate(match(TRAIN_DATA$City, val)))] 
# TRAIN_DATA$City[is.na(TRAIN_DATA$City)]<-mode 
# 
# val<-unique(TRAIN_DATA$Zip[!is.na(TRAIN_DATA$Zip)])
# mode<-val[which.max(tabulate(match(TRAIN_DATA$Zip, val)))] 
# TRAIN_DATA$Zip[is.na(TRAIN_DATA$Zip)]<-mode 
# 
# val<-unique(TRAIN_DATA$BirthYear[!is.na(TRAIN_DATA$BirthYear)])
# mode<-val[which.max(tabulate(match(TRAIN_DATA$BirthYear, val)))] 
# TRAIN_DATA$BirthYear[is.na(TRAIN_DATA$BirthYear)]<-mode 
# 
# 
# val<-unique(TRAIN_DATA$Hispanic[!is.na(TRAIN_DATA$Hispanic)])
# mode<-val[which.max(tabulate(match(TRAIN_DATA$Hispanic, val)))] 
# TRAIN_DATA$Hispanic[is.na(TRAIN_DATA$Hispanic)]<-mode 
# 
# val<-unique(TRAIN_DATA$AmericanIndian[!is.na(TRAIN_DATA$AmericanIndian)])
# mode<-val[which.max(tabulate(match(TRAIN_DATA$AmericanIndian, val)))] 
# TRAIN_DATA$AmericanIndian[is.na(TRAIN_DATA$AmericanIndian)]<-mode 
# 
# val<-unique(TRAIN_DATA$Asian[!is.na(TRAIN_DATA$Asian)])
# mode<-val[which.max(tabulate(match(TRAIN_DATA$Asian, val)))] 
# TRAIN_DATA$Asian[is.na(TRAIN_DATA$Asian)]<-mode 
# 
# val<-unique(TRAIN_DATA$Black[!is.na(TRAIN_DATA$Black)])
# mode<-val[which.max(tabulate(match(TRAIN_DATA$Black, val)))] 
# TRAIN_DATA$Black[is.na(TRAIN_DATA$Black)]<-mode 
# 
# val<-unique(TRAIN_DATA$NativeHawaiian[!is.na(TRAIN_DATA$NativeHawaiian)])
# mode<-val[which.max(tabulate(match(TRAIN_DATA$NativeHawaiian, val)))] 
# TRAIN_DATA$NativeHawaiian[is.na(TRAIN_DATA$NativeHawaiian)]<-mode 
# 
# val<-unique(TRAIN_DATA$White[!is.na(TRAIN_DATA$White)])
# mode<-val[which.max(tabulate(match(TRAIN_DATA$White, val)))] 
# TRAIN_DATA$White[is.na(TRAIN_DATA$White)]<-mode 
# 
# val<-unique(TRAIN_DATA$TwoOrMoreRace[!is.na(TRAIN_DATA$TwoOrMoreRace)])
# mode<-val[which.max(tabulate(match(TRAIN_DATA$TwoOrMoreRace, val)))] 
# TRAIN_DATA$TwoOrMoreRace[is.na(TRAIN_DATA$TwoOrMoreRace)]<-mode 
# 
# val<-unique(TRAIN_DATA$HSDip[!is.na(TRAIN_DATA$HSDip)])
# mode<-val[which.max(tabulate(match(TRAIN_DATA$HSDip, val)))] 
# TRAIN_DATA$HSDip[is.na(TRAIN_DATA$HSDip)]<-mode 
# 
# val<-unique(TRAIN_DATA$Zip[!is.na(TRAIN_DATA$Zip)])
# mode<-val[which.max(tabulate(match(TRAIN_DATA$Zip, val)))] 
# TRAIN_DATA$Zip[is.na(TRAIN_DATA$Zip)]<-mode 
# 
# # which columns have missing data
# colnames(TRAIN_DATA)[colSums(is.na(TRAIN_DATA)) > 0]
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# # TEST DATA IMPUTATION
# # -----------------------------
# 
# # Load the dataset
# TEST_DATA=read.csv("TEST.csv",header = T)
# 
# # Convert empty spaces and (-1) to NULL in order to facilitate imputation later on
# TEST_DATA[TEST_DATA=="-1"]<-NA
# TEST_DATA[TEST_DATA==""]<-NA
# 
# # Check for missing columns
# colnames(TEST_DATA)[colSums(is.na(TEST_DATA)) > 0]
# 
# 
# # Impute categorical variables using mode
# val<-unique(TEST_DATA$Zip[!is.na(TEST_DATA$Zip)])
# mode<-val[which.max(tabulate(match(TEST_DATA$Zip, val)))] 
# TEST_DATA$Zip[is.na(TEST_DATA$Zip)]<-mode 
# 
# 
# 
# # Impute using median the other numeric variables
# for(i in 1:nrow(TEST_DATA)) {
#   TEST_DATA[i, ][is.na(TEST_DATA[i,])]<-median(as.numeric(TEST_DATA[i,]), na.rm = TRUE)
# }
# 
# colnames(TEST_DATA)[colSums(is.na(TEST_DATA)) > 0]
# 
# TEST_Imputed=TEST_DATA
# 
# # Save the imputed test data in a csv file
# write.csv(TEST_Imputed,"D:/KAZI/UPWORK/KAGGLE COMPETITION/TEST_Imputed.csv")
# 
# # -------------------------------------------------------------------------------------------------------------------
# 
# 
# 
# 
# 
# # FEATURE ENGINEERING
# 
# # Load the TRAIN_Imputed dataset
# 
# TRAIN_Imputed=read.csv("TRAIN_Imputed.csv",header=T)
# 
# TEST_Imputed=read.csv("TEST_Imputed.csv",header = T)
# 
# 
# colnames(TRAIN_Imputed)
# colnames(TEST_Imputed)
# 
# # Exclude irrelevavant index columns
# TRAIN_Imputed=TRAIN_Imputed[,-c(1,2)]
# TEST_Imputed=TEST_Imputed[,-1]
# 
# colnames(TRAIN_Imputed)
# colnames(TEST_Imputed)
# 
# # Check the data types
# str(TRAIN_Imputed)
# names=c("Dropout","cohort_term","CohortTerm","RegistrationDate","BirthYear","","Zip"
#         ,"Hispanic","Black","White","HSDipYr","EnrollmentStatus","HighDeg","MathPlacement",
#         "EngPlacement","GatewayMathStatus","CompleteDevEnglish")
# TRAIN_Imputed[,names]<-lapply(TRAIN_Imputed[,names] , factor)
# 
# str(TRAIN_DATA)
# 
# # Implement feature selection using Boruta package 
# 
# library(Boruta)
# 
# # Now is the time to implement and check the performance of boruta package. The syntax of boruta is almost similar to regression (lm) method.
# 
# set.seed(123)
# boruta.train <- Boruta(Dropout~.-StudentID, data = TRAIN_Imputed, doTrace = 2)
# print(boruta.train)
# 
# 
# # Now, we'll plot the boruta variable importance chart.
# plot(boruta.train, xlab = "", xaxt = "n")
# lz<-lapply(1:ncol(boruta.train$ImpHistory),function(i)
#   boruta.train$ImpHistory[is.finite(boruta.train$ImpHistory[,i]),i])
# names(lz) <- colnames(boruta.train$ImpHistory)
# Labels <- sort(sapply(lz,median))
# axis(side = 1,las=2,labels = names(Labels),
#      at = 1:ncol(boruta.train$ImpHistory), cex.axis = 0.7)
# 
# 
# # Blue boxplots correspond to minimal, average and maximum Z score of a shadow attribute. Red, yellow and green boxplots represent Z scores of rejected, tentative and confirmed attributes respectively.
# # Now is the time to take decision on tentative attributes.  The tentative attributes will be classified as confirmed or rejected by comparing the median Z score of the attributes with the median Z score of the best shadow attribute. Let's do it.
# 
# final.boruta <- TentativeRoughFix(boruta.train)
# print(final.boruta)
# 
# # We'll create a data frame of the final result derived from Boruta.
# boruta.df <- attStats(final.boruta)
# class(boruta.df)
# 
# print(boruta.df)
# 
# 
# # It's time for results now. Let's obtain the list of confirmed attributes
# features=getSelectedAttributes(final.boruta, withTentative = F) 
# features
# 
# TRAIN_Features=TRAIN_Imputed[,features]
# 
# TRAIN_Features$Dropout=TRAIN_Imputed$Dropout
# dim(TRAIN_Features)
# 
# dim(TEST_Imputed)
# dim(TRAIN_Imputed)
# 
# # Also apply the feature selection on the TEST DATA
# TEST_Features=TEST_Imputed[,features]
# 
# 
# 
# 
# # Let's understand the parameters used in Boruta as follows:
# #   
# # - maxRuns: maximal number of random forest runs.
# # You can consider increasing this parameter if tentative attributes are left.Default is 100.
# # - doTrace: It refers to verbosity level. 0 means no tracing. 1 means reporting attribute decision as soon as it is cleared. 2 means all of 1 plus additionally reporting each iteration. Default is 0.
# # - holdHistory: The full history of importance runs is stored if set to TRUE
# # (Default). Gives a plot of Classifier run vs. Importance when the plotImpHistory function is called.
# 
# 
# ### Save the selected features data
# 
# write.csv(TRAIN_Features,"D:/KAZI/UPWORK/KAGGLE COMPETITION/TRAIN_Features.csv")
# write.csv(TEST_Features,"D:/KAZI/UPWORK/KAGGLE COMPETITION/TEST_Features.csv")
# 
# 
# 
# 
# str(TRAIN_Features)
# 



# MODELLING
# -----------------
# -----------------
# First split the dataset

#Load packages
library(tidyverse)
library(rpart)
library(dplyr)

# Load the datasets
# TRAIN_Features=read.csv("TRAIN_Features.csv",header = T)
# dim(TRAIN_Features)
# TEST_Features=read.csv("TEST_Features.csv",header = T)
# dim(TEST_Features)
# colnames(TRAIN_Features)
# colnames(TEST_Features)

TRAIN_Imputed=read.csv("TRAIN_Imputed.csv",header = T)

TRAIN_Features<-TRAIN_Imputed[,features]
TEST_Features<-TEST_Imputed[,features]
colnames(TRAIN_Features)

# PRE-PROCESS THE DATASET

library(tidyverse)
# TRAIN DATA
drop.cols <- c('City', 'Address1','Zip','RegistrationDate')
TRAIN_Features<-TRAIN_Features %>% select(-drop.cols)
dim(TRAIN_Features)
# TEST DATA
drop.cols <- c('City','Address1','Zip','RegistrationDate')
TEST_Features<-TEST_Features %>% select(-drop.cols)
dim(TEST_Features)


TRAIN_Features$Dropout=TRAIN_Imputed$Dropout
TRAIN_Features$Dropout=as.factor(TRAIN_Features$Dropout)


# DUMMIFY THE FACTOR VAKRIABLES
# Before we dummify the variables check to ensure the categories are have same count 
# in test and train dataset


str(TRAIN_Features)

library(dummies)
# TRAIN DATA
TRAIN_Features <- dummy.data.frame(TRAIN_Features, names = c("cohort_term",
          "Marital.Status" ,"Father.s.Highest.Grade.Level",
          "Mother.s.Highest.Grade.Level","Housing","Cohort") , sep = ".")
TRAIN_Features=scale(TRAIN_Features,names=c("Adjusted.Gross.Income","Parent.Adjusted.Gross.Income"))
  

# STANDARDIZATION
# -------------------
# load libraries
library(caret)

view(TRAIN_Features)
# calculate the pre-process parameters from the dataset
preprocessParams <- preProcess(TRAIN_Features[,7:56], method=c("center", "scale"))
# summarize transform parameters
print(preprocessParams)
# transform the dataset using the parameters
transformed_train <- predict(preprocessParams, TRAIN_Features[,7:56])
# summarize the transformed dataset
summary(transformed_train)
dim(transformed_train)



# TEST DATA
TEST_Features <- dummy.data.frame(TEST_Features, names = c("cohort_term",
            "Marital.Status" ,"Father.s.Highest.Grade.Level",
  "Mother.s.Highest.Grade.Level","Housing","Cohort") , sep = ".")
dim(TEST_Features)

# load libraries
library(caret)

# calculate the pre-process parameters from the dataset
preprocessParams <- preProcess(TEST_Features[,7:56], method=c("center", "scale"))
# summarize transform parameters
print(preprocessParams)
# transform the dataset using the parameters
transformed_test <- predict(preprocessParams, TRAIN_Features[,7:56])
# summarize the transformed dataset
summary(transformed_train)
dim(transformed_test)

view(transformed_test)


# SPlit the datasets
transformed_train$Dropout=TRAIN_Features$Dropout

set.seed(123)
## 80% of the sample size
smp_size <- floor(0.80 * nrow(transformed_train))
train_ind <- sample(seq_len(nrow(transformed_train)), size = smp_size)
train.set <- transformed_train[train_ind, ]
validation.set <- transformed_train[-train_ind, ]

# --------------------------------
# K - NEAREST NEOGHBORS MODEL
# ----------------------------------

#Creating seperate dataframe for 'Creditability' feature which is our target.
train.dropout_labels <- train.set$Dropout
val.dropout_labels <-validation.set$Dropout

#Install class package
# Load class package
library(class)

#Find the number of observation
NROW(train.dropout_labels) 


#Model optimization
i=1
k.optm=1
for (i in 1:28){
  knn.mod <- knn(train=train.set, test=validation.set, cl=train.dropout_labels, k=i)
  k.optm[i] <- 100 * sum(val.dropout_labels == knn.mod)/NROW(val.dropout_labels)
  k=i
  cat(k,'=',k.optm[i],'
')
}

# From the output you can see that for K = , we achieve the maximum accuracy, i.e. 68%. We can also represent this graphically, like so:

#Accuracy plot
plot(k.optm, type="b", xlab="K- Value",ylab="Accuracy level")


knn.24 <- knn(train=train.set, test=validation.set, cl=train.dropout_labels, k=24)
knn.23[1:10]
knn.28 <- knn(train=train.set, test=validation.set, cl=train.dropout_labels, k=28)
knn.28[1:10]

# Check prediction against actual value in tabular form for k=23
table(knn.24,val.dropout_labels)

# Check prediction against actual value in tabular form for k=28
table(knn.28 ,val.dropout_labels)


#Confusion Matrix and Statistics
library(caret)
confusionMatrix(table(knn.24 ,val.dropout_labels))

confusionMatrix(table(knn.28 ,val.dropout_labels))





# ----------------------
# RPART MODEL
# --------------------
#Load packages
library(tidyverse)
library(rpart)
library(dplyr)

install.packages("janitor")
library(janitor)

train.set <- clean_names(train.set)

model_tree<-train(Dropout ~. ,data=train.set, method = "rpart",
          trControl = trainControl("cv", number = 5),tuneLength = 3)
model_tree




#Fit a boosting model to the training set with  Dropout as the response and the other variables as predictors.
# Use 1,000 trees, and a shrinkage value of 0.01.
# check predictors that appear to be the most important

#Variable importance
library(gbm)

model.boost = gbm(formula=Dropout~ .,data=train.set, n.trees=1000,
                    shrinkage=.01)
summary(model.boost)

##Use the boosting model to predict the response on the test data.
#Predict that a student will dropout 
#Prediction 

probs = predict(model.boost, newdata=validation.set,n.trees=1000,type="response")
probs

pred = ifelse(probs>.5,1,0)


pred[1:5] #first five predicted values

test.y = validation.set$Dropout
test.y[1:5] #fist five

# Confusion matrix
confusionMat=table(pred,test.y)
confusionMat

#What fraction of the people predicted to make a purchase do in fact make one? 
confusionMat[4]/(confusionMat[3]+confusionMat[4])


# How does this compare with the results obtained from applying KNN or logistic regression to this data set?

# Apply Logistic regression
lm <-  glm(Dropout ~ . , data=train.set, family=binomial)
lm.prob <-  predict(lm, validstion.test, type="response")
lm.pred <-  ifelse(lm.prob > 0.5, "1", "0")

# confusion matrix
CF=table(test.y, lm.pred)
CF


#What fraction of the people predicted to make a purchase do in fact make one?
CF[4]/(CF[3]+CF[4])







# ENSEMBLING

##Try a stacking ensemble with at least two different models.set the response variable to factor
library(mlbench)
library(caret)
library(caretEnsemble)


TRAIN_Features$Dropout=as.factor(TRAIN_Features$Dropout)

class(Dropout)

# SPlit the datasets
set.seed(123)
## 80% of the sample size
smp_size <- floor(0.80 * nrow(TRAIN_Features))
train_ind <- sample(seq_len(nrow(TRAIN_Features)), size = smp_size)
train.set <- TRAIN_Features[train_ind, ]
validation.set <- TRAIN_Features[-train_ind, ]


# Example of Stacking algorithms
# create submodels
control<-trainControl(method="repeatedcv", number=10, repeats=3,
                      savePredictions=TRUE)

algorithmList <- c('rpart','glm', 'knn', 'svmRadial')

class(Dropout)


models <- caretList(Dropout ~. ,data=train.set,trControl=control
                    ,methodList=algorithmList)

results <- resamples(models)
summary(results)


dotplot(results)

# When we combine the predictions of different models using stacking, it is desirable that the predictions made by the sub-models have low correlation. This would suggest that the models are skillful but in different ways, allowing a new classifier to figure out how to get the best from each model for an improved score.

# If the predictions for the sub-models were highly correlated (>0.75) then they would be making the same or very similar predictions most of the time reducing the benefit of combining the predictions.

# correlation between results
modelCor(results)

#We can see that all pairs of predictions have generally low correlation.
#The two methods with the highest correlation between their predictions are Logistic Regression (GLM) and kNN at 0.517 correlation which is not considered high (>0.75).

splom(results)



# Let's combine the predictions of the classifiers using a simple linear model. stack using glm

stackControl <- trainControl(method="repeatedcv", number=10, repeats=3, savePredictions=TRUE, classProbs=TRUE)
set.seed(1)
stack.glm <-caretStack(models, method="glm", trControl=stackControl)
print(stack.glm)



# MODEL TESTING


# NEW KNN

# Distance
euclideanDist <- function(a, b){
  d = 0
  for(i in c(1:(length(a)-1) ))
  {
    d = d + (a[[i]]-b[[i]])^2
  }
  d = sqrt(d)
  return(d)
}

# KNN prediction

knn_predict <- function(validation.set, train.set, k_value){
  pred <- c()  #empty pred vector 
  #LOOP-1
  for(i in c(1:nrow(validation.set))){   #looping over each record of test data
    eu_dist =c()          #eu_dist & eu_char empty  vector
    eu_char = c()
    good = 0              #good & bad variable initialization with 0 value
    bad = 0
    
    #LOOP-2-looping over train data 
    for(j in c(1:nrow(train.set))){
      
      #adding euclidean distance b/w test data point and train data to eu_dist vector
      eu_dist <- c(eu_dist, euclideanDist(validation.set[i,], train.set[j,]))
      
      #adding class variable of training data in eu_char
      eu_char <- c(eu_char, as.character(train.set[j,][[6]]))
    }
    
    eu <- data.frame(eu_char, eu_dist) #eu dataframe created with eu_char & eu_dist columns
    
    eu <- eu[order(eu$eu_dist),]       #sorting eu dataframe to gettop K neighbors
    eu <- eu[1:k_value,]               #eu dataframe with top K neighbors
    
    #Loop 3: loops over eu and counts classes of neibhors.
    for(k in c(1:nrow(eu))){
      if(as.character(eu[k,"eu_char"]) == "g"){
        good = good + 1
      }
      else
        bad = bad + 1
    }
    
    # Compares the no. of neighbors with class label good or bad
    if(good > bad){          #if majority of neighbors are good then put "g" in pred vector
      
      pred <- c(pred, "g")
    }
    else if(good < bad){
      #if majority of neighbors are bad then put "b" in pred vector
      pred <- c(pred, "b")
    }
    
  }
  return(pred) #return pred vector
}


# Accuracy calculation
accuracy <- function(validation.set){
  correct = 0
  for(i in c(1:nrow(validation.set))){
    if(validation.set[i,6] == validation.set[i,7]){ 
      correct = correct+1
    }
  }
  accu = correct/nrow(validation.set) * 100  
  return(accu)
}


dim(train.set)
dim(validation.set)

K = 5
predictions <- knn_predict(validation.set, train.set, K) #calling knn_predict()


test.df[,52] <- predictions #Adding predictions in test data as 7th column
print(accuracy(test.df))



# SUBMISSION FILE
dim(train.set)
dim(transformed_test)
dim(validation.set)





