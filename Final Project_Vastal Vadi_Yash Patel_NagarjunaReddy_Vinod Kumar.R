require(dplyr)
require(ggplot2)
require(ggpubr)
require(gridExtra)
require(choroplethrMaps)
require(DT)
require(plyr)
require(pROC)
library(gridExtra)
library(dplyr)
library(ggpubr)
library(choroplethrMaps)
library(plyr)
library(DT)
library(ggthemes)
library(pROC)
#Disabling scientific notation in R
options(scipen = 999)
# raeding data from the file
data1<-read.csv(file.choose(),header=T,nrows = 887379)[ ,3:73]
dim(data1)
head(data1)
View(data1)
# merge the loan book with the state names
data1 <- merge(data1, state.regions, by.x = "addr_state", by.y = "abb")
colnames(data1)
#***************Explorarory data Analysis ****************
## 1.univariate data analysis
# Loan amount applied borrower
P1<-ggplot(data1, aes(loan_amnt,fill=I("lightblue"), col=I("white")))  + geom_histogram(binwidth=1000) +ggtitle("Loan Applied by borrower")
# total amount commited to loan at that point of time.
P2<-ggplot(data1, aes(funded_amnt,fill=I("blue"), col=I("white")))  +geom_histogram(binwidth=1000)+ ggtitle("Amount funded by the lender")
#amount given by investors 
P3<-ggplot(data1, aes(funded_amnt_inv,fill=I("lightgreen"), col=I("white")))  +geom_histogram(binwidth=1000) +  ggtitle("Total committed by Invetstor")
grid.arrange(P1, P2,P3, nrow = 1)
## Loan status.
#checking the differen loan status 
distinct(data1, loan_status)
Desc(data1$loan_status, main = "Loan Status", plotit = TRUE)
# Loan Purpose 
Desc(data1$purpose, main = "Loan purposes", plotit = TRUE)
# volume  by State
install.packages("choroplethrMaps")
library(choroplethrMaps)
library(choroplethr)
data(state.regions)
head(state.regions)
state_by_volume <-
  data1 %>% group_by(region) %>%
  summarise(value = n())
state_choropleth(state_by_volume, title = "Volume by State")
# Multivarriate analysis
## Grade vs Interst rate
ggplot(data1,aes(grade,int_rate,fill=grade))+geom_boxplot(show.legend = F)+
  facet_grid(.~is_bad)+
  stat_summary(geom = "text",fun.data=function(x) {return(c(y=median(x)*1.1,label=length(x)))})
##home ownership vs laon amount 
ggplot(data1,aes(home_ownership,loan_amnt,fill=home_ownership))+geom_boxplot(show.legend = F)+facet_grid(.~is_bad)+
  stat_summary(geom = "text",fun.data=function(x) {return(c(y=quantile(x,.75)[[1]],label=length(x)))},aes(vjust="bottom"))
## Grade Vs. Interest Rate Vs Tenure
ggplot(data1,aes(grade,int_rate,fill=grade))+geom_boxplot(show.legend = F)+facet_grid(.~term)+
  stat_summary(geom = "text",fun.data=function(x) {return(c(y=median(x)*1.1,label=length(x)))})
# for choosing the variables for Predictive modelling
# 'bad' statuses
bad_loanindicators <- c("Charged Off ","Default",
                    "In Grace Period", 
                    "Default Receiver", 
                    "Late (16-30 days)",
                    "Late (31-120 days)")
# assign certain statuses to a 'bad' ('0') group
data1$is_bad <- ifelse(data1$loan_status %in% bad_loanindicators, 0,
                          ifelse(data1$loan_status=="", NA, 1))
View(data1$is_bad)
# figure out which columns are numeric so that we can look at the distribution
numeric_cols <- sapply(data1, is.numeric)
library(reshape2)
data1.lng <- melt(data1[,numeric_cols], id="is_bad")
p <- ggplot(aes(x = value, group = is_bad, colour = factor(is_bad)), 
            data = data1.lng)
p + geom_density() +
  facet_wrap(~variable, scales="free")
library(DT)
data1 %>% 
  filter(is_bad == '0') %>% 
  select(loan_amnt, loan_status) %>% 
  datatable(., options = list(pageLength = 10))

#*******************************
# Predictive Modeling 
# Selecting the predictors and preparing data
data2<- select(data1,loan_status, loan_amnt, int_rate, grade, emp_length, home_ownership,annual_inc, term)
View(data2)
dim(data2)
sapply(data2 , function(x) sum(is.na(x)))
str(data2)
data2<-filter(data2,!is.na(annual_inc) , 
              !(home_ownership %in% c('NONE' , 'ANY')) , 
              emp_length != 'n/a')
data2 = data2 %>% mutate(loan_outcome = ifelse(loan_status %in% c('Charged Off' , 'Default') , 
                               1, 
                               ifelse(loan_status == 'Fully Paid' , 0 , 'No info')))
barplot(table(data2$loan_outcome) , col = 'lightblue')
loan2<-data2 %>%select(-loan_status) %>%filter(loan_outcome %in% c(0 , 1))
head(loan2)
# Partiotioning data Set
loan2$loan_outcome = as.numeric(loan2$loan_outcome)
idx = sample(dim(loan2)[1] , 0.70*dim(loan2)[1] , replace = F)
trainset = loan2[idx , ]
testset = loan2[-idx , ]
# Fit logistic regression
glm.model <-glm(loan_outcome ~ . , trainset , family = binomial(link = 'logit'))
summary(glm.model)
glm.model

# Prediction on test set
preds<-predict(glm.model , testset , type = 'response')
preds

# Density of probabilities
ggplot(data.frame(preds) , aes(preds)) + 
  geom_density(fill = 'lightblue' , alpha = 0.4) +
  labs(x = 'Predicted Probabilities on test set')

k = 0
accuracy = c()
sensitivity = c()
specificity = c()
for(i in seq(from = 0.01 , to = 0.5 , by = 0.01)){
  k = k + 1
  preds_binomial = ifelse(preds > i , 1 , 0)
  confmat = table(testset$loan_outcome , preds_binomial)
  accuracy[k] = sum(diag(confmat)) / sum(confmat)
  sensitivity[k] = confmat[1 , 1] / sum(confmat[ , 1])
  specificity[k] = confmat[2 , 2] / sum(confmat[ , 2])
}

threshold<-seq(from = 0.01 , to = 0.5 , by = 0.01)

data<-data.frame(threshold , accuracy , sensitivity , specificity)
head(data)
library(tidyr)
# Gather accuracy , sensitivity and specificity in one column
ggplot(gather(data , key = 'Metric' , value = 'Value' , 2:4) , 
       aes(x = threshold , y = Value , color = Metric)) + 
  geom_line(size = 1.5)

#confusion Matrix
preds.for.30<-ifelse(preds > 0.3 , 1 , 0)
confusion_matrix_30 = table(Predicted = preds.for.30 , Actual = testset$loan_outcome)
confusion_matrix_30
# Validation of predicted results
# Area Under Curve
auc(roc(testset$loan_outcome , preds))

# Plot ROC curve
plot.roc(testset$loan_outcome , preds , main = "Confidence interval of a threshold" , percent = TRUE , 
         ci = TRUE , of = "thresholds" , thresholds = "best" , print.thres = "best" , col = 'blue')

