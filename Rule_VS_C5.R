##### Set env

path <- readline()# set your path below
C:\DirveD\Git\R_Projects\Supervised\Rule
setwd(path )


dir()
#"RuleDataSet.csv"

####Loading data
df = read.csv("RuleDataSet.csv" , stringsAsFactors = TRUE)


#### Exploring data
str(df)
summary(df)

#veil_type has one factor that we ignore it
df$veil_type <- NULL
prop.table(table(df$type))

#### sampling and split Data
X <- df[-1]
y <- df$type

library(caTools)
set.seed(900)
sample_model <-  sample.split(Y =y , SplitRatio = .80)


train <- subset(x = df ,sample_model==TRUE)
test <- subset(x = df, sample_model==FALSE)



####Modeling data with OneR Algorithm
library(OneR)
#fit Model with Train data
model_oner <- OneR(type ~ . ,data = train)
summary(model_oner)

#predict with Test data
model_oner_predict <- predict(object = model_oner,newdata =test )

#summary Results as table
table(actual = test$type , predicted= model_oner_predict)

####Modeling data with C.50 Algorithm

library(C50)
model_c5r <-  C5.0(type ~ . , train ,rules =TRUE)
summary(model_c5r)


#predict with Test data
model_c5r_predict <- predict(model_c5r ,test)

#summary Results as table
table(actual = test$type , predicted = model_c5r_predict)

###The comparison of the results shows: C5 Algorithm has a beter results
