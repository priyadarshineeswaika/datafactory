ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)}


# I am just calling the above function to the necessary packages :)
# btw install robustbase without compilation
ipak(c('stringr',
       'dplyr',
       'tidyverse',
       'robustbase',
       'caret',
       'RANN',
       'caretEnsemble',
       'pROC',
       'keras',
       'RCurl',
       'ggplot2'))
rm(ipak)

ds1 <- read.csv('2016 School Explorer.csv')
Schools <- ds1
#Reoving useless columns
ds1 <- ds1[,-c(1,2,3)]
ds1 <- ds1[, -c(39:158)]
names(ds1)
ds1$School.Name <- NULL
ds1$SED.Code <- NULL
ds1$District <- NULL
ds1$Address..Full. <- NULL
#ds1$Latitude <- NULL
#ds1$Longitude <- NULL
ds1$City <- NULL
ds1$Zip <- NULL
ds1$Grade.Low <- NULL
ds1$Grade.High <- NULL
ds1$Grades <- NULL
names(ds1)
glimpse(ds1)

#COnverting percentages into numerics
ds1$Percent.Asian <- as.numeric(sub("%","",ds1$Percent.Asian))/100
ds1$Percent.ELL <- as.numeric(sub("%","",ds1$Percent.ELL))/100
ds1$Percent.Black <- as.numeric(sub("%","",ds1$Percent.Black))/100
ds1$Percent.Black...Hispanic <- as.numeric(sub("%","",ds1$Percent.Black...Hispanic))/100
ds1$Percent.White <- as.numeric(sub("%","",ds1$Percent.White))/100
ds1$Percent.Hispanic <- as.numeric(sub("%","",ds1$Percent.Hispanic))/100
ds1$Percent.of.Students.Chronically.Absent <- as.numeric(sub("%","",ds1$Percent.of.Students.Chronically.Absent))/100
ds1$Student.Attendance.Rate <- as.numeric(sub("%","",ds1$Student.Attendance.Rate))/100
ds1$Rigorous.Instruction.. <- as.numeric(sub("%","",ds1$Rigorous.Instruction..))/100
ds1$Collaborative.Teachers.. <- as.numeric(sub("%","",ds1$Collaborative.Teachers..))/100
ds1$Supportive.Environment.. <- as.numeric(sub("%","",ds1$Supportive.Environment..))/100
ds1$Effective.School.Leadership.. <- as.numeric(sub("%","",ds1$Effective.School.Leadership..))/100
ds1$Strong.Family.Community.Ties.. <- as.numeric(sub("%","",ds1$Strong.Family.Community.Ties..))/100
ds1$Trust.. <- as.numeric(sub("%","",ds1$Trust..))/100
glimpse(ds1)

#Converting required numerics into numerics
ds1$Average.ELA.Proficiency <- as.numeric(ds1$Average.ELA.Proficiency)
ds1$Average.Math.Proficiency <- as.numeric(ds1$Average.Math.Proficiency)
ds1$Economic.Need.Index <- as.numeric(ds1$Economic.Need.Index)

#Convert school income estimate into numeric
#ds1$Location.Code <- as.numeric(sub("M","",ds1$LocationCode))
glimpse(ds1)
names(ds1)

ds1$Student.Achievement.Rating <- ifelse(ds1$Student.Achievement.Rating == 'Not Meeting Rating', 1, 
                                         ifelse(ds1$Student.Achievement.Rating == 'Meeting Target', 2,
                                                ifelse(ds1$Student.Achievement.Rating == 'Approaching Target', 3,
                                                       ifelse(ds1$Student.Achievement.Rating == 'EXceeding Target', 4,0))))
ds1$Rigorous.Instruction.Rating <- ifelse(ds1$Rigorous.Instruction.Rating == 'Not Meeting Rating', 1, 
                                         ifelse(ds1$Rigorous.Instruction.Rating == 'Meeting Target', 2,
                                                ifelse(ds1$Rigorous.Instruction.Rating == 'Approaching Target', 3,
                                                       ifelse(ds1$Rigorous.Instruction.Rating == 'EXceeding Target', 4,0))))
ds1$Collaborative.Teachers.Rating <- ifelse(ds1$Collaborative.Teachers.Rating == 'Not Meeting Rating', 1, 
                                          ifelse(ds1$Collaborative.Teachers.Rating == 'Meeting Target', 2,
                                                 ifelse(ds1$Collaborative.Teachers.Rating == 'Approaching Target', 3,
                                                        ifelse(ds1$Collaborative.Teachers.Rating == 'EXceeding Target', 4,0))))
ds1$Supportive.Environment.Rating <- ifelse(ds1$Supportive.Environment.Rating == 'Not Meeting Rating', 1, 
                                            ifelse(ds1$Supportive.Environment.Rating == 'Meeting Target', 2,
                                                   ifelse(ds1$Supportive.Environment.Rating == 'Approaching Target', 3,
                                                          ifelse(ds1$Supportive.Environment.Rating == 'EXceeding Target', 4,0))))
ds1$Trust.Rating <- ifelse(ds1$Trust.Rating == 'Not Meeting Rating', 1, 
                                            ifelse(ds1$Trust.Rating == 'Meeting Target', 2,
                                                   ifelse(ds1$Trust.Rating == 'Approaching Target', 3,
                                                          ifelse(ds1$Trust.Rating == 'EXceeding Target', 4,0))))
ds1$Strong.Family.Community.Ties.Rating <- ifelse(ds1$Strong.Family.Community.Ties.Rating == 'Not Meeting Rating', 1, 
                           ifelse(ds1$Strong.Family.Community.Ties.Rating == 'Meeting Target', 2,
                                  ifelse(ds1$Strong.Family.Community.Ties.Rating == 'Approaching Target', 3,
                                         ifelse(ds1$Strong.Family.Community.Ties.Rating == 'EXceeding Target', 4,0))))
ds1$Effective.School.Leadership.Rating <- ifelse(ds1$Effective.School.Leadership.Rating == 'Not Meeting Rating', 1, 
                                                  ifelse(ds1$Effective.School.Leadership.Rating == 'Meeting Target', 2,
                                                         ifelse(ds1$Effective.School.Leadership.Rating == 'Approaching Target', 3,
                                                                ifelse(ds1$Effective.School.Leadership.Rating == 'EXceeding Target', 4,0))))

ds1$Community.School.<- ifelse(ds1$Community.School.=='Yes', 1, 0)
#Encoding factor variables into numerics via dummy vars
#factor_set <- ds1[,c(2, 14, 16, 18, 20, 22, 24, 25)]
#ummies_transformer <- dummyVars( ~ ., data = factor_set, fullRank = T)
#numeric_set <- predict(dummies_transformer, newdata = factor_set)
#ds1 <- cbind(ds1[,-c(2, 14, 16, 18, 20, 22, 24, 25)],numeric_set)
#rm(numeric_set, factor_set)
#names(ds1)
#glimpse(ds1)

#Removing NA levels
#ds1$`Rigorous.Instruction.Rating.N/A` <- NULL
#ds1$`Collaborative.Teachers.Rating.N/A` <- NULL
#ds1$`Supportive.Environment.Rating.N/A` <- NULL
#ds1$`Effective.School.Leadership.Rating.N/A` <- NULL
#ds1$`Strong.Family.Community.Ties.Rating.N/A` <- NULL
#ds1$`Trust.Rating.N/A`<-NULL
#ds1$`Student.Achievement.Rating.N/A` <- NULL

#Converting Location Code and School Income Estimate into numeric
ds1$Location.Code <- as.numeric(gsub("[^0-9]", "", ds1$Location.Code))
ds1$School.Income.Estimate <- as.numeric(gsub("[^0-9]", "", ds1$School.Income.Estimate))

glimpse(ds1)

#Missing Value Imputation
methods <- c("medianImpute")
preProcess_transformer <- preProcess(x = ds1, method = methods)
ds1 <- predict(preProcess_transformer, newdata = ds1)
rm(methods)
glimpse(ds1)
#ds1$ID <- seq.int(nrow(ds1))

#Dimensionality Reduction

pca.model <- prcomp(ds1, scale=TRUE)
pca.pred <- predict(pca.model)
pca.pred
plot(pca.model, n.var = 20)

pca.pred <- pca.pred[,c(1:7)]

wcss = vector()
for (i in 1:20) wcss[i] = sum(kmeans(pca.pred, i)$withinss)
plot(1:20,
     wcss,
     type = 'b',
     main = paste('The Elbow Method'),
     xlab = 'Number of clusters',
     ylab = 'WCSS')


set.seed(42)
fit <- kmeans(pca.pred, 3) # 3 or 7/8 cluster solution
# get cluster means 
fit$centers
y_kmeans <- fit$cluster
# append cluster assignment
pca.pred <- data.frame(pca.pred, fit$cluster)
library(cluster)
clusplot(pca.pred,
         y_kmeans,
         lines = 0,
         shade = TRUE,
         color = TRUE,
         labels = 2,
         plotchar = FALSE,
         span = TRUE)
fit$centers
pca.pred$ID <- seq.int(nrow(pca.pred))
#How to get back school names from this?

names(Schools)
Schools$ID <- seq.int(nrow(Schools))
names(Schools)

x <- Schools[,c(162, 4, 8,9, 11, 16, 17, 18, 39)]
y <- pca.pred[,c(9,8)]

#School Names in the respective clusters
z <- left_join(x, y, by = 'ID', copy = FALSE)

library(leaflet)

palfact <- colorFactor(c("red", "green", "blue"),z$fit.cluster)

leaflet() %>% 
  addTiles() %>%
  addCircles(data=z, lat = ~Latitude, lng = ~Longitude,
             color =~palfact(fit.cluster),
             stroke = TRUE,
             radius = 3,
             fillOpacity = 0.75)

c1 <- z %>% filter(fit.cluster == 1)
c2 <- z %>% filter(fit.cluster == 2)
c3 <- z %>% filter(fit.cluster == 3)

c3$Student.Achievement.Rating <- ifelse(c3$Student.Achievement.Rating == 'Not Meeting Rating', 1, 
                                        ifelse(c3$Student.Achievement.Rating == 'Meeting Target', 2,
                                               ifelse(c3$Student.Achievement.Rating == 'Approaching Target', 3,
                                                      ifelse(c3$Student.Achievement.Rating == 'EXceeding Target', 4,0))))

#Interpretation of PCs
pca.model$rotation[,1:7]
#From here we can get demographic information of PCs

#Interpretation of Kmeans
fit$centers

median(c3$School.Income.Estimate, na.rm = TRUE)
c3$School.Income.Estimate <- as.numeric(gsub("[^0-9]", "", c3$School.Income.Estimate))

head(c3)

order.scores<-order(c3$Student.Achievement.Rating, c3$School.Income.Estimate,c3$Economic.Need.Index)
c3$rank <- NA
c3$rank[order.scores] <- nrow(c3):1
head(c3)

passnyc <- c3 %>% filter(rank <=10)
passnyc

