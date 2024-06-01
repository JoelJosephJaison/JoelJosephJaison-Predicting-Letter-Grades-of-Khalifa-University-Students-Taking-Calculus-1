library(tidyverse)
library(caret)
data <- read.csv("C:/Users/dimad/OneDrive/Desktop/Multivariate Statistics/Calculus 1a.csv")
data
data <- filter(data,data$Did.you.take.the.Calculus.1.course.in.KU == "Yes")
data<-subset(data,select = -Did.you.take.the.Calculus.1.course.in.KU )
data <- subset(data, select = -Timestamp)
data
# Find the average of hours spent studying
data$How.many.hours.per.week.did.you.typically.spend.studying.for.Calculus.1.[data$How.many.hours.per.week.did.you.typically.spend.studying.for.Calculus.1. == "Zero"] <- 0 
data$How.many.hours.per.week.did.you.typically.spend.studying.for.Calculus.1.[data$How.many.hours.per.week.did.you.typically.spend.studying.for.Calculus.1. == "3-5 hours"] <- 4
data$How.many.hours.per.week.did.you.typically.spend.studying.for.Calculus.1.[data$How.many.hours.per.week.did.you.typically.spend.studying.for.Calculus.1. == "5-7 hours"] <- 6
data$How.many.hours.per.week.did.you.typically.spend.studying.for.Calculus.1.[data$How.many.hours.per.week.did.you.typically.spend.studying.for.Calculus.1. == "7-9 hours"] <- 8
data$How.many.hours.per.week.did.you.typically.spend.studying.for.Calculus.1.[data$How.many.hours.per.week.did.you.typically.spend.studying.for.Calculus.1. == "10 hours and more"] <- 10
str(data$How.many.hours.per.week.did.you.typically.spend.studying.for.Calculus.1.)

data$How.many.hours.per.week.did.you.typically.spend.studying.for.Calculus.1.<- as.numeric(data$How.many.hours.per.week.did.you.typically.spend.studying.for.Calculus.1.)
str(data)
data$Level<-as.factor(data$Level)
data$Age<-as.numeric(data$Age)
data$Gender<-as.factor(data$Gender)
data$What.was.your.grade.in.Calculus.1<-as.factor(data$What.was.your.grade.in.Calculus.1)
data$Did.you.take.any..prior.calculus.or.math.related.coursework.in.High.School.before.enrolling.in.Calculus.1.<-as.factor(data$Did.you.take.any..prior.calculus.or.math.related.coursework.in.High.School.before.enrolling.in.Calculus.1.)
data$Did.you.form.study.groups.with.classmates.for.Calculus.1.<-as.factor(data$Did.you.form.study.groups.with.classmates.for.Calculus.1.)
data$How.frequently.did.you.use.online.resources.to.supplement.your.Calculus.1.studies.<-as.factor(data$How.frequently.did.you.use.online.resources.to.supplement.your.Calculus.1.studies.)
data$Do.you.ever.get.anxious.when.doing.the.test.including.quizzes.midterms.and.finals...<-as.factor(data$Do.you.ever.get.anxious.when.doing.the.test.including.quizzes.midterms.and.finals...)
data$Did.you.use.to.solve.all.recommended.problems.<-as.factor(data$Did.you.use.to.solve.all.recommended.problems.)
data$Did.you.follow.up.with.the.material.or.do.you.usually.study.for.the.test.last.minute.<-as.factor(data$Did.you.follow.up.with.the.material.or.do.you.usually.study.for.the.test.last.minute.)
data$Did.you.usually.attend.office.hour.<-as.factor(data$Did.you.usually.attend.office.hour.)
str(data)
data1<-data
colnames(data1) <- c("A","B","C","D","E","F","G","H","I","J","K","L")
data2<-data1
# Define the condition where the unusual responses exists
condition <- (data2$D == "A") & (data2$F < 6) & (data2$I == "Often") & (data2$J == "No") & (data2$K == "Last minute Studying")
condition2 <- (data2$D == "A") & (data2$F < 6) & (data2$J == "No") & (data2$K == "Last minute Studying")
condition3 <- (data2$D == "A") & (data2$F < 6) & (data2$K == "Last minute Studying")
# Subset the data frame to remove rows that meet the conditions
data5 <- data2[!condition, ]
data5 <- data2[!condition2, ]
data5 <- data2[!condition3, ]
conditions_to_remove<- c("C-", "D", "F")
data6 <- data5[!(data5$D %in% conditions_to_remove), ]
data6$D <- factor(data6$D)
#EDA
#Plot 1: Pie chart for Grade letter distribution
df <- data6 %>% 
  group_by(D) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(perc = `n` / sum(`n`)) %>% 
  arrange(perc) %>%
  mutate(labels = scales::percent(perc))
ggplot(df, aes(x = "", y = perc, fill = D)) +
  geom_col(color = "black") +
  coord_polar(theta = "y")+
  geom_text(aes(label = labels),
            position = position_stack(vjust =0.5))+
  guides(fill = guide_legend(title = "Grade Level"))+
  labs(title = "Pie chart for Grade letter distribution") +
  theme_void()
#Plot 2: Pie chart for level distribution 
df1 <- data6 %>% 
  group_by(A) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(perc1 = `n` / sum(`n`)) %>% 
  arrange(perc1) %>%
  mutate(labels = scales::percent(perc1))
ggplot(df1, aes(x = "", y = perc1, fill = A)) +
  geom_col(color = "black") +
  coord_polar(theta = "y")+
  geom_text(aes(label = labels),
            position = position_stack(vjust =0.5))+
  guides(fill = guide_legend(title = "Student Level"))+
  labs(title = "Pie chart for Level distribution") +
  theme_void()
#Plot 3: Pie chart for gender distribution 
df2 <- data6 %>% 
  group_by(C) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(perc2 = `n` / sum(`n`)) %>% 
  arrange(perc2) %>%
  mutate(labels = scales::percent(perc2))
ggplot(df2, aes(x = "", y = perc2, fill = C)) +
  geom_col(color = "black") +
  coord_polar(theta = "y")+
  geom_text(aes(label = labels),
            position = position_stack(vjust =0.5))+
  guides(fill = guide_legend(title = "Gender"))+
  labs(title = "Pie chart for Gender distribution") +
  theme_void()
#Plot 4: Pie chart for prior course work distribution 
df3 <- data6 %>% 
  group_by(E) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(perc3 = `n` / sum(`n`)) %>% 
  arrange(perc3) %>%
  mutate(labels = scales::percent(perc3))
ggplot(df3, aes(x = "", y = perc3, fill = E)) +
  geom_col(color = "black") +
  coord_polar(theta = "y")+
  geom_text(aes(label = labels),
            position = position_stack(vjust =0.5))+
  guides(fill = guide_legend(title = "Response"))+
  labs(title = "Pie chart for taken prior course work response") +
  theme_void()+
  scale_fill_brewer(palette = "Pastel2")
#Plot 5: Pie chart for Study Group variable 
df4 <- data6 %>% 
  group_by(G) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(perc4 = `n` / sum(`n`)) %>% 
  arrange(perc4) %>%
  mutate(labels = scales::percent(perc4))
ggplot(df4, aes(x = "", y = perc4, fill = G)) +
  geom_col(color = "black") +
  coord_polar(theta = "y")+
  geom_text(aes(label = labels),
            position = position_stack(vjust =0.5))+
  guides(fill = guide_legend(title = "Response"))+
  labs(title = "Pie chart for study group formation response") +
  theme_void()+
  scale_fill_brewer(palette = "Accent")
#Plot 6: Pie chart for online resource usage variable 
df5 <- data6 %>% 
  group_by(H) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(perc5 = `n` / sum(`n`)) %>% 
  arrange(perc5) %>%
  mutate(labels = scales::percent(perc5))

ggplot(df5, aes(x = "", y = perc5, fill = H)) +
  geom_col(color = "black") +
  coord_polar(theta = "y")+
  geom_text(aes(label = labels),
            position = position_stack(vjust =0.5))+
  guides(fill = guide_legend(title = "Response"))+
  labs(title = "Pie chart for usage of online resources response") +
  theme_void()+
  scale_fill_brewer(palette = "Pastel1")
#Plot 7: Pie chart for test anxiety variable 
df6 <- data6 %>% 
  group_by(I) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(perc6 = `n` / sum(`n`)) %>% 
  arrange(perc6) %>%
  mutate(labels = scales::percent(perc6))
ggplot(df6, aes(x = "", y = perc6, fill = I)) +
  geom_col(color = "black") +
  coord_polar(theta = "y")+
  geom_text(aes(label = labels),
            position = position_stack(vjust =0.5))+
  guides(fill = guide_legend(title = "Response"))+
  labs(title = "Pie chart for test anxiety response") +
  theme_void()+
  scale_fill_brewer(palette = "Paired")
#Plot 8: Pie chart for solve recommended problems variable 
df7 <- data6 %>% 
  group_by(J) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(perc7 = `n` / sum(`n`)) %>% 
  arrange(perc7) %>%
  mutate(labels = scales::percent(perc7))
ggplot(df7, aes(x = "", y = perc7, fill = J)) +
  geom_col(color = "black") +
  coord_polar(theta = "y")+
  geom_text(aes(label = labels),
            position = position_stack(vjust =0.5))+
  guides(fill = guide_legend(title = "Response"))+
  labs(title = "Pie chart showing recommended problems solved response") +
  theme_void()+
  scale_fill_brewer(palette = "Set2")
#Plot 9: Pie chart for study method 
df8 <- data6 %>% 
  group_by(K) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(perc8 = `n` / sum(`n`)) %>% 
  arrange(perc8) %>%
  mutate(labels = scales::percent(perc8))
ggplot(df8, aes(x = "", y = perc8, fill = K)) +
  geom_col(color = "black") +
  coord_polar(theta = "y")+
  geom_text(aes(label = labels),
            position = position_stack(vjust =0.5))+
  guides(fill = guide_legend(title = "Response"))+
  labs(title = "Pie chart showing study method distributions") +
  theme_void()+
  scale_fill_brewer(palette = "Set3")
#Plot 10: Pie chart for office hours variable 
df9 <- data6 %>% 
  group_by(L) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(perc9 = `n` / sum(`n`)) %>% 
  arrange(perc9) %>%
  mutate(labels = scales::percent(perc9))
ggplot(df9, aes(x = "", y = perc9, fill = L)) +
  geom_col(color = "black") +
  coord_polar(theta = "y")+
  geom_text(aes(label = labels),
            position = position_stack(vjust =0.5))+
  guides(fill = guide_legend(title = "Response"))+
  labs(title = "Pie chart showing attending office hours response") +
  theme_void()+
  scale_fill_brewer(palette = "Spectral")
#Plot 11: Age Distribution Bar Plot
ggplot(data6, aes(x=B)) + 
  geom_bar(fill = "maroon",position= "dodge")+
  labs(title = "Age Distribution Bar Plot")+
  xlab("Age")+
  geom_text(stat='count',aes(label=after_stat(count)),position = position_dodge(width = 1),vjust = -0.5, size = 3)
#Plot 12: Study hours Bar Plot
ggplot(data6, aes(x=F)) + 
  geom_bar(fill = "#89CFF0",position= "dodge")+
  labs(title = "Study Hours Distribution Bar Plot")+
  xlab("Study Hours")+
  geom_text(stat='count',aes(label=after_stat(count)),position = position_dodge(width = 1),vjust = -0.5, size = 3)
#one hot encoding
data6<-subset(data6,select = -c(1,2,5,7,8,9,10,12) ) #Remove variables that make the accuracy higher
data6$C <- ifelse(data6$C=="Female",0,1)
data6$K <- ifelse(data6$K=="Follow up - (day to day studying)",1,0)
data6 <- data6[, order(names(data6))]
# Training and testing the data
set.seed(123)
data_obs <- nrow(data6)
training.sample <- sample(data_obs,size=trunc(0.8*data_obs))
training.sample
train<-data6[training.sample,]
test<-data6[-training.sample,]
# Random forest model 
library(randomForest)
classifier_RF = randomForest(x = train[-2], 
                             y = train$D, 
                             ntree = 50) 
y_pred<- predict(classifier_RF, newdata = test[-2]) 
confusion_mtx <-  confusionMatrix(y_pred, test[, 2]) 
print(confusion_mtx)
# variable importance
importance(classifier_RF) 

# LDA model
library(MASS)
model<- MASS::lda(D ~ ., data = train)
summary(model)
model
predict(model,test)$class
table(test$D)               
model$prior
pred_lda<-predict(model,test)$class
confusionMatrix(pred_lda,test$D)
# SVM model
library(e1071)
svm_model <- svm(D ~ ., data = train, kernel = "linear")
svm_model
summary(svm_model)
svm_model
predict(svm_model,test)
table(test$D)
pred_svm<-predict(svm_model,test)
confusionMatrix(pred_svm,test$D)
# Naive Bayes model
library(e1071)
nb_model <- naiveBayes(D ~ ., data = train)
pred_nb<-predict(nb_model, test)
confusionMatrix(pred_nb, test$D)
# Multinomial model
library(nnet)
multi <- nnet::multinom(D ~., data = train)
pred_mt <- predict(multi,test)
confusionMatrix(pred_mt,test$D)
