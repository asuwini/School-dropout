#library/packages

install.packages("party")
library(party)
library(caret)
library(rpart)
library(rpart.plot)
library(sandwich)
library(readxl)
library(lattice)
library(ggplot2)

#load the data
schooll<-read_excel("C:/Users/ELCOT-Lenovo/Desktop/schooll.xlsx")

#view
view(schooll)

#structure

str(schooll)

#head 

head(schooll)

#convert factor
schooll$gender=as.factor(schooll$gender)
schooll$dob=as.factor(schooll$dob)
schooll$School=as.factor(schooll$School)
schooll$`School Area`=as.factor(schooll$School)
schooll$Reason=as.factor(schooll$Reason)
schooll$Satisfied=as.factor(schooll$Satisfied)
schooll$area=as.factor(schooll$area)
schooll$std=as.factor(schooll$std)
schooll$`Father's Occupation`=as.factor(schooll$`Father's Occupation`)
schooll$`Father's  Monthly Income`=as.factor(schooll$`Father's  Monthly Income`)
schooll$diffcult=as.factor(schooll$diffcult)
schooll$Expect=as.factor(schooll$Expect)


#data slicing 
set.seed(2546)

#create partition
create_train_test(schooll,size=0.7,train=TRUE)

#train and test
train<-sample(1:nrow(schooll),0.8*nrow(schooll))
test<-setdiff(1:nrow(schooll),train)

x_train<-schooll[train,]
x_test<-schooll[-train,]

#ctree train and test
t2<-ctree(`School Area` ~ Reason+gender,data=x_train)
t3<-ctree(`School Area` ~ Reason+gender,data=x_test)

#plot ctree
plot(t2)
plot(t3)

#pie chart for satisfcation level of school dropout
x<-c(50,35,15)
label<-c("NO","MAYBE","YES")
pie(x,label,main="satisfication level of school dropout")

#histogram for DOB
schooll$std=as.numeric(schooll$std)
hist(schooll$dob)

#pie chart for Difficult
y<-c(28,26,9,23,12)
labe<-c("Teachers","The School Itself","Classmates","School Work","Hours")
pie(y,labe,main="Difficult part of the School from the Dropout Students",col=rainbow(length(y)))

#pie chart for Expectation level
z<-c(19,13,10,16,11,19,9)
labels<-c("Good Education","Good Staff/Tutor","Good Friends","Good Enivornment","Good Infrastrut","Good Sports Facilites","Good Spiritual")
pie(z,labels,main="Expectation from the School",col=rainbow(length(z)))

#predict the test dataset
predict_unseen<-predict(t3,x_test,type='prob')

#confusion matrix
table_mat<-table(x_test$`School Area`,predict_unseen)
table_mat

#finding accuracy
accuracy_test<-sum(diag(table_mat)/sum(table_mat))
print(paste("accuracy for test",accuracy_test))


