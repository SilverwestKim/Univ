rm(list=ls())

#데이터 로드
data(iris)

#구조 살펴보기 # 150 X 5
str(iris)

table(iris$Species)
prop.table(table(iris$Species))

#train/ test set 분리하기. 7:3의 비율로 분리하였습니다. 
set.seed(100)
idx<-sample(1:nrow(iris),0.7*nrow(iris),rep=F)
iris_train<-iris[idx,]
iris_test<-iris[-idx,]

train_labels<- iris_train$Species
class(train_labels)
test_labels<-iris_test$Species

library(class)
library(gmodels)
library(caret)

pred<-knn(iris_train[-5],iris_test[-5],train_labels,k=5)
CrossTable(test_labels,pred,prop.chisq=FALSE)

confusionMatrix(pred,iris_test$Species) #0.9556
#상당히 높은 분류율을 보이지만, k 값을 임의로 5개 주어서 계산하였으니
#다른 k 값들은 어떤지 비교해보고, 오분류율을 최소로 하는 k를 계산해보겠습니다.

#3부터 33까지의 홀수들을 k로 저장합니다.
k<-seq(3,33,by=2)
x<-c()
a<-c()

#k중 Accuracy들만 뽑아서 a vector에 저장해줍니다.
for(i in k){
  pre<-knn(iris_train[-5],iris_test[-5],train_labels,k=i)
  x<-confusionMatrix(pre,test_labels)
  a<-cbind(a,x[[3]][1])
}

#Accuracy 최댓값의 위치를 뽑은 후 best_k를 출력합니다. 
best<-which.max(a)
best_k<-k[best]
best_k #21일때 정분류율이 제일 높은 것을 알 수 있습니다.
   