# Forbes2000 데이터 분석

# 작업공간 설정 
setwd("C:/Users/eunse/Desktop/tong")

# read data
Forbes<- read.csv("Forbes2000.csv",header=T)

# Forbes에 대한 summary

class(Forbes)
str(Forbes)
dim(Forbes)
nrow(Forbes)
ncol(Forbes)
names(Forbes)

Forbes[,2]
Forbes[,"name"]
Forbes$name

# maketvalue에 대한 조사
hist(Forbes$marketvalue)
# 0과 20 사이에 대부분의 관측값이 몰려 있다.
# 왼쪽으로 많이 쏠려 있는 것 같다.
boxplot(Forbes$marketvalue)
# 히스토그램에서와 같이 0과 20 사이에 많은 관측값(75%이상)이 
# 모여 있고 outer fence 밖으로 많은 관측값이 있어 왼쪽으로
# 쏠려 있는 형태의 상자그림이 나타나고 있다.

# 0과 20 사이의 marketvalue 분석
# marketvalue 20 이하인 관측값 선택 
sub<- Forbes[which(Forbes$marketvalue<=20),]
dim(sub)
hist(sub$marketvalue)
boxplot(sub$marketvalue)
# 왼쪽으로 쏠려 있는 형태를 가지고 있다.

# 전체적으로 왼쪽으로 쏠려 있는 형태를 가진 분포이다.

qqnorm(Forbes$marketvalue)

# 정규확률그림도 분포가 왼쪽으로 쏠려 있다는 것을 나타내고 있다.


# 기술통계량

mean(Forbes$marketvalue)
var(Forbes$marketvalue)
sd(Forbes$marketvalue)
summary(Forbes$marketvalue)

# 왜도, 첨도

#install.packages("moments")
library(moments)

skewness(Forbes$marketvalue)
# 왜도가 6.4로 분포가 상당히 왼쪽으로 쏠려 있는 것으로 보인다.

# 왜도가 0인지를 검정
agostino.test(Forbes$marketvalue)
# 귀무가설이 대칭이고 p-value가 0.05보다 작으므로 대칭이라고 
# 보기 힘들다. 따라서, 정규분포를 따른다고 하기 힘들다.


kurtosis(Forbes$marketvalue)
anscombe.test(Forbes$marketvalue)
# 귀무가설이 첨도가 3이다이고 p-value가 0.05보다 작으므로 
# 첨도가 3이라고 보기 힘들다. 따라서, 정규분포를 따른다고 
# 하기 힘들다.


# 정규성 검정

shapiro.test(Forbes$marketvalue)
# 귀무가설은 정규분포를 따른다이고 p-value가 0.05보다 작으므로
# 유의수준 5%에서 데이터가 정규분포로부터 나왔다고 보기 힘들다.

jarque.test(Forbes$marketvalue)

# 정규성 검정을 위한 package
install.packages("nortest")
library(nortest)

ad.test(Forbes$marketvalue)
lillie.test(Forbes$marketvalue)

# 모든  검정이 데이터가 정규분포로부터 나왔다고 하기 힘들다는 
# 결론은 내고 있다.

# 변수 변환

smarket <- sqrt(Forbes$marketvalue)
lmarket <- log(Forbes$marketvalue)

# sqrt

hist(smarket)
skewness(smarket)
qqnorm(smarket)
agostino.test(smarket)

# 원자료보다 쏠린 정도가 완화되기는 했으나 아직 왼쪽으로 쏠린 형태를 
# 가지고 있다.

# log

hist(lmarket)

# 왜도 

skewness(lmarket)
qqnorm(lmarket)
agostino.test(lmarket)

# log를 취한 경우 거의 종모양의 대칭의 분포를 갖는 것으로 보인다.
# 왜도에 대한 검정에서 분포가 대칭을 것을 알 수 있다. (유의수준=0.05)

# 첨도

kurtosis(lmarket)
anscombe.test(lmarket)

# kurtosis가 3보다 크고 정규확률그림에서 약간 역 S-자 형태를 보이고 
# 있으며 첨도에 대한 검정에서 p-value가 0.05보다 작으므로 첨도가 3
# 이라고 하기 힘들다. 따라서, 꼬리가 정규분포보다 두껍다고 할 수 있다.

# 정규성 검정

shapiro.test(lmarket)
ad.test(lmarket)
lillie.test(lmarket)

# 정규성 검정에 대한 모든 검정의 p-value가 0.05보다 작으므로 log를
# 취한 marketvalue는 정규분포를 따른다고 하기 힘들다. (유의수준=0.05)

# 결론적으로, marketvalue는 상당히 왼쪽으로 쏠려 있는 분포를 가지고 있고
# log를 취할 경우 종모양의 대칭 분포를 따르는 것으로 보이나 꼬리는 정규분포
# 보다 두꺼운 것으로 보인다. 정규분포를 따르는 것으로 보기 힘들다.