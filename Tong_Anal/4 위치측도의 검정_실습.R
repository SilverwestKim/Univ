
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
## 10장                                                                    ## 
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##

## 예제 13 ------------------------------------------------------------------
# 자료 입력
height <- c(163, 161, 168, 161, 157, 162, 153, 159, 164, 170,
            152, 160, 157, 168, 150, 165, 156, 151, 162, 150,
            156, 152, 161, 165, 168, 167, 165, 168, 159, 156)


# H0: mu=159 vs. mu not= 159

length(height)

# 그림

hist(height)
hist(height,breaks=seq(150,171,4))

# 특별한 패턴을 찾기 힘들다. 

boxplot(height)

# 대칭인 분포의 boxplot을 가지고 있다.

# 통계량
mean(height)
var(height)
sd(height)
summary(height)

install.packages("moments")
library(moments)

skewness(height)
agostino.test(height)

kurtosis(height)
anscombe.test(height)

install.packages("nortest")
library(nortest)
shapiro.test(height)
lillie.test(height)

# 검정에 의하면 정규분포를 따른다는 귀무가설을 기각할 수 없다.
# (유의수준 0.05). 따라서, 데이터가 정규분포로부터 나왔다고
# 가정한다.

# 신뢰구간 구하기

mean <- mean(height)
sd <- sd(height)
n <- length(height)
z <- qnorm(1-0.05/2)

# 정규분포를 이용한 신뢰구간 ( n>=30 )
lcl <- mean-z*sd/sqrt(n) # 신뢰구간의 하한
ucl <- mean+z*sd/sqrt(n) # 신뢰구간의 상한

# t 분포를 이용한 신뢰구간
t <- qt(1-0.05/2,n-1)
lcl <- mean-t*sd/sqrt(n) # 신뢰구간의 하한
ucl <- mean+t*sd/sqrt(n) # 신뢰구간의 상한

# 검정 H0: mu=159 vs. H1: mu not= 159

# 검정통계량
tvalue <- (mean-159)/(sd/sqrt(n))

# p-value 구하기 (정규분포 이용)
pvalue <- 2*(1-pnorm(abs(tvalue)))
pvalue <- 2*pnorm(-abs(tvalue))

# p-value 구하기 (t-분포 이용)
pvalue <- 2*(1-pt(abs(tvalue),n-1))

# p-value가 0.05보다 크므로 유의수준 5%에서 귀무가설을 
# 기각할 수 없다. 따라서, 중심의 위치가 159라고 할 수 있다.




## 예제 15 ------------------------------------------------------------------
# 자료 입력
noise <- c(55.9, 63.8, 57.2, 59.8, 65.7, 62.7, 60.8, 51.3, 61.8, 56.0,
           66.9, 56.8, 66.2, 64.6, 59.5, 63.1, 60.6, 62.0, 59.4, 67.2,   
           63.6, 60.5, 66.8, 61.8, 64.8, 55.8, 55.7, 77.1, 62.1, 61.0,
           58.9, 60.0, 66.9, 61.7, 60.3, 51.5, 67.0, 60.2, 56.2, 59.4,
           67.9, 64.9, 55.7, 61.4, 62.6, 56.4, 56.4, 69.4, 57.6, 63.8)




## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
## 11장                                                                    ## 
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##

## 예제 09 ------------------------------------------------------------------
# 자료 입력
bacteria <- c(175, 190, 215, 198, 184, 207, 210, 193, 196, 180)


length(bacteria)

hist(bacteria)
boxplot(bacteria)
qqnorm(bacteria)
qqline(bacteria)

# 자료의 개수가 적어 히스토그램을 통하여 분포의 모양을 파악
# 하기는 쉽지 않으나 한쪽에 쏠려 있거나 다봉형으로 보이지는
# 않는다. 상자그림도 대칭인 분포를 보여주고 있으며 정규확률그림은
# 직선의 형태를 띄고 있으므로 정규분포에 가까운 분포로부터
# 데이터가 추출된 것으로 보인다.

skewness(bacteria)
agostino.test(bacteria)
# 왜도가 0에 가깝고 왜도가 0인지에 대한 검정에서 p-value(0.9099)가 
# 매우 크므로 왜도가 0이라고 할 수 있다. 즉, 대칭인 분포로부터
# 추출된 데이터라고 할 수 있다.

kurtosis(bacteria)
anscombe.test(bacteria)
# 첨도는 1.9로 3보다 작으나 첨도가 3인지에 대한 검정에서 p-value( 0.4629)가
# 크므로 첨도가 3이라고 할 수 있다.

# 왜도와 첨도로부터 데이터가 정규분포에서 추출되었을 가능성이 높다.


shapiro.test(bacteria)
lillie.test(bacteria)

# 두 개의 정규성 검정에서 p-value가 크므로 정규분포를 따른다고 
# 할 수 있다.

# 신뢰구간 구하기
mean<-mean(bacteria)
sd<-sd(bacteria)
n<-length(bacteria)
t<-qt(0.975,n-1)
lcl<-mean-t*sd/sqrt(n)
ucl<-mean+t*sd/sqrt(n)

# 가설 검정 H0: mu=200 vs. mu <200

tvalue<-(mean-200)/(sd/sqrt(n))
critical<- qt(0.05,n-1)

(tvalue<critical)

# t 값이 기각역에 속하지 않으므로 귀무가설을 기각할 수 없다.

pvalue<-pt(tvalue,n-1)

pvalue1<- 2*(1-pt(abs(tvalue),n-1)) # 양측검정에 대한 p-value

# p-value가 0.05보다 크므로 귀무가설을 기각할 수 없다. 따라서,
# 박테리아의 개수가 200보다 작다고 할 수 없다.


## 예제 10 ------------------------------------------------------------------
# 자료 입력
x <- c(31, 35, 37, 38, 38, 38, 39, 40, 40, 41 ,42 , 43 ,44 ,44 ,46 ,48)

length(x)

hist(x)

boxplot(x)

# 히스토그램과 상자그림에서 분포의 모양이 단봉형으로 보인다.

skewness(x)
agostino.test(x)

kurtosis(x)
anscombe.test(x)

qqnorm(x)
qqline(x)

shapiro.test(x)
lillie.test(x)

# 왜도가 0에 가깝고 왜도가 0인지 여부에 대한 검정에서 p-value가
# 0.05보다 크므로 왜도가 0이라 할 수 있다. (유의수준 0.05)
# 또한 첨도가 2.88로 3에 가깝고 첨도가 3이라는 가설 검정에서 
# p-value가 0.592로 크므로 첨도가 3이라는 귀무가설을 유의수준 
# 0.05에서 기각할 수 없으로 첨도가 정규분포와 같은 3이라 할 수 있다.
# 한편, 정규확률그림에서 점들이 직선에 가깝게 위치해 있고 정규분포 
# 여부에 대한 가설검정에서 Shaprio test나 Kolmogorov-Smirnov test
# 모두 p-value가 크므로 데이터가 정규분포로부터 추출되었다고 할 수
# 있다. (유의수준 0.05)

mean(x)
sd(x)

# 가설검정 H0: mu=40 vs. H1: mu not= 40

t.test(x,mu=40)

# 가설검정 H0: mu=40 vs. H1: mu < 40

t.test(x,mu=40, alternative=c("less"))


# 가설검정 H0: mu=40 vs. H1: mu > 40

t.test(x,mu=40, alternative="greater")




## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
## 비모수 검정                                                             ## 
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##

x <- c(2.3, 4.2, 2.6, 1.2, 1.6, 1.9, 2.5, 2.9, 3.6, 0.5, 3.1, 3.7, 2.2, 2.4)


length(x)

hist(x)

boxplot(x)

# 히스토그램과 상자그림에서 분포의 모양이 단봉형에 좌우 대칭으로 보인다.

skewness(x)
agostino.test(x)

kurtosis(x)
anscombe.test(x)

qqnorm(x)
qqline(x)

shapiro.test(x)
lillie.test(x)

# 왜도, 첨도, 정규확률그림, 정규성 검정 등의 결과를 종합하면
# 정규분포를 따른다고 할 수 있다.

mean(x)
sd(x)

# 가설검정 H0: mu=2.5 vs. mu not= 2.5

# 정규성 가정

t.test(x,mu=2.5)

# 기각역

c.region<-qt(1-0.05/2,length(x)-1)

# 왜도, 첨도, 정규성 검정 등으로부터 데이터가 정규분포로부터 추출되었다고 할 수 있는데
# 이 가정 하에서 p-value가 매우 크고 t 값의 절대값이 기각역보다 작으므로 유의수준 
# 0.05에서 귀무가설을 기각할 수 없다.


# 대칭성 가정

wilcox.test(x,mu=2.5)

# 히스토그램, 상자그림, 왜도 등에 따르면 대칭에 가까운 분포로부터 데이터가 추출
# 되었다고 할 수 있다. 정규분포에 대한 가정 없이 대칭만을 가정하여 Wilcoxon
# test를 가지고 가설검정을 하면 역시 p-value가 0.05보다 크므로 유의수준 0.05에서
# 귀무가설을 기각할 수 없다.


# signed test

install.packages("BSDA")
library(BSDA)
SIGN.test(x,md=2.5)

binom.test(6,14)

# 분포에 대한 가정이 없이 부호검정을 통하여 가설을 검정하면 역시 p-value가 0.05보다 크므로
# 귀무가설을 유의수준 0.05에서 기각할 수 없다. 따라서 중심의 위치가 2.5라고 할 수 있다.





