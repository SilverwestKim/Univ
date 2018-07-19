# 난수 생성 (random number generation

y1<-rnorm(100,mean=4,sd=1)
y2<-rnorm(50,mean=6,sd=2)


# histogram

hist(y1)
hist(y2)

# 같은 화면에 출력

par(mfrow=c(2,1))
# y1과 y2의 최소, 최대 확인
summary(y1)
summary(y2)
hist(y1,xlim=c(0,12))
hist(y2,xlim=c(0,12))

# 겹쳐 그리기
par(mfrow=c(1,1))

hist(y1,xlim=c(0,12),freq=F,col=rgb(1,0,0,0.5))
hist(y2,xlim=c(0,12),add=T,freq=F,col=rgb(0,0,1,0.5))

# density plot

d1<-density(y1)
d2<-density(y2)

par(mfrow=c(2,1))
plot(d1,xlim=c(0,12))
plot(d2,xlim=c(0,12))

par(mfrow=c(1,1))
plot(d1,xlim=c(0,12),col="red")
lines(d2,col="blue")

# y1과 y2의 분포를 보면 둘 다 종모양에 가까워 보인다.
# y1은 y2에 비하여 중심의 위치가 작고 퍼진 정도도 작아보인다.

# y1과 y2의 qqplot

qqplot(y1,y2)
abline(a=0,b=1)

# qqplot에서 점들이 직선을 이루고 있으므로 같은 종류의 분포라고
# 할 수 있다. 즉, 한 자료의 선형변환으로 다른 자료와 같은 분포를
# 만들 수 있는 것으로 보인다. 그러나, 점들이 y=x 위에 위치하지
# 않으으로 모수까지 동일한 분포로 볼 수 없다. 즉, y1과 y2는
# 같은 종류의 분포로부터 추출된 것으로 보이나 중심의 위치나 
# 퍼진 정도가 같다고 볼 수 없다.


mean1<-mean(y1)
mean2<-mean(y2)
var1<-var(y1)
var2<-var(y2)
sd1<-sd(y1)
sd2<-sd(y2)
n1<-length(y1)
n2<-length(y2)

# y1의 평균이 y2의 평균보다 작고 표본분산도 작아보인다.
# 특히, y2의 표본분산이 y1의 표본분산의 4배 가까이 되므로 
# 두 모집단의 분산이 같다고 하기 힘들어 보인다.


# 정규성 검정
install.packages("moments")
library(moments)
install.packages("nortest")
library(nortest)

skewness(y1)
agostino.test(y1)
skewness(y2)
agostino.test(y2)

kurtosis(y1)
anscombe.test(y1)
kurtosis(y2)
anscombe.test(y2)

qqnorm(y1)
qqline(y1)

qqnorm(y2)
qqline(y2)

shapiro.test(y1)
lillie.test(y1)
shapiro.test(y2)
lillie.test(y2)

# y1과 y2 모두 왜도가 0에 가깝고 (유의수준=0.05), 첨도도
# 3에 가까우며(유의수준=0.05), 정규확률그림이 직선에 가깝고
# 정규성 검정에서 유의수준 0.05에서 귀무가설을 기각할 수 없으믈
# y1과 y2는 정규분포로부터 추출된 데이터라고 할 수 있다.

# H0: mu1=mu2 vs. H1: mu1 not= mu2



# 분산의 동일성 검정

var.test(y1,y2)

# 표본분산의 비가 2가 넘었고 p-value가 0.05보다 작으므로 
# 유의수준 0.05에서 두 모집단의 분산이 같다고 할 수 없다.


# 분산의 같다고 가정

var_pooled <- ((n1-1)*var1+(n2-1)*var2)/(n1+n2-2)
t1 <- (mean1-mean2)/sqrt(var_pooled*(1/n1+1/n2))
pvalue <- 2*(1-pt(abs(t1),n1+n2-2))

t.test(y1,y2, var.equal = TRUE)


# 분산이 다르고 정규분포를 따른다고 가정할 경우

t2 <- (mean1-mean2)/sqrt((var1/n1+var2/n2))
# Satterthwaite 자유도

w1 <- var1/n1
w2 <- var2/n2
df<- (w1+w2)*(w1+w2)/(w1*w1/(n1-1)+w2*w2/(n2-1))
pvalue <- 2*(1-pt(abs(t2),df))

t.test(y1,y2)

# 교과서에서의 자유도 사용
df1 <- min(n1-1, n2-2)
pvalue <- 2*(1-pt(abs(t2),df1))


# 중심극한정리 이용 (n1>=30, n2>=30)
pvalue <- 2*(1-pnorm(abs(t2)))

# 비모수 검정: 정규분포를 따르지 않는 경우

wilcox.test(y1,y2)


# 어느 경우에나 p-value가 0.05보다 작으므로 두 모집단의
# 평균이 같다고 할 수 없다.



ks.test(y1,y2)

# p-value가 0.05보다 작으므로 유의수준 5%에서 
# 두 모집단의 분포가 다르고 할 수 있다.


y1<-rnorm(100,mean=4,sd=1)
y2<-rnorm(50,mean=4,sd=2.5)

t.test(y1,y2)

ks.test(y1,y2)

# 두 모집단의 평균은 같다고 할 수 있으나 분포가 동일하다고 
# 할 수 없다.