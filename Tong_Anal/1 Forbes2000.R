# Forbes2000 ������ �м�

# �۾����� ���� 
setwd("C:/Users/eunse/Desktop/tong")

# read data
Forbes<- read.csv("Forbes2000.csv",header=T)

# Forbes�� ���� summary

class(Forbes)
str(Forbes)
dim(Forbes)
nrow(Forbes)
ncol(Forbes)
names(Forbes)

Forbes[,2]
Forbes[,"name"]
Forbes$name

# maketvalue�� ���� ����
hist(Forbes$marketvalue)
# 0�� 20 ���̿� ��κ��� �������� ���� �ִ�.
# �������� ���� ��� �ִ� �� ����.
boxplot(Forbes$marketvalue)
# ������׷������� ���� 0�� 20 ���̿� ���� ������(75%�̻�)�� 
# �� �ְ� outer fence ������ ���� �������� �־� ��������
# ��� �ִ� ������ ���ڱ׸��� ��Ÿ���� �ִ�.

# 0�� 20 ������ marketvalue �м�
# marketvalue 20 ������ ������ ���� 
sub<- Forbes[which(Forbes$marketvalue<=20),]
dim(sub)
hist(sub$marketvalue)
boxplot(sub$marketvalue)
# �������� ��� �ִ� ���¸� ������ �ִ�.

# ��ü������ �������� ��� �ִ� ���¸� ���� �����̴�.

qqnorm(Forbes$marketvalue)

# ����Ȯ���׸��� ������ �������� ��� �ִٴ� ���� ��Ÿ���� �ִ�.


# �����跮

mean(Forbes$marketvalue)
var(Forbes$marketvalue)
sd(Forbes$marketvalue)
summary(Forbes$marketvalue)

# �ֵ�, ÷��

#install.packages("moments")
library(moments)

skewness(Forbes$marketvalue)
# �ֵ��� 6.4�� ������ ����� �������� ��� �ִ� ������ ���δ�.

# �ֵ��� 0������ ����
agostino.test(Forbes$marketvalue)
# �͹������� ��Ī�̰� p-value�� 0.05���� �����Ƿ� ��Ī�̶�� 
# ���� �����. ����, ���Ժ����� �����ٰ� �ϱ� �����.


kurtosis(Forbes$marketvalue)
anscombe.test(Forbes$marketvalue)
# �͹������� ÷���� 3�̴��̰� p-value�� 0.05���� �����Ƿ� 
# ÷���� 3�̶�� ���� �����. ����, ���Ժ����� �����ٰ� 
# �ϱ� �����.


# ���Լ� ����

shapiro.test(Forbes$marketvalue)
# �͹������� ���Ժ����� �������̰� p-value�� 0.05���� �����Ƿ�
# ���Ǽ��� 5%���� �����Ͱ� ���Ժ����κ��� ���Դٰ� ���� �����.

jarque.test(Forbes$marketvalue)

# ���Լ� ������ ���� package
install.packages("nortest")
library(nortest)

ad.test(Forbes$marketvalue)
lillie.test(Forbes$marketvalue)

# ���  ������ �����Ͱ� ���Ժ����κ��� ���Դٰ� �ϱ� ����ٴ� 
# ����� ���� �ִ�.

# ���� ��ȯ

smarket <- sqrt(Forbes$marketvalue)
lmarket <- log(Forbes$marketvalue)

# sqrt

hist(smarket)
skewness(smarket)
qqnorm(smarket)
agostino.test(smarket)

# ���ڷẸ�� �� ������ ��ȭ�Ǳ�� ������ ���� �������� �� ���¸� 
# ������ �ִ�.

# log

hist(lmarket)

# �ֵ� 

skewness(lmarket)
qqnorm(lmarket)
agostino.test(lmarket)

# log�� ���� ��� ���� ������� ��Ī�� ������ ���� ������ ���δ�.
# �ֵ��� ���� �������� ������ ��Ī�� ���� �� �� �ִ�. (���Ǽ���=0.05)

# ÷��

kurtosis(lmarket)
anscombe.test(lmarket)

# kurtosis�� 3���� ũ�� ����Ȯ���׸����� �ణ �� S-�� ���¸� ���̰� 
# ������ ÷���� ���� �������� p-value�� 0.05���� �����Ƿ� ÷���� 3
# �̶�� �ϱ� �����. ����, ������ ���Ժ������� �β��ٰ� �� �� �ִ�.

# ���Լ� ����

shapiro.test(lmarket)
ad.test(lmarket)
lillie.test(lmarket)

# ���Լ� ������ ���� ��� ������ p-value�� 0.05���� �����Ƿ� log��
# ���� marketvalue�� ���Ժ����� �����ٰ� �ϱ� �����. (���Ǽ���=0.05)

# ���������, marketvalue�� ����� �������� ��� �ִ� ������ ������ �ְ�
# log�� ���� ��� ������� ��Ī ������ ������ ������ ���̳� ������ ���Ժ���
# ���� �β��� ������ ���δ�. ���Ժ����� ������ ������ ���� �����.