# 데이터 불러 오기 
chfls<-read.csv("CHFLS.csv",header=T)

str(chfls)

table(chfls$R_happy)
xtabs(~R_happy,data=chfls)
barplot(table(chfls$R_happy))

# somewhat happy가 가장 많고 very happy가
# 그 다음으로 많다. 전체적으로 not too happy와
# very unhappy의 비율이 높지 않다.

# 건강상태 vs. 행복정도

table(chfls$R_health)
barplot(table(chfls$R_health))

# not good과 poor의 비율이 작으므로 전반적으로
# 건강상태가 나쁘지 않은 것으로 보인다.

table<-table(chfls$R_health,chfls$R_happy)
table

prop.table(table)
margin.table(table,margin=1)
margin.table(table,margin=2)

install.packages("gmodels")
library(gmodels)

CrossTable(table,prop.r=T,prop.c=F,prop.t=F,expected=F,prop.chisq=F)

CrossTable(table,prop.r=T,prop.c=F,prop.t=F,expected=T,prop.chisq=T)

mosaicplot(table)
# 전체적으로 행복정도와 건강이 연관이 있어 보인다.
# very happy인 경우 건강이 좋을수록 비율이 늘어나는 
# 경향이 있는 반면, very unhappy의 경우
# 건강이 poor에서 빈도가 증가하는 것을 알 수 있다.

CrossTable(chfls$R_health,chfls$R_happy)
with(chfls,CrossTable(R_health,R_happy))

mosaicplot(with(chfls,table(R_region,R_happy)))









