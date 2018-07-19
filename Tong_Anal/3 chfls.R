# ������ �ҷ� ���� 
chfls<-read.csv("CHFLS.csv",header=T)

str(chfls)

table(chfls$R_happy)
xtabs(~R_happy,data=chfls)
barplot(table(chfls$R_happy))

# somewhat happy�� ���� ���� very happy��
# �� �������� ����. ��ü������ not too happy��
# very unhappy�� ������ ���� �ʴ�.

# �ǰ����� vs. �ູ����

table(chfls$R_health)
barplot(table(chfls$R_health))

# not good�� poor�� ������ �����Ƿ� ����������
# �ǰ����°� ������ ���� ������ ���δ�.

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
# ��ü������ �ູ������ �ǰ��� ������ �־� ���δ�.
# very happy�� ��� �ǰ��� �������� ������ �þ�� 
# ������ �ִ� �ݸ�, very unhappy�� ���
# �ǰ��� poor���� �󵵰� �����ϴ� ���� �� �� �ִ�.

CrossTable(chfls$R_health,chfls$R_happy)
with(chfls,CrossTable(R_health,R_happy))

mosaicplot(with(chfls,table(R_region,R_happy)))








