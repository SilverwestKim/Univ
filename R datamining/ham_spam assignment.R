##R datamining assignment 

rm(list=ls())
setwd("C:/Users/eunse/Desktop/Rdatamining/chapter 4")

# read the sms data into the sms data frame
sms_raw <- read.csv("sms_spam.csv", stringsAsFactors = FALSE)

# examine the structure of the sms data
str(sms_raw)

###누락된 부분이 있는것 같아 보여서 수정
sms_raw[1072,"type"]<-factor("ham")
sms_raw[1072,"text"]<-c("All done, all handed in. Don't know if mega shop in asda counts as celebration but thats what i'm doing!")

# convert spam/ham to factor.
sms_raw$type <- factor(sms_raw$type)

# examine the type variable more carefully
str(sms_raw$type)
table(sms_raw$type)

# build a corpus using the text mining (tm) package
library(tm)

#####인코딩 때문에 오류가 발생하므로 실행해줍니다.
sms_raw$text<-iconv(enc2utf8(sms_raw$text),sub="byte")

sms_corpus <- Corpus(VectorSource(sms_raw$text))

# examine the sms corpus
print(sms_corpus)
inspect(sms_corpus[1:3])


scc<-tm_map(sms_corpus,content_transformer(tolower))
scc<-tm_map(scc,removeNumbers)
scc<-tm_map(scc,removeWords,stopwords())
scc<-tm_map(scc,removePunctuation)

library(SnowballC)

scc<-tm_map(scc,stemDocument)
scc<-tm_map(scc,stripWhitespace)

sms.dtm<-DocumentTermMatrix(scc)
dim(sms.dtm)

sms.freq.words<-findFreqTerms(sms.dtm,5)
str(sms.freq.words)

sms<-sms.dtm[,sms.freq.words]
dim(sms)
View(as.matrix(sms[1:10,1:10]))

sms<-as.matrix(sms)
sms<-as.data.frame(sms)

dim(sms)

sms.name<-colnames(sms)


sms_raw$type[1072]<-factor("ham")

sms$ttype<-sms_raw$type
sms$ttype<-as.factor(sms$ttype)

importance<-function(){
  imp<-data.frame()
  for(i in 1:(ncol(sms)-1)){
    temp<-t.test(sms[,i]~sms$ttype)
    imp<-rbind(imp,cbind(i,temp$p.value))
  }
  return(imp)
}

imp<-importance()
View(imp)

idx<-order(imp$V2)[1:10]
top10<-sms.name[idx]

rst<-data.frame()
for(i in idx){
  tmp<-t.test(sms[,i]~sms$ttype)$estimate[[1]]
  tmp2<-t.test(sms[,i]~sms$ttype)$estimate[[2]]
  rst<-rbind(rst,cbind(sms.name[i],tmp,tmp2))
}

rst
rownames(rst)<-top10
rst<-rst[,-1]
colnames(rst)<-c("ham","spam")
rst