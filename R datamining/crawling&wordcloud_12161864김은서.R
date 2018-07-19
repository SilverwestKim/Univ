#현재 영화 예매 순위 상위랭크 3개의 영화(7년의 밤, 레디 플레이어 원, 곤지암)의 리뷰를 다음 영화 페이지에서 크롤링하여 리뷰를 워드클라우드로 시각화하는 과정을 진행하겠습니다. 

rm(list=ls())


setwd("C:/Users/eunse/Desktop/Rdatamining")

#install.packages("rJava")
#install.packages("memoise")
#install.packages("KoNLP")
#install.packages("wordcloud")
#install.packages("dplyr")
#install.packages("stringr")
#install.packages("rvest")

library(stringr)
library(memoise)
library(KoNLP)
library(rvest)
library(rJava)
library(dplyr)

useNIADic() #형태소 분석을 하는데 이용할 사전입니다.

######7년의 밤 리뷰 분석하기#####

url_base<-"http://movie.daum.net/moviedb/main?movieId=84354&type=netizen&page="

#all.reviews에 비어 있는 벡터를 저장하고, 크롤링을 시작한다.

all.reviews<-c()

#한 페이지에 10개의 리뷰가 있으므로, 총 500개의 리뷰를 크롤링 할 예정입니다.

for(page in 1:50){
  url<-paste(url_base,page,sep="")
  htxt<-read_html(url)
  com<-html_nodes(htxt,'div') %>%html_nodes('p')
  reviews<-html_text(com)
  reviews<-repair_encoding(reviews,from='utf-8')
  if(length(reviews)==0){break}
  reviews<-str_trim(reviews)
  all.reviews<-c(all.reviews,reviews)
}

write.table(all.reviews,"data.txt")

data<-readLines("data.txt")

#크롤링한 결과를 확인하기.

###1. 텍스트에서 명사만 추출하기.

#특수문자 제거하는 정규표현식
data<-str_replace_all(data,"\\W"," ")

#리뷰에서 명사 추출
noun<-extractNoun(data)

#추출한 명사 list를 문자열 벡터로 반환, 단어별 빈도표 생성
wordcount<-table(unlist(noun))
wordcount<-as.data.frame(wordcount)
#숫자가 같이 크롤링 되어 있으므로 이를 제거해주기 위해 정규표현식을 사용합니다.
wordcount[,1]<-gsub("\\d+",NA,wordcount[,1])
wordcount<-na.omit(wordcount)



#데이터 프레임으로 변환. 
df.word<-as.data.frame(wordcount,stringAsFactors=F)

#변수명 수정
df.word<-rename(df.word,word=Var1,freq=Freq)



###2. 자주 등장한 단어 빈도표 만들기.
#자주 등장한 20개 단어를 나타낸 빈도표를 만들어 보겠습니다. 한 글자로 된 단어는
#의미가 없는 경우가 많기 때문에 nchar()을 이용해 두 글자 이상으로 된 단어만 추출하겠습니다.

#두 글자 이상 단어 추출
df.word<-filter(df.word,nchar(word)>=2)
top.20<-df.word %>% arrange(desc(freq)) %>% head(20)
top.20


#"영화"는 의미없는 단어이지만 1위에 있으므로 제거한다.
i<-which(df.word$word=="영화")
df.word<-df.word[-i,]

#다시 상위 20개를 확인해본다.
top.20<-df.word %>% arrange(desc(freq)) %>% head(20)
top.20

###wordcloud 만들기
#install.packages("wordcloud")
#install.packages("RColorBrewer)

library(wordcloud)
library(RColorBrewer)

#단어 색상 목록 만들기
pal<-brewer.pal(8,"Dark2")

#난수 고정하기
set.seed(100)

###wordcloud 만들기
#df.word는 단어와 단어가 사용된 빈도, 두 변수로 구성된 data.frame 형태입니다.

wordcloud(words = df.word$word, #단어
          freq = df.word$freq,  #빈도
          min.freq = 2,         #최소 단어 빈도
          max.words = 200,      #표현 단어 수
          random.order = F,     #고빈도 단어 중앙 배치
          rot.per = .1,         #회전 단어 비율
          scale = c(4,0.3),     #단어 크기 범위
          colors = pal)         #색상 목록

#표현 단어수를 300으로 늘리고, 다른 색으로도 시각화해 보겠습니다.
pal<-brewer.pal(9,"Blues")[5:9]

wordcloud(words = df.word$word, #단어
          freq = df.word$freq,  #빈도
          min.freq = 2,         #최소 단어 빈도
          max.words = 300,      #표현 단어 수
          random.order = F,     #고빈도 단어 중앙 배치
          rot.per = .1,         #회전 단어 비율
          scale = c(4,0.3),     #단어 크기 범위
          colors = pal)         #색상 목록

#####레디 플레이어 원 리뷰 분석하기#####

url_base<-"http://movie.daum.net/moviedb/main?movieId=96030&type=netizen&page="

#all.reviews에 비어 있는 벡터를 저장하고, 크롤링을 시작한다.

all.reviews<-c()

#한 페이지에 10개의 리뷰가 있으므로, 총 500개의 리뷰를 크롤링 할 예정입니다.

for(page in 1:50){
  url<-paste(url_base,page,sep="")
  htxt<-read_html(url)
  com<-html_nodes(htxt,'div') %>%html_nodes('p')
  reviews<-html_text(com)
  reviews<-repair_encoding(reviews,from='utf-8')
  if(length(reviews)==0){break}
  reviews<-str_trim(reviews)
  all.reviews<-c(all.reviews,reviews)
}

all.reviews

write.table(all.reviews,"data2.txt")
data<-readLines("data2.txt")

#크롤링한 결과를 확인하기.

###1. 텍스트에서 명사만 추출하기.

#특수문자 제거하는 정규표현식
data<-str_replace_all(data,"\\W"," ")

#리뷰에서 명사 추출
noun<-extractNoun(data)

#추출한 명사 list를 문자열 벡터로 반환, 단어별 빈도표 생성
wordcount<-table(unlist(noun))
wordcount<-as.data.frame(wordcount)
wordcount[,1]<-gsub("\\d+",NA,wordcount[,1])
wordcount<-na.omit(wordcount)

#데이터 프레임으로 변환. 
df.word<-as.data.frame(wordcount,stringAsFactors=F)

#변수명 수정
df.word<-rename(df.word,word=Var1,freq=Freq)

###2. 자주 등장한 단어 빈도표 만들기.
#자주 등장한 20개 단어를 나타낸 빈도표를 만들어 보겠습니다. 한 글자로 된 단어는
#의미가 없는 경우가 많기 때문에 nchar()을 이용해 두 글자 이상으로 된 단어만 추출하겠습니다.

#두 글자 이상 단어 추출
df.word<-filter(df.word,nchar(word)>=2)
top.20<-df.word %>% arrange(desc(freq)) %>% head(20)
top.20

###wordcloud 만들기

#단어 색상 목록 만들기
pal<-brewer.pal(8,"Dark2")

#난수 고정하기
set.seed(100)

###wordcloud 만들기
#df.word는 단어와 단어가 사용된 빈도, 두 변수로 구성된 data.frame 형태입니다.

wordcloud(words = df.word$word, #단어
          freq = df.word$freq,  #빈도
          min.freq = 2,         #최소 단어 빈도
          max.words = 200,      #표현 단어 수
          random.order = F,     #고빈도 단어 중앙 배치
          rot.per = .1,         #회전 단어 비율
          scale = c(4,0.3),     #단어 크기 범위
          colors = pal)         #색상 목록



#####곤지암 리뷰 분석하기#####
url_base<-"http://movie.daum.net/moviedb/main?movieId=118604&type=netizen&page="

#all.reviews에 비어 있는 벡터를 저장하고, 크롤링을 시작한다.

all.reviews<-c()

#한 페이지에 10개의 리뷰가 있으므로, 총 300개의 리뷰를 크롤링 할 예정입니다.
for(page in 1:30){
  url<-paste(url_base,page,sep="")
  htxt<-read_html(url)
  com<-html_nodes(htxt,'div') %>%html_nodes('p')
  reviews<-html_text(com)
  reviews<-repair_encoding(reviews,from='utf-8')
  if(length(reviews)==0){break}
  reviews<-str_trim(reviews)
  all.reviews<-c(all.reviews,reviews)
}



all.reviews
write.table(all.reviews,"data.txt")
data<-readLines("data.txt")

#크롤링한 결과를 확인하기.

###1. 텍스트에서 명사만 추출하기.
#특수문자 제거하는 정규표현식

data<-str_replace_all(data,"\\W"," ")
#리뷰에서 명사 추출

noun<-extractNoun(data)

#추출한 명사 list를 문자열 벡터로 반환, 단어별 빈도표 생성

wordcount<-table(unlist(noun))
wordcount<-as.data.frame(wordcount)
wordcount[,1]<-gsub("\\d+",NA,wordcount[,1])
wordcount<-na.omit(wordcount)


#데이터 프레임으로 변환. 
df.word<-as.data.frame(wordcount,stringAsFactors=F)

#변수명 수정
df.word<-rename(df.word,word=Var1,freq=Freq)


###2. 자주 등장한 단어 빈도표 만들기.
#자주 등장한 20개 단어를 나타낸 빈도표를 만들어 보겠습니다. 한 글자로 된 단어는
#의미가 없는 경우가 많기 때문에 nchar()을 이용해 두 글자 이상으로 된 단어만 추출하겠습니다.

#두 글자 이상 단어 추출
df.word<-filter(df.word,nchar(word)>=2)
top.20<-df.word %>% arrange(desc(freq)) %>% head(20)
top.20



#"영화"는 의미없는 단어이지만 1위에 있으므로 제거한다.
i<-which(df.word$word=="영화")
df.word<-df.word[-i,]

#다시 상위 20개를 확인해본다.
top.20<-df.word %>% arrange(desc(freq)) %>% head(20)
top.20

###wordcloud 만들기
#단어 색상 목록 만들기
pal<-brewer.pal(8,"Dark2")

#난수 고정하기
set.seed(100)

###wordcloud 만들기

#df.word는 단어와 단어가 사용된 빈도, 두 변수로 구성된 data.frame 형태입니다.
wordcloud(words = df.word$word, #단어
          freq = df.word$freq,  #빈도
          min.freq = 2,         #최소 단어 빈도
          max.words = 200,      #표현 단어 수
          random.order = F,     #고빈도 단어 중앙 배치
          rot.per = .1,         #회전 단어 비율
          scale = c(4,0.3),     #단어 크기 범위
          colors = pal)         #색상 목록
