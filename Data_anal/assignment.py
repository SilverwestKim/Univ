import requests
from bs4 import BeautifulSoup
import re as rere
import time
import pandas as pd

re = requests.get('https://movie.naver.com/movie/bi/mi/pointWriteFormList.nhn?code=167638&type=after&isActualPointWriteExecute=false&isMileageSubscriptionAlready=false&isMileageSubscriptionReject=false&page=1')
soup = BeautifulSoup(re.text, 'html.parser')
#총 몇 건인지 확인하고 페이지 수 맞추기
howmany = soup.find('strong',{'class':'total'}).findAll('em')[1].get_text()
print(howmany)
#10개가 한 페이지에 존재하므로, 919개의 페이지를 크롤링한다.

suburl = 'https://movie.naver.com/movie/bi/mi/pointWriteFormList.nhn?code=167638&type=after&isActualPointWriteExecute=false&isMileageSubscriptionAlready=false&isMileageSubscriptionReject=false&page='
url = []
for i in range(1,920):
    url.append(suburl+str(i))

ID = []
date = []
score = []
text = []

for link in url:
    re = requests.get(link)
    soup = BeautifulSoup(re.text, 'html.parser')
    source = soup.findAll('div',{'class':'score_reple'})

    ##ID
    #닉네임 / 관람객 태그 제거 후, 아이디만 남기기
    for i in range(10):
        if len(source[i].findAll('span'))==2:
            x = source[i].findAll('span')[1].get_text()
        else:
            x = source[i].find('span').get_text()
        m = rere.search('[a-zA-Z0-9_-]{4}[\*]{4}',x).group()
        ID.append(m)
    ##날짜 / 시간
    source = soup.findAll('div',{'class':'score_reple'})
    for k in range(0,10):
        d = source[k].findAll('em')
        date.append(d[1].get_text())

    ##평점 리스트
    star = soup.findAll('div',{'class':'star_score'})
    for i in range(1,11):
        u = star[i].get_text(); u = rere.sub('\n','',u);
        score.append(u)
    #score = score[1:]
    #score

    ##리뷰 리스트
    for i in range(0,10):
        t = source[i].find('p')
        text.append(t.get_text())


rst = {'ID': ID,
        'score': score,
        'date' : date,
        'review': text}
data = pd.DataFrame(rst)
data
