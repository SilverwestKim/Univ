# 머리색깔과 눈색깔

haireye <- matrix( data=c(
        68,  15,   5,  20,
       119,  54,  29,  84,
        26,  14,  14,  17,
         7,  10,  16,  94),
   nrow=4, ncol=4, byrow=TRUE,
	dimname=list(
		c("black","brunette","red","blond"),
		c("brown","hazel","green","blue")
	)
)

margin.table(haireye)  # 전체합
margin.table(haireye,margin=1) # 행별 합
margin.table(haireye,margin=2) # 열별 합

# 머리색깔 분포
barplot(margin.table(haireye,margin=1))
# 머리색깔에서 brunette이 다른 색깔에 비하여
# 월들히 많고 blond와 black의 도수는 비슷하며
# red가 가장 적게 분포하는 것을 알 수 있다.

# 눈색깔 분포
barplot(margin.table(haireye,margin=2))
# 눈색깔의 경우 brown과 blue의 숫자가 비슷하고
# hazel과 green이 brown과 blue에 비하여 절반 이하
# 로 적고, green은 가장 적은 도수를 가지고 있다.

# 눈색깔 vs. 머리색깔
barplot(haireye)
mosaicplot(haireye,
	main="머리색깔 vs. 눈색깔",
	color=T
)
# 눈색깔이 brown인 경우에 대하여 머리색깔이 black인
# 사람 중 눈색깔이 brown인 사람의 비율이 가장 많은 반면
# 머리색깔이 brunette, red blond에서는 눈색깔이 brown인 
# 사람의 비율이 줄어든다.
# 눈색깔이 blue인 경우에 대하여 머리색깔이 blond에서는
# 다수의 사람이 눈색깔이 blue인 반면, 머리색깔이 black,
# brunette, red에서는 눈색깔이 blue인 사람의 비율이 적다.
# 전반적으로 머리색깔에 따라 눈색깔의 비율이 
# 달라지는 것을 알 수 있으므로 머리색깔과 눈색깔은
# 연관이 있다고 보인다.


# 분할표

install.packages("gmodels")
library(gmodels)

CrossTable(haireye,prop.chisq=T,prop.t=F,expected=T)

# 분할표에서 눈색깔의 비율은 brown:hazel:green:blue=
# 0.372:0.157:0.108:0.363이다. 반면, 각 머리색깔에서 
# 눈색깔의 비율은 전체 눈색깔의 비율과 다른 경향을 보이고 있다.
# 각 행에서 expected, chi-squre 값, 행에서의 눈색깔 비율 
# 등을 통하여 각 머리색깔에서 눈색깔의 경향의 어떤지를 알 수 
# 있다.
# 머리색깔이 black인 경우 눈색깔이 brown인 사람의 비율이 높고 
# 눈색깔이 blue인 사람의 비율이 낮은 것을 알 수 있다.
# 머리색깔이 brunette인 경우 눈색깔이 blue인 사람의 비율이 
# 약간 낮은 경향을 보이나 많이 낮지는 않다.
# 머리색깔이 red인 경우에는 눈색깔이 green인 사람의 비율이
# 약간 높아 보이나 전체적으로 비율과 크게 차이나지 않아
# 보인다.
# 머리색깔이 blond인 경우 눈색깔이 blue인 비율이 상당히 높고
# 눈색깔이 brown인 비율이 상당히 낮아 전체적인 비율과 다른 
# 경향을 보이는 것을 알 수 있다.
# 전체적으로 머리색깔에 따라서 눈색깔이 다른 것은 머리색깔이 
# black인 경우와 blond인 경우가 전체적인 비율과 다른 것에서
# 기인한 것으로 보인다.

# 머리색깔이 brunette과 red인 경우에 눈색깔의 차이가 있는지는
# 아래의 명령문으로 확인해볼 수 있다.

CrossTable(haireye[c(2,3),],prop.chisq=T,prop.t=F,expected=T)

# 각 행에서의 눈색깔의 비율, 기댓값(expected), Chi-squre 값
# 등을 보면
# 		눈색깔의 비율 : 전체와 큰 차이가 없다.
# 		기댓값 : 도수와 큰 차이가 없다.
# 		chi-square 값 : 크지 않다. (대부분 4보다 작다)
# 전체적으로 Pearson's Chi-square test에서 p-value가 0.05
# 보다 크므로 머리색깔이 brunette, red 여부에 따라 눈색깔이
# 다르다고 볼 수 있는 증거가 약하다고 할 수 있다.






