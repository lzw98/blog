#1 n = 893,y = 400
#CI 93%
library(cdabookdb)
binom.test(400,893,p = 0.5)
#Test pi = 0.5,alternative pi != 0.5





#2 95%CI wald planA, planB
#n = 10, y = 9
binom_inference(0.9,10,0.5,method = 'wald')
binom_inference(0.9,10,0.5,method = 'l')








#n=10,y=0
binom_inference(0,10,0.5,method = 'wald')
binom_inference(0,10,0.5,method = 'l')





#3 small sample exact test
#pi<=0.8 alternateive pi>0.8 n=10,y=9









#pi>=0.3 alternative pi<0.3 n=10,y=0