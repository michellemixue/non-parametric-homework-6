
#4
library("np")
library("boot")
library("stats")
het<- c(2276,3724,2723,4020,4011,2035,1540,1300,2240,2467,3700,1501,2907,2898,2783,2870,3263,2780)
lym<- c(2830,5488,2904,5528,4966,3135,2079,1755,3080,2363,5087,2821,5130,4830,4690,3570,3480,3823)
matrix<- cbind(het,lym)
matrix
ma<-as.data.frame(matrix)
fit<- lm(lym~het)
summary(fit)
Betas<- coefficients(fit)
M<- length(Betas)
fitlym <- 289.352+1.271*het
fitlym
anova(fit)

res<- lym-fitlym
N<-1000
BtStrpRes   <- matrix( rep(0,M*N), ncol=M)
for (i in 1:N) {
  BtStrpRes[i,] <-coefficients(lm(unlist(simulate(fit)) ~het, ma))
}
BtStrpResConfInt1 <- quantile(BtStrpRes[,2], c(.025, 0.975))
BtStrpResConfInt1

#7
sam1<-c(13.4,11.8,18.5,13.1,7.1,17.2,19.8,15.8,13.6,14.2)
sam2<-c(16.5,17.3,16.3,16.3,16.7,17.1,17.9,16.6,23.7,19.5)
error1<-sam1-15
error2<-sam2-18
error<-as.data.frame(cbind(error1,error2))
y1<-c(error1,error2)
group1 = rep (c ("error1", "error2"), c (length (error1), length (error2)))
comb1<- data.frame(group1,y1)
comb1

extract<- function(P){
  sampleset = sample(nrow(P), 5, replace=TRUE)
  subA <- P[sampleset, ]
  subB <- P[sampleset, ]
  meanA <- mean(subA$y1)
  meanB <- mean(subB$y1)
  varA<- var(subA$y1)
  varB<- var(subB$y2)
  diff <- meanA-meanB
  vary<-(varA+BarB/10)^(0.5)
  t0<-diff/vary
  return(t0)
}
booterror<- boot(error,extract, R=1000)
boot.ci(booterror, conf = 0.95)



#t-pivot
diferror<-mean(error1)-mean(error2)
error1-mean(error1)
error2-mean(error2)
1.05^2+2.65^2+4.05^2+1.35^2+7.35^2+2.75^2+5.35^2+1.35^2+0.85^2+0.25^2
1.29^2+0.49^2+1.49^2+1.49^2+1.09^2+0.69^2+0.11^2+1.19^2+5.91^2+1.71^2
(119.165+47.289)/18

diferror+2.101*(9.247444*(1/5))^2
diferror-2.101*(9.247444*(1/5))^2

#8
#(a)
#percentile Bootstrap
rural<- c(3,2,1,1,2,1,3,2,2,2,2,5,1,4,1,1,1,1,6,2,2,2,1,1)
urban<- c(1,0,1,1,0,0,1,1,1,8,1,1,1,0,1,1,2)
meanrural<-mean(rural)
meanurban<- mean(urban)
a<- length(rural)
b<- length(urban)
B = 100000
rural.boot = numeric(B)
urban.boot = numeric(B)
# Use a for loop to take the samples
for ( i in 1:B ) {
  rural.boot[i] = mean(sample(rural,size=a,replace=TRUE))
  urban.boot[i] = mean(sample(urban,size=b,replace=TRUE))
}
boot.stat = rural.boot - urban.boot
quantile(boot.stat,c(0.05,0.95))


#bca
install.packages("boot")
library("boot")
y<-c(rural, urban)
group = rep (c ("rural", "urban"), c (length (rural), length (urban)))
comb<- data.frame(group,y)
comb
a<-length(rural)
b<-length(urban)
c<-length(y)
bootfun <- function(data, i){
  d <- data[i,]
  m <- mean(d$y[1:24])-mean(d$y[25:41])
  v <- var(d$y[1:24])/24+var(d$y[25:41])/17
  c(m, v) }
fun.boot <- boot(comb,bootfun,R=999,strata=comb$group)
fun.boot
library("simpleboot")
boot.ci(fun.boot,type=c("bca"))

#(b)
e1<- rural-mean(rural)
e2<- urban-mean(urban)

err1.boot = numeric(B)
err2.boot = numeric(B)
ev1.boot = numeric(B)
ev2.boot = numeric(B)
for ( i in 1:B ) {
  err1.boot[i] = mean(sample(e1,size=a,replace=TRUE))
  err2.boot[i] = mean(sample(e2,size=b,replace=TRUE))
  ev1.boot[i]= var(sample(e1,size=a,replace=TRUE))
  ev2.boot[i]= var(sample(e2,size=b,replace=TRUE))
}
boot.stat1 = (err1.boot - err2.boot)/(23*ev1.boot/24+16*ev2.boot/17)^(0.5)
quantile(boot.stat1,c(0.05,0.95))

#11
#(a)
install.packages("stats")
library(stats)
DH<- c(70,69,65,64,66,65,64,66,60,70,66,66,60,68,65,65,67,64,63,64,65,66,65,64,62,66,66,64,68,62,62,67,69,64)
MH<- c(67,64,62,64,69,70,65,66,63,74,60,66,60,65,65,66,67,63,65,62,61,65,64,63,61,62,70,63,65,60,66,65,69,63)
FH<- c(72,74,71,71,68,71,71,70,66,73,72,67,65,65,71,70,69,74,68,68,71,76,72,64,66,68,72,72,68,71,66,72,71,68)
matrix2<- cbind(DH,MH,FH)
matrix2
mat2<-as.data.frame(matrix2)
mat2
fit2<- lm(DH~MH+FH)
summary(fit2)
#then we get the result that DH=20.6087+0.3906*MH+0.27698*FH
error2<- DH-0.3906*MH-0.27698*FH-20.6087
error2
Betas2<-coefficients(fit2)
fitlym2 <- 220.6087+0.3906*MH+0.27698*FH
set.seed(3244)
xdh<-DH[1:length(DH)]
N <- length(DH)
choices2<- sample(1:N,size=N,replace=T)
choices2
xdh[choices2]
LDH<-length(DH)
B2 = 1000
mstar = NULL
for(draw in 1:B2) {
  mstar = c(mstar,mean(DH[sample(1:LDH,size=LDH,replace=T)]))
}
hist(mstar)
sort(mstar)[25]; sort(mstar)[975]
t.test(DH)


set.seed(3244)
bstar = NULL
for(draw in 1:B2){
  Dstar = mat2[sample(1:N,size=N,replace=T),]
  model = lm(DH ~ MH + FH, data=Dstar)
  bstar = rbind(bstar,coef(model))
}
bstar[1:34,]
vb<- var(bstar)
vb
# Test individual coefficients. H0: betaj=0
se<- sqrt(diag(vb))
Z<- Betas2/se
Z
rbind(Betas2,se,Z)


res=c(DH,MH,FH)
trt=c(rep('DH',length(DH)),rep('MH',length(MH)),rep('FH',length(FH)))
kw_data=data.frame(trt,res)
# Find the observed <i>F</i>-ratio
F=anova(lm(res~trt,kw_data))$F[1]
F

# Pool the data
res=kw_data$res
# Under Hnil, obtain (R=) 5000 values of F
R=5000 
i=1
while(i<R){
  i=i+1
  kw_data$res=sample(res,replace = T)
  F[i]=anova(lm(res~trt,kw_data))$F[1]
}
1-(rank(F)[1]-.5)/length(F)
sum(F>=F[1])/length(F)

#(b)
install.packages("FRB")
library(FRB)

x <- data.matrix(kw_data[1:34,2])
y <- data.matrix(kw_data[35:68,2])
z <- data.matrix(kw_data[69:102,2])
Sres <- FRBmultiregS(DH,MH,FH,R=1600,conf = 0.95)
summary(Sres)


ab<- data.matrix(c(3,7,8,2,2,1))
cd<- data.matrix(c(4,8,9,1,4,6))
ef<- data.matrix(c(8,7,2,4,7,1))
Sres <- FRBmultiregS(ab, cd, ef, R=33,conf = 0.95)
summary(Sres)
