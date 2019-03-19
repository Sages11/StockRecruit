# notes ----

# Sarah Power
# sarah.power@alaska.gov
# March 2019

#Stock recruit analysis based on Dr. Derek Ogles FSA packages

# load ----

library(FSA)
library(nlstools)
library(plotrix)
library(tidyverse)
#source('code/functions.R')

# data ----
srdata <- read_csv('H:\\sarah\\Projects\\spawner recruit\\UpperStationSockeyeAK2019.csv')
srdata <- srdata %>%    
  mutate(recruits = recruit,
         logR = log(recruit),
         RperS = recruit/stock,
         logRperS = log(RperS)) 

#srdata <- MI7
#ricker1s <- r3s
#r1fit <- r3fit

# analysis ----

#define function
ricker1 <- function(logR, stock, a, b){
  logR ~ log(a*stock) - b*stock
}

#Find starting values
#ricker1 <- logR ~ log(a*stock) - b*stock #
#ricker1 <- srFuns(type = "Ricker", param = 3) 
ricker1s <- srStarts(recruit~stock,data=srdata,type="Ricker", plot = TRUE)
#ra <- ricker1s$a
#rb <- ricker1s$b

#ricker1s <- srStarts(recruit~stock,data=srdata,type="Ricker", plot = TRUE, fixed = list(a=ricker1s$a,b=ricker1s$b, Rp = ricker1s$a/(ricker1s$b*exp(1))))
#\Rp <- ricker1s$a/(ricker1s$b*exp(1))
#Model fit and saved as
r1fit <- nls(logR ~ log(a*stock) - b*stock ,data=srdata,start=ricker1s,algorithm="port",lower=c(0,0))
summary(r1fit)
#density-independance model for cmparistion
r0 <- logR~log(a*stock)
r0s <- ricker1s[1]
r0fit <- nls(r0,data=srdata,start=r0s,algorithm="port",lower=c(0))
anova(r0fit,r1fit)
AIC(r0fit,r1fit)
#A small pvalue and smaller AIC value would suggest that the Ricker model with the density-dependant parapmeter (ricker1) provide a "better fit to the data. 
#

#continue to wrok here on down
#plot of both models:
plot(recruits~stock,data=srdata,pch=19)
curve(ricker1(x,coef(r1fit)[1],coef(r1fit)[2]),from=0,to=2000,col="red",lwd=2,add=TRUE)
curve(coef(r0fit)[1]*x,from=0,to=2000,col="blue",lwd=2,add=TRUE)
legend("topright",legend=c("density independent","density dependent"),col=c("blue","red"),
         lwd=2,cex=0.6)

#parameter estiamtes and summary results
overview(ricker1)

#The 95% bootstrap confidence intervals are obtained and visualized with
bootricker1 <- nlsBoot(r1fit, niter = 2000) #
confint(bootricker1, plot= TRUE)

#Applying the formula provided previously to the a and Rp results from each of the bootstrap samples found
#in the coefboot object of the bootricker1 object. Thus, the Sp value for each bootstrap sample is computed with
Sp <- bootricker1$coefboot[,"Rp"]*exp(1)/bootricker1$coefboot[,"a"]

#The median value and 95% confidence interval for Sp can be found by supplying those results to the quantile
#function as follows
( qSp <- quantile(Sp,c(0.5,0.025,0.975)) )

#Thus, one is 95% condent that the stock level that produces the peak recruitment level is between 63.7
#and 99.6. An interesting plot (Figure ??) of these results, along with the peak level of recruitment results,
#is constructed with
plot(recruits~stock,data=srdata,pch=19,col="gray")
curve(ricker1(x,coef(r1fit)[1],coef(r1fit)[2]),from=0,to=130,lwd=2,add=TRUE)
( cRp <- coef(r1fit)["Rp"] )

plotCI(x=qSp[1],y=cRp,li=qSp[2],ui=qSp[3],err="x",lwd=2,pch=19,col="red",add=TRUE)
plotCI(x=qSp[1],y=cRp,li=confint(bootricker1,parm="Rp")[1],ui=confint(bootricker1,parm="Rp")[2],
         err="y",lwd=2,pch=19,col="red",add=TRUE)

