#this is used to calculate the cash flow for renting construction equipment

#load define directory
#getwd()
#setwd("C:/Users/nlethanh/Documents/R")

#global cashflow functions
#define the functions
# x and y are interest rate and number of period, respectively
FA <- function(x,y){((1+x)^y-1)/x} #this is function to define F based on A
AF <- function(x,y){x/((1+x)^y-1)} #this is function to define A based on F
PF <- function(x,y){1/((1+x)^y)} #this is function to define P based on F
FP <- function(x,y){(1+x)^y} #this is function to define P based on F
PA <- function(x,y){((1+x)^y-1)/(x*(1+x)^y)} #this is function to define P based on A
AP <- function(x,y){(x*(1+x)^y)/((1+x)^y-1)} #this is function to define A based on P

# ---------------------------------------------

#source("example15.R")
#example 15 - NPV comparisoin
#INPUT
MARR<-0.2
T<-5

#process
u<-matrix(double(1),nrow=1,ncol=T) #positive cashflow in year t
c<-matrix(double(1),nrow=1,ncol=T) #negative cashflow in year t
A_u <- matrix(double(1),nrow=1,ncol=T)
A_c <- matrix(double(1),nrow=1,ncol=T)
B_u <- matrix(double(1),nrow=1,ncol=T)
B_c <- matrix(double(1),nrow=1,ncol=T)
C_u <- matrix(double(1),nrow=1,ncol=T)
C_c <- matrix(double(1),nrow=1,ncol=T)

NPVa <- matrix(double(1),nrow=1,ncol=T)

NPVb <- matrix(double(1),nrow=1,ncol=T)

NPVc <- matrix(double(1),nrow=1,ncol=T)

# Options are name A, B, C, and the atrributes of options are their prefixes

A_u <- cbind(1000,0,0,0,1200)
A_c <- cbind(7200,0,0,0,0)

B_u <- cbind(0,0,0,0,0)
B_c <- cbind(2250,2250,2250,2250,0)


C_u <- cbind(0,0,0,0,1200)
C_c <- cbind(750,2700,2700,2700,2700)


for (t in 1:T){
if (t==1){
NPVa[t]<-(A_u[t]-A_c[t])*PF(MARR,0)
NPVb[t]<-(B_u[t]-B_c[t])*PF(MARR,0)
NPVc[t]<-(C_u[t]-C_c[t])*PF(MARR,0)
} 
else if (t<T){
NPVa[t]<-(A_u[t]-A_c[t])*PF(MARR,t-1)
NPVb[t]<-(B_u[t]-B_c[t])*PF(MARR,t-1)
NPVc[t]<-(C_u[t]-C_c[t])*PF(MARR,t-1)
} else {
NPVa[t]<-(A_u[t]-A_c[t])*PF(MARR,t-1)
NPVb[t]<-(B_u[t]-B_c[t])*PF(MARR,t-1)
NPVc[t]<-(C_u[t]-C_c[t])*PF(MARR,t-1)
}
}

cat("NPV of option A \n")
print(sum(NPVa))
cat("NPV of option B \n")
print(sum(NPVb))
cat("NPV of option C \n")
print(sum(NPVc))





# the END