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

#source("overdraft-example14.R")
#example 14 - overdraft due to late payment and retainage
#INPUT
i<-0.02
T<-8

#model input

c<-matrix(double(1),nrow=1,ncol=T)

c<-cbind(-45200,-56280,-67300,-35600,-22800,-8700,12450,223430)
F<-matrix(double(1),nrow=1,ncol=T)

for (t in 1:T){
F[t]<-c[t]*FP(i,T-t)
}
TotalF<-sum(F)
cat("value of cashflow for each year \n")
print(c)
cat("value of F for each year \n")
print(F)
cat("Total future value \n")
print(TotalF)

# the END