#this is used to calculate the equipment depreciation by using 3 methods

#global cashflow functions
#define the global functions
# 
#Straightline method
Rm1<-function(N){1/N} #Rm1 is the rate of depreciation followed the straighline method
Dm1<-function(P,F,N){(P-F)/N} #Dm1 is the depreciation at years m
BVm1<-function(P,F,N,m){P-m*(P-F)/N} #BVm1 is the book value at years m

#Sum-of-the-years (SOY) method
SOY<-function(N){N*(N+1)/2}
Rm2<-function(N,m){(N-m+1)/(N*(N+1)/2)}  
Dm2<-function(P,F,N,m){((N-m+1)/(N*(N+1)/2))*(P-F)}
BVm2<-function(P,F,N,m){P-(P-F)*((m*(N-m/2+0.5))/(N*(N+1)/2))}

#Declining balance methods
Rm3<-function(i,N){i/N} # i is the choosen declining factor, N is total year
Dm3<-function(P,i,m,N){(i/N)*P*(1-i/N)^(m-1)}
BVm3<-function(P,i,m,N){P*(1-i/N)^m}

#Example
# equipment is purchased with value of $12'000 and has its life of 5 years, salvage value of $2'000.

T<-5 # total number of year

R<-matrix(double(1),nrow=T,ncol=3)
D<-matrix(double(1),nrow=T,ncol=3)
BV<-matrix(double(1),nrow=T,ncol=3)

P<-12000
F<-2000
i<-2 # mean for double declining depreciation method

for (m in 1:T){
for (n in 1:3){
if (n==1){
R[m,n]<-Rm1(T)
D[m,n]<-Dm1(P,F,T)
BV[m,n]<-BVm1(P,F,T,m)
}
else if (n==2){
R[m,n]<-Rm2(T,m)
D[m,n]<-Dm2(P,F,T,m)
BV[m,n]<-BVm2(P,F,T,m)
}
else {
R[m,n]<-Rm3(i,T)
D[m,n]<-Dm3(P,i,m,T)
BV[m,n]<-BVm3(P,i,m,T)
}
}
}

cat("Results \n")
for (n in 1:3){
print(c("method",n))
print(data.frame(n,R[,n],D[,n],BV[,n]))
}

# the END