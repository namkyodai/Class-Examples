#Prepared by Nam Lethanh
#https://sites.google.com/site/namkyodai/
#this is used to calculate the cash flow for renting construction equipment
#example from the book Construction Planning, Equipment, and Method
#model input
T <-8400 #this is total renting hour or service life of the equipment
P <- 45000 #this is up front investment cost to buy the equipment
w <- 12.34 #this is the money for every hour that the equipment is in used
q <- 3200 #this is the replacement cost for the tire, expect to be every 2800 hours in use
c <- 6000 # this is the major repair cost after 4200 hours
e <- 1400 # expected number of hour per year from the owner
i <-0.15 # interest reate per year
s <-0.1 # percentage of sank cost to the original investment

# QUESTION --> how much will the owner charge per hour of use?

# Start the program

#define the functions
# x and y are interest rate and number of period, respectively
FA <- function(x,y){((1+x)^y-1)/x} #this is function to define F based on A
AF <- function(x,y){x/((1+x)^y-1)} #this is function to define A based on F
PF <- function(x,y){1/((1+x)^y)} #this is function to define P based on F
FP <- function(x,y){(1+x)^y} #this is function to define P based on F
PA <- function(x,y){((1+x)^y-1)/(x*(1+x)^y)} #this is function to define P based on A
AP <- function(x,y){(x*(1+x)^y)/((1+x)^y-1)} #this is function to define A based on P
n <- T/e # this is the number of year

A1 <- -P*AP(i,n) #annual sank cost of equipment, negative sign
A2 <- -w*e # this is the actual routine maintenance cost
A3a <- -q*AF(i,n/3) # this is the annual sank cost fore repacement of tire 
A3b <- -q*(PF(i,2)+PF(i,4)+PF(i,n))*AP(i,n) # this is the annual sank cost fore repacement of tire
# note: A3a is same as A3b, mathematically
A4 <- -c*PF(i,n/2)*AP(i,n) # annual cost spending on major replacement
A5 <- (s*P+q)*AF(i,n) # q is included as in fact in the last year, owner might not need to change the tire
AT <- A1+A2+A3a+A4+A5 #total annual cost

rate <- abs(AT/e) # this is expected hour rate to be charged.

print(A1)
print(A2)
print(A3a)
print(A3b)
print(A4)
print(A5)
print(AT)
print(rate)
# the END
