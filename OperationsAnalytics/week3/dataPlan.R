# Data plan
dataAllowance = 20
fixedPayment = 160
rateAboveAllowance = 15

# Data statistics
expectedDataUsage = 23
stDevDataUsage = 5

# set seed
set.seed(123)

# sample data & sample statistics
inputRandomVariableU1000 = rnorm(1000, expectedDataUsage, stDevDataUsage)
histinfo<-hist(inputRandomVariableU1000)
histinfo
mean(inputRandomVariableU1000)
sd(inputRandomVariableU1000)

min(inputRandomVariableU1000)
max(inputRandomVariableU1000)

# hitogram of sample data usage
hist(inputRandomVariableU1000, 
     breaks=40, 
     xlab="Usage, U (in GB)", 
     main="Distribution of Data Usage, U (in GB)", 
     col="lightgreen", 
     xlim=c(5,38),  
     ylim=c(0, 100))


# sample payment data and statistics
ouputRandomVariableP1000 = 160 + ifelse(inputRandomVariableU1000 > 20, 15 * (inputRandomVariableU1000 - 20), 0)
histinfo<-hist(ouputRandomVariableP1000)
histinfo
mean(ouputRandomVariableP1000)
sd(ouputRandomVariableP1000)
min(ouputRandomVariableP1000)
max(ouputRandomVariableP1000)

# hitogram of sample payment
hist(ouputRandomVariableP1000, 
     breaks=40, 
     xlab="Payment, P (in $)", 
     main="Distribution of Payment, P (in $)", 
     col="blue", 
     xlim=c(160,450),  
     ylim=c(0, 400))

