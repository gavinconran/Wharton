# To clean up the memory of your current R session run the following line
rm(list=ls(all=TRUE))

set.seed(1234)

## IDEA
price = 150

## Market Demand
minDemandWeak = 2000
maxDemandWeak = 8000
minDemandStrong = 6000
maxDemandStrong = 14000

# sample data & sample statistics
# for weak demand
inputVariableDemandWeakD10 = runif(10000, minDemandWeak, maxDemandWeak)
inputVariableDemandWeakD10
meanDemandWeak = mean(inputVariableDemandWeakD10)
meanDemandWeak

# hitogram of sample weak demand
hist(inputVariableDemandWeakD10, 
     xlab="Weak Demand, D (in single units)", 
     main="Distribution of Weak Demand, D (in single units)", 
     col="lightgreen")

# for strong demand
inputVariableDemandStrongD10 = runif(10000, minDemandStrong, maxDemandStrong)
inputVariableDemandStrongD10
meanDemandStrong = mean(inputVariableDemandStrongD10)
meanDemandStrong

# hitogram of sample strong demand
hist(inputVariableDemandStrongD10, 
     xlab="Strong Demand, D (in single units)", 
     main="Distribution of Strong Demand, D (in single units)", 
     col="lightgreen")


##Expected Value for Supplier S
SorderQuantityQ = 5000
SfixedCost = 0
SunitCost = 120
SvariableCost = SunitCost * SorderQuantityQ;
SvariableCost

# profit transform function
SProfitTransform <- function(x) SfixedCost - SvariableCost + 
  (price * min(x, SorderQuantityQ))

# Weak demand
SProfitWeakR10 <- sapply(inputVariableDemandWeakD10, SProfitTransform)
SProfitWeakR10
SMeanProfitWeakR10 <- mean(SProfitWeakR10)
SMeanProfitWeakR10
SSdProfitWeakR10 <- sd(SProfitWeakR10)
SSdProfitWeakR10

# hitogram of sample data usage
hist(SProfitWeakR10, 
     xlab="Revenue, R (in Euro)", 
     main="Distribution of Profit, P (in Euro) for Supplier S", 
     col="blue")

# Strong demand
SProfitStrongR10 <- sapply(inputVariableDemandStrongD10, SProfitTransform)
SProfitStrongR10
SMeanProfitStrongR10 <- mean(SProfitStrongR10)
SMeanProfitStrongR10
SSdProfitStrongR10 <- sd(SProfitStrongR10)
SSdProfitStrongR10

# hitogram of sample data usage
hist(SProfitStrongR10, 
     xlab="Revenue, R (in Euro)", 
     main="Distribution of profit, P (in Euro) for supplier S", 
     col="blue")


##Supplier P
PorderQuantityQ = 10000
PfixedCost = 50000
PunitCost = 100
PvariableCost = PunitCost * PorderQuantityQ;
PvariableCost

# profit transform function
PProfitTransform <- function(x) - PfixedCost - (PunitCost * PorderQuantityQ) + 
  (price * min(x, PorderQuantityQ))
PRevenueTransform <- function(x) price * min(x, PorderQuantityQ)

# Weak demand
PRevenueWeakR10 <- sapply(inputVariableDemandWeakD10, PRevenueTransform)
PRevenueWeakR10
PMeanRevenueWeakR10 <- mean(PRevenueWeakR10)
PMeanRevenueWeakR10
PProfitWeakR10 <- sapply(inputVariableDemandWeakD10, PProfitTransform)
PProfitWeakR10
PMeanProfitWeakR10 <- mean(PProfitWeakR10)
PMeanProfitWeakR10
PSdProfitWeakR10 <- sd(PProfitWeakR10)
PSdProfitWeakR10

# hitogram of sample data usage
hist(PProfitWeakR10, 
     xlab="Revenue, R (in Euro)", 
     main="Distribution of Profit, P (in Euro) from supplier P", 
     col="blue")

# Strong demand
PProfitStrongR10 <- sapply(inputVariableDemandStrongD10, PProfitTransform)
PProfitStrongR10
PMeanProfitStrongR10 <- mean(PProfitStrongR10)
PMeanProfitStrongR10
PSdProfitStrongR10 <- sd(PProfitStrongR10)
PSdProfitStrongR10

# hitogram of sample data usage
hist(PProfitStrongR10, 
     xlab="Revenue, R (in Euro)", 
     main="Distribution of Profit, P (in Euro) from supplier P", 
     col="blue")


SMeanProfitWeakR10
SMeanProfitStrongR10

PMeanRevenueWeakR10
PMeanProfitWeakR10
PMeanProfitStrongR10


# Solver
library(lpSolve)
# defining parameters
obj.fun <- c(7,884.09, 
             6,993.19,
             6,052.00,
             7,884.09,
             7,884.09,
             7,884.09,
             7,884.09,
             7,884.09,
             7,884.09,
             6,990.26,
             7,884.09)

constr <- matrix(c(6,993.19,
                   6,052.00,
                   7,884.09,
                   7,884.09,
                   7,884.09,
                   7,884.09,
                   7,884.09,
                   7,884.09,
                   6,990.26,
                   7,884.09,
                   6,993.19,
                   6,052.00,
                   7,884.09,
                   7,884.09,
                   7,884.09,
                   7,884.09,
                   7,884.09,
                   7,884.09,
                   6,990.26,
                   7,884.09), ncol=1, byrow=TRUE)

constr.dir <- c("<=",
                "<=",
                "<=",
                "<=",
                "<=",
                "<=",
                "<=",
                "<=",
                "<=",
                "<=",
                "<=",
                "<=",
                "<=",
                "<=",
                "<=",
                "<=",
                "<=",
                "<=",
                "<=",
                "<=")
                
rhs <- c(6,993.19,
         6,052.00,
         9,115.57,
         8,138.25,
         11,629.08,
         7,884.09,
         9,729.12,
         11,983.58,
         6,990.26,
         9,248.15,
         7,884.09,
         7,884.09,
         7,884.09,
         7,884.09,
         7,884.09,
         7,884.09,
         7,884.09,
         7,884.09,
         7,884.09,
         7,884.09)

# solving model
prod.sol <- lp("max", obj.fun, constr, constr.dir, rhs, compute.sens = TRUE)

# accessing R output
prod.sol$objval # objective function value
prod.sol$solution # decision variables value



































































