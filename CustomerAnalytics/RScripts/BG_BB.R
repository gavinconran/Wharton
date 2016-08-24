#### The BG/BB model is also used for non-contractual settings. In many regards,
#### it is very similar to the Pareto/NBD model—it also uses four parameters to
#### describe a purchasing process and a dropout process. The difference between the
#### models is that the BG/BB is used to describe situations in which customers have
#### discrete transaction opportunities, rather than being able to make transactions at any time.

library(BTYD)

### STEP 1: data preparation
simElog<- system.file("data/discreteSimElog.csv", package="BTYD")
elog<-dc.ReadLines(simElog,cust.idx=1,date.idx= 2)
elog[1: 3,]

elog$date<- as.Date(elog$date,"%Y-%m-%d")
max(elog$date);
min(elog$date);

# let's make the calibration period end somewhere in-between
T.cal<-  as.Date("1977-01-01")
simData<-dc.ElogToCbsCbt(elog,per="year", T.cal)
cal.cbs<- simData$cal$cbs

freq<-cal.cbs[, "x"]
rec<-cal.cbs[,"t.x"]
trans.opp<- 7
# transaction opportunities
cal.rf.matrix<- dc.MakeRFmatrixCal(freq, rec, trans.opp)
cal.rf.matrix[1:5,]

### STEP 2: Parameter Estimation
### Estimating BG/BB parameters is very similar to estimating Pareto/NBD parameters
data(donationsSummary);
rf.matrix<-donationsSummary$rf.matrix
params<-bgbb.EstimateParameters(rf.matrix);
LL<-bgbb.rf.matrix.LL(params, rf.matrix);
p.matrix<-c(params, LL);
for(i in 1: 2) {
  params<-bgbb.EstimateParameters(rf.matrix, params);
  LL<-bgbb.rf.matrix.LL(params, rf.matrix);
  p.matrix.row<-c  (params, LL);
  p.matrix<-rbind(p.matrix, p.matrix.row);
}

colnames(p.matrix)<-c("alpha","beta","gamma","delta","LL");
rownames(p.matrix)<-1:3;
p.matrix;


### The parameter estimation converges very quickly. It is much easier,and faster,
### to estimate BG/BB parameters than it is to estimate Pareto/NBD parameters,because there are fewer calculations involved.

### We can interpret these parameters by plotting the mixing distributions. 
### Alpha and beta describe the beta mixing distribution of the beta-Bernoulli transaction
### process. We can see the beta distribution by using
bgbb.PlotTransactionRateHeterogeneity(params)

### Gamma and Delta describe the beta mixing distribution of the beta-geometric
### dropout process. We can see the beta distribution with parameters gamma and delta using
#bgbb.PlotDropoutHeterogeneity(params) # ERROR!!!!

### The story told by these plots describes the type of customers most firms would
### want—their transaction parameters are more likely to be high, and their dropout parameters are more likely to be low.

### Step 3: Individual Level Estimations
### We can estimate the number of transactions we expect a newly acquired
### customer to make in a given time period, just as with the Pareto/NBD model
bgbb.Expectation(params,n=10);

## But we want to be able to say something about our existing customers, not
## just about a hypothetical customer to be acquired in the future
# customer A
n.cal=6
n.star=10
x=0
t.x=0
bgbb.ConditionalExpectedTransactions(params, n.cal,
                                     n.star, x, t.x)

# customer B
x=4
t.x=5
bgbb.ConditionalExpectedTransactions(params, n.cal,
                                     n.star, x, t.x)

### As expected, B’s conditional expectation is much higher than A’s. 
### The point I am trying to make, however, is that there are 3464 A’s in this dataset 
### and only 284 B’s—you should never ignore the zeroes in these models

### Step 4: Plotting/ Goodness-of-fit
## the first plot to test the goodness-of-fit: a simple calibration period
# histogram.bgbb.PlotFrequencyInCalibration(params, rf.matrix) ## Returns an error

## The next step is to see how well the model performs in the holdout period
holdout.cbs<-simData$holdout$cbs
x.star<-holdout.cbs[, "x.star"]

n.star<-5 # length of the holdout period
x.star<-donationsSummary$x.star
comp<-bgbb.PlotFreqVsConditionalExpectedFrequency(params, n.star,
                                                  rf.matrix, x.star)

rownames
(comp)<-c("act","exp","bin")
comp

## Since the BG/BB model uses discrete data, we can also bin customers by recency.
comp<-bgbb.PlotRecVsConditionalExpectedFrequency(params, n.star,
                                                 rf.matrix, x.star)
rownames(comp)<-c("act","exp","bin")
comp

### Another plotting function provided by the BTYD package is the tracking
### function—binning the data according to which time period transactions occurred in.
inc.track.data<-donationsSummary$annual.trans
n.cal<-6
xtickmarks<-1996:2006
inc.tracking<-bgbb.PlotTrackingInc(params, rf.matrix,inc.track.data, xticklab = xtickmarks)

rownames(inc.tracking)<-c("act","exp")
inc.tracking

###  smooth it out for a cleaner graph by making it cumulative
cum.track.data<-cumsum(inc.track.data)
cum.tracking<-bgbb.PlotTrackingCum(params, rf.matrix, cum.track.data,xticklab= xtickmarks)

rownames(cum.tracking)<-c("act","exp")
cum.tracking


## END
