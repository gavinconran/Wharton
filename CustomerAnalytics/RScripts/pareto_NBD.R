#### Pareto / NBD
### The Pareto/NBD model is used for non-contractual situations in which customers
### can make purchases at any time. Using four parameters, it describes the rate
### at which customers make purchases and the rate at which they drop out—
### allowing for heterogeneity in both regards, of course. We will walk through the
###Pareto/NBD functionality provided by the BTYD package using the CDNOW1dataset. 


library(BTYD)

### STEP 1: data preparation
## Step 1A: create an event log from the file “cdnowElog.csv”, which has customer IDs in the
## second column, dates in the third column and sales numbers in the fifth column
cdnowElog <- system.file("data/cdnowElog.csv", package = "BTYD")
elog <- dc.ReadLines(cdnowElog, cust.idx = 2, date.idx = 3, sales.idx = 5)
elog[1:3,]

## Step 1B: we convert the dates in the event log to R Date objects
elog$date <- as.Date(elog$date, "%Y%m%d")
elog[1:3,]

## Step 1C: The dc.MergeTransactionsOnSameDate function returns an event log
## with only one transaction per customer per day, with the total sum of 
## their spending for that day as the sales number.
elog <- dc.MergeTransactionsOnSameDate(elog)
elog[1:3,]

## Step 1D: we need to divide the data up into a
## calibration period and a holdout period
## 30 September 1997 as the cutoff date, 
## as this point (39 weeks)divides the dataset in half
end.of.cal.period <- as.Date("1997-09-30")
elog.cal <- elog[which(elog$date <= end.of.cal.period), ]

## Step 1E:  we use dc.SplitUpElogForRepeatTrans, which returns a filtered event
## log ($repeat.trans.elog) as well as saving important information about 
## each customer ($cust.data)
split.data <- dc.SplitUpElogForRepeatTrans(elog.cal);
clean.elog <- split.data$repeat.trans.elog

## Step 1F: create a customer-by-time matrix
## This is simply a matrix with a row for each customer and a column for each date
freq.cbt <- dc.CreateFreqCBT(clean.elog);
freq.cbt[1:3,1:5]

## Step 1G: we create a customer-by-timematrix using all transactions, 
## and then merge the filtered CBT with this total
## CBT (using data from the filtered CBT and customer IDs from the total CBT)
tot.cbt <-dc.CreateFreqCBT(elog)
cal.cbt<-dc.MergeCustomers(tot.cbt, freq.cbt)

## Step 1H:  we can finally create the customer-by-sufficient-statistic matrix described earlier
birth.periods<-split.data$cust.data$birth.per
last.dates<-split.data$cust.data$last.date
cal.cbs.dates<-data.frame(birth.periods, last.dates,end.of.cal.period)
cal.cbs<-dc.BuildCBSFromCBTAndDates(cal.cbt, cal.cbs.dates,per="week")

### Step 2: Model Parameter Estimation
params<-pnbd.EstimateParameters(cal.cbs);
params

LL<-pnbd.cbs.LL(params, cal.cbs);
LL

## As with any optimization, we should not be satisfied with the first output
## weget. Let’s run it a couple more times, with its own output as a starting
## point, to see if it converges:
p.matrix<-c(params, LL);
for (i in 1:2) {
  params <- pnbd.EstimateParameters (cal.cbs, params);
  LL <- pnbd.cbs.LL (params, cal.cbs);
  p.matrix.row <- c(params, LL);
  p.matrix <- rbind(p.matrix, p.matrix.row);
}
colnames(p.matrix) <- c("r", "alpha", "s", "beta", "LL");
rownames(p.matrix) <- 1:3;
p.matrix

## Now that we have the parameters, the BTYD package provides functions to
## interpret them

## As we know, r and alpha describe the gamma mixing distribution
## of the NBD transaction process. We can see this gamma distribution in figure2, plotted using
## pnbd.PlotTransactionRateHeterogeneity(params)
pnbd.PlotTransactionRateHeterogeneity(params)

## We also know that s and beta describe the gamma mixing distribution of the Pareto 
##(or gamma exponential) dropout process. We can see this gamma distribution
## in figure 3, plotted using pnbd.PlotDropoutHeterogeneity(params)
#pnbd.PlotDropoutHeterogeneity(params)  ## Returns an error!!!!

### Step 3: Individual Level Estimations
## Now that we have parameters for the population, we can make estimations for
## customers on the individual level

## First, we can estimate the number of transactions we expect a newly acquired
## customer to make in a given time period. Let’s say, for example, that we are
## interested in the number of repeat transactions a newly acquired customer will
## make in a time period of one year.
pnbd.Expectation(params,t=52);

## We can also obtain expected characteristics for a specific customer, 
## conditional on their purchasing behavior during the calibration period. 
## The first of these is pnbd.ConditionalExpectedTransactions, 
## which gives the number of transactions we expect a customer to make in the holdout period. 
cal.cbs["1516",]
x<- cal.cbs["1516", "x"]
t.x <- cal.cbs["1516", "t.x"]
T.cal <-cal.cbs["1516", "T.cal"]
pnbd.ConditionalExpectedTransactions(params, T.star = 52, x, t.x, T.cal)


## The second is pnbd.PAlive, 
## which gives the probability that a customer is still alive at the
## end of the calibration period. 
pnbd.PAlive(params, x, t.x, T.cal)
## As above, the time periods used depend on which time period was used to estimate the parameters

## There is one more point to note here—using the conditional expectationfunction, 
## we can see the “increasing frequency paradox” in action:
for (i in seq( 10, 25, 5)) {
  cond.expectation <- pnbd.ConditionalExpectedTransactions(params, T.star = 52,  
                                                           x = i, t.x = 20,  T.cal =39)
  cat("x:" ,i," \t Expectation:", cond.expectation, fill = TRUE )
}

### Step 4: Plotting/ Goodness-of-fit
## We would like to be able to do more than make inferences about individual customers. 
## The BTYD package provides functions to plot expected customer behavior against 
## actual customer behaviour in the both the calibration and holdout periods

## The first such function is the obvious starting point: a comparison of actual
## and expected frequencies within the calibration period generated using the following code:
pnbd.PlotFrequencyInCalibration(params, cal.cbs,7)

## We need to verify that the fit of the model holds into the holdout period
elog<-  dc.SplitUpElogForRepeatTrans(elog)$repeat.trans.elog;
x.star<-rep(0,nrow(cal.cbs));
cal.cbs<-cbind(cal.cbs, x.star);
elog.custs<-elog$cust;

for(i in 1 : nrow (cal.cbs)) {
  current.cust  <- rownames(cal.cbs)[i]
  tot.cust.trans <- length( which(elog.custs == current.cust))
  cal.trans <- cal.cbs[i, "x"]
  cal.cbs[i, "x.star"]  <- tot.cust.trans - cal.trans
}
cal.cbs[1 : 3,]

## Now we can see how well our model does in the holdout period
T.star<-  39 # length of the holdout period
censor<-7 # This censor serves the same purpose described above
x.star<-cal.cbs[, "x.star"]
comp <- pnbd.PlotFreqVsConditionalExpectedFrequency(params, T.star, cal.cbs, x.star, censor)

rownames(comp)<-c("act","exp","bin")
comp

## how well does our model predict how many transactions will occur in each week?
tot.cbt<-  dc.CreateFreqCBT(elog)
d.track.data<-rep(0,7*78)
origin<-as.Date("1997-01-01")

for(i in colnames(tot.cbt)) {
  date.index  <-  difftime( as.Date(i), origin) + 1;
  d.track.data[date.index]<-sum(tot.cbt[,i]);
}

w.track.data<-rep(0,78)
for(j in 1 : 78){  
  w.track.data[j] <-sum(d.track.data[(j * 7 -6) : (j * 7)])
}

## Now, we can make a plot comparing the actual number of transactions
## to the expected number of transactions on a weekly basis
T.cal<- cal.cbs[, "T.cal"]
T.tot<- 78
n.periods.final<- 78
inc.tracking<-pnbd.PlotTrackingInc(params, T.cal,
                                   T.tot, w.track.data,
                                   n.periods.final)

inc.tracking[,20:25]

## We may need to smooth the data out by cumulating it over time
cum.tracking.data<- cumsum(w.track.data)
cum.tracking<-pnbd.PlotTrackingCum(params, T.cal,
                                   T.tot, cum.tracking.data,
                                   n.periods.final)

cum.tracking[, 20:25]

## END
  