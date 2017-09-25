---
title: "Example of Report"
output: html_document
---
Putting together the data
Means for each: treatment, onTimeDoc, fidelity, peopleServed, costs, income, outcome1, outcome2, expenses, timepoint

Rep what is static four times then we can add the time variable.  Or create four columns named one through four then use the long package.
```{r}
library(Hmisc)
library(dplyr)
library(lme4)
library(nlme)
library(ggplot2)
library(scales)
# Do everything that is static first 
treatmentSamp = c(1,2,3,4)
genderSamp = c(1,0)
ageSamp = c(10:18)

set.seed(12345)
treatment = sample(treatmentSamp, 250, replace = TRUE); treatment
treatment = rep(treatment, 4); treatment
# Need to create four different variables then use the long the variables have to be the same, because treatment is the same

datStatic = as.data.frame(cbind(treatment, gender, age)); datStatic
datStatic = as.data.frame(apply(datStatic, 2, function(x) rep(x, 4)))
dim(datStatic)
```
Now the non static stuff: onTimeDoc, fidelity outcome1, outcome2, time
```{r}
onTimeDocSamp = c(1,0)
fidelitySamp = c(1,0)
set.seed(123456)
outcome1 = as.data.frame(round(c(rnorm(250, 80, 10), rnorm(250, 90, 10), rnorm(250, 100, 10), rnorm(250, 110, 10)), 0))
colnames(outcome1) = c("outcome1")
outcome2 = as.data.frame(round(c(rnorm(250, 75, 10), rnorm(250, 85, 10), rnorm(250, 95, 10), rnorm(250, 105, 10)), 0))
colnames(outcome2) = c("outcome2")
onTimeDoc = sample(onTimeDocSamp, 1000, replace = TRUE, prob = c(.7, .3)); onTimeDoc
fidelity = sample(fidelitySamp, 1000, replace = TRUE, prob = c(.7, .3)); fidelity
time = rep(1:4, 250); time
datNonStatic = cbind(onTimeDoc, fidelity, outcome1, outcome2, time)
# Combine all data sources
dat = cbind(datStatic, datNonStatic)
```
The following items will be measured on a monthly basis for one year: costs, revenues, peopleServed, time (in months)
```{r, echo=FALSE}
set.seed(1234)
costs = round(rnorm(12, 10000, 100),0)
income = round(rnorm(12, 10000, 100),0)
peopleServed = round(rnorm(12,100,5), 0)
revenues= income - costs
time  = 1:12
datMonthly = data.frame(cbind(costs, income, revenues, peopleServed, time)); datMonthly
datMonthly$costsDiff = round(Delt(datMonthly$costs),2)
datMonthly$incomeDiff = round(Delt(datMonthly$income),2)
colnames(datMonthly) = c("costs","income", "revenues", "peopleServed","time", "costChange", "incomeChange")

```
Graphs
Income and expenses over time (Done)
Revenues ()
Outcomes 1 and 2 over time (Done)
Outcomes 1 and 2 over time % change 
Outcomes 1 and 2 over time by treatments 
OnTimeDocs over time (percentage)
OnTimeDocs over treatment (percentage)
Fidelity over time (percentage)
Fiedlity over treatment (percentage)
Fidelity over time by treatment
OnTimeDocs over time by treatment
```{r}
# Income and Expenses Graph
theme_set(theme_grey(base_size = 13))
p = ggplot(datMonthly, aes(x = time)); p
p = p+geom_line(aes(y = income, color = "Income")); p
p = p+geom_line(aes(y = costs, color = "Costs")); p
p = p+labs(y = "Income & Costs"); p
p = p+ scale_x_continuous(breaks=seq(0,12, 1)); p
p = p + scale_y_continuous(labels = dollar); p

# New plot for percentage change
theme_set(theme_grey(base_size = 13))
p = ggplot(datMonthly, aes(x = time)); p
p = p+geom_line(aes(y = costChange, color = "% Change in Costs")); p
p = p+geom_line(aes(y = incomeChange, color = "% Change in Income")); p
p = p+labs(y = "Costs & Expenses"); p
p = p +expand_limits(y = c(-.05, .05)); p
p = p+ scale_x_continuous(breaks=seq(0,12, 1)); p
p = p + scale_y_continuous(labels = percent); p

# New plot difference between the two
diffP = ggplot(datMonthly, aes(x = time)); diffP
diffP = diffP+geom_line(aes(y = revenues, color = "Revenues")); diffP
diffP = diffP+labs(y = ""); diffP
diffP = diffP+ scale_x_continuous(limits = c(1, 12));diffP
diffP = diffP + scale_y_continuous(labels = dollar); diffP
```
Now try what you try outcomes over time.  You need to get averages and then get averages for teh different groups that you are interested in.
```{r}
library(psych)
#datAg = round(aggregate(dat, list(dat$time, dat$treatment), mean),2); datAgg
#datAg
#Testing that aggregation worked
#datTest = subset(dat, time == 1 & treatment == 1)
#datTestMeans = apply(datTest, 2, mean); datTestMeans

### Here we are going to graph by fidelity over time

datTimeAgg = round(aggregate(dat, list(dat$time), mean),2); datTimeAgg
theme_set(theme_grey(base_size = 13))
p = ggplot(datTimeAgg, aes(x = time)); p
p = p+geom_line(aes(y = outcome1, color = "Outcome 1")); p
p = p+geom_line(aes(y = outcome2, color = "Outcome 2")); p
p = p+labs(y = "Outcomes 1 & 2"); p
p = p+ scale_x_continuous(limits = c(1, 12));p
p = p+ scale_x_continuous(breaks = c(1:12)); p


```
Package for percentage change
```{r}
library(quantmod)
Delt(dat$outcome1)
```

