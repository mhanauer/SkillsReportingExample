---
title: "Example of Report"
output: html_document
---
Putting together the data
Means for each: treatment, onTimeDoc, fidelity, peopleServed, costs, income, outcome1, outcome2, expenses, timepoint

Rep what is static four times then we can add the time variable.  Or create four columns named one through four then use the long package.
```{r, echo=FALSE, message=FALSE, warning=FALSE}
library(Hmisc)
library(dplyr)
library(lme4)
library(nlme)
library(ggplot2)
library(scales)
library(quantmod)
library(psych)
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
```{r, echo=FALSE, message=FALSE, warning=FALSE}
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
```{r, echo=FALSE, message=FALSE, warning=FALSE}
set.seed(1234)
costs = round(rnorm(12, 10000, 100),0)
income = round(rnorm(12, 10000, 100),0)
peopleServed = round(rnorm(12,100,5), 0)
revenues= income - costs
time  = 1:12
datMonthly = data.frame(cbind(costs, income, revenues, peopleServed, time)); datMonthly
datMonthly$costsChange = round(Delt(datMonthly$costs),2)
datMonthly$incomeChange = round(Delt(datMonthly$income),2)
colnames(datMonthly) = c("costs","income", "revenues", "peopleServed","time", "costChange", "incomeChange")
```
Graphs
Income and expenses over time with and without percentage changes  (Done)
Revenues over time
Outcomes 1 and 2 over time (Done)
Outcomes 1 and 2 over time % change (Done)
Outcomes 1 and 2 over time by treatments (Done)
OnTimeDocs over time by treatment (percentage)
Fidelity over time by treatment (percentage)
```{r, echo=FALSE, message=FALSE, warning=FALSE}
# Income and Costs Graph
theme_set(theme_grey(base_size = 13))
#labs(title = "Title")
p = ggplot(datMonthly, aes(x = time)); p
p = p+geom_line(aes(y = income, color = "Income")); p
p = p+geom_line(aes(y = costs, color = "Costs")); p
p = p+labs(y = "Income & Costs"); p
p = p+ scale_x_continuous(breaks=seq(0,12, 1)); p
p = p + scale_y_continuous(labels = dollar); p
p = p+ggtitle("Income and Costs Over Time"); p

# New plot for percentage change
theme_set(theme_grey(base_size = 13))
p = ggplot(datMonthly, aes(x = time)); p
p = p+geom_line(aes(y = costChange, color = "% Change in Costs")); p
p = p+geom_line(aes(y = incomeChange, color = "% Change in Income")); p
p = p+labs(y = "% Change in Income and costs"); p
p = p +expand_limits(y = c(-.05, .05)); p
p = p+ scale_x_continuous(breaks=seq(0,12, 1)); p
p = p + scale_y_continuous(labels = percent); p
p = p+ggtitle("% Change in Income and Costs Over Time"); p


# New plot for revenue (not change)
theme_set(theme_grey(base_size = 13))
p = ggplot(datMonthly, aes(x = time)); p
p = p+geom_line(aes(y = revenues)); p
p = p+labs(y = "Revenues"); p
p = p+ scale_x_continuous(breaks=seq(0,12, 1)); p
p = p +expand_limits(y = c(-400, 400)); p
p = p + scale_y_continuous(labels = scales::dollar); p
p = p+ggtitle("Change in Revenue Over Time"); p

```
Now try what you try outcomes over time.  You need to get averages and then get averages for teh different groups that you are interested in.

Let use assume that the scale for the outcome measures is from 70 to 100 
```{r, message=FALSE, warning=FALSE, echo=FALSE}
#datAg = round(aggregate(dat, list(dat$time, dat$treatment), mean),2); datAgg
#datAg
#Testing that aggregation worked
#datTest = subset(dat, time == 1 & treatment == 1)
#datTestMeans = apply(datTest, 2, mean); datTestMeans

### Outcomes over time
datTimeAgg = round(aggregate(dat, list(dat$time), mean),2); datTimeAgg
datTimeAgg$outcome1Change = Delt(datTimeAgg$outcome1)
datTimeAgg$outcome2Change = Delt(datTimeAgg$outcome2)
theme_set(theme_grey(base_size = 13))
p = ggplot(datTimeAgg, aes(x = time)); p
p = p+geom_line(aes(y = outcome1, color = "Outcome 1")); p
p = p+geom_line(aes(y = outcome2, color = "Outcome 2")); p
p = p+labs(y = "Outcomes 1 & 2"); p
p = p +expand_limits(y = c(70, 100)); p
p = p+ggtitle("Outcomes 1 and 2 Over Time"); p


### Outcomes over time percentage change
theme_set(theme_grey(base_size = 13))
p = ggplot(datTimeAgg, aes(x = time)); p
p = p+geom_line(aes(y = outcome1Change, color = "% Change in Outcome 1")); p
p = p+geom_line(aes(y = outcome2Change, color = "% Change in Outcome 2")); p
p = p+labs(y = "% Change in Outcomes 1 & 2"); p
p = p +expand_limits(y = c(-.05, .05)); p
scale_x_continuous(limits = c(2, 4)); p
p = p+ scale_x_continuous(breaks=seq(2,4, 1)); p
p = p + scale_y_continuous(labels = percent); p
p = p+ggtitle("% Change in Outcomes 1 and 2 Over Time"); p

```
Outcomes 1 and 2 over time by treatments 
```{r, message=FALSE, warning=FALSE, echo=FALSE}
#Outcome 1 over time 
datTimeAgg = round(aggregate(dat, list(dat$time, dat$treatment), mean),2); datTimeAgg$treatment = as.factor(datTimeAgg$treatment)
theme_set(theme_grey(base_size = 13))
p = ggplot(datTimeAgg, aes(x = time)); p
p = p+geom_line(aes(y = outcome1, group = treatment));p 
p = p + geom_line(aes(y = outcome1,shape=treatment, color=treatment)); p
p = p+labs(y = "Outcome 1"); 
p = p +expand_limits(y = c(70, 100)); p
p = p+ggtitle("Outcomes 1 and 2 by Treatment over Time"); p
```
Fidelity over time by treatment
```{r, message=FALSE, warning=FALSE, echo=FALSE}
#Outcome 1 over time 
datTimeAgg = round(aggregate(dat, list(dat$time, dat$treatment), mean),2); datTimeAgg$treatment = as.factor(datTimeAgg$treatment)
theme_set(theme_grey(base_size = 13))
p = ggplot(datTimeAgg, aes(x = time)); p
p = p+geom_line(aes(y = fidelity, group = treatment));p 
p = p + geom_line(aes(y = fidelity,shape=treatment, color=treatment)); p
p = p+labs(y = "% Fidelity"); 
p = p +expand_limits(y = c(.25, 1)); p
p = p + scale_y_continuous(labels = percent); p
p = p+ggtitle("% Fidelity by Treatment over Time"); p
```
OnTimeDoc over time by treatment
```{r, message=FALSE, warning=FALSE, echo=FALSE}
datTimeAgg = round(aggregate(dat, list(dat$time, dat$treatment), mean),2); datTimeAgg$treatment = as.factor(datTimeAgg$treatment)
theme_set(theme_grey(base_size = 13))
p = ggplot(datTimeAgg, aes(x = time)); p
p = p+geom_line(aes(y = onTimeDoc, group = treatment));p 
p = p + geom_line(aes(y = onTimeDoc,shape=treatment, color=treatment)); p
p = p+labs(y = "% onTimeDoc"); 
p = p +expand_limits(y = c(.25, 1)); p
p = p + scale_y_continuous(labels = percent); p
p = p+ggtitle("% OnTimeDoc by Treatment over Time"); p
```



