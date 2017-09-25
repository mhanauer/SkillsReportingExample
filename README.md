---
title: "Example of Report"
output:
  word_document: default
  pdf_document: default
  html_document: default
---
Here is an example of an artificial report for four treatment groups with two outcomes (outcome1, outcome2) measured on a scale ranging from 70 to 100.  I also analyzed how important factors to implementation of treatments such as fidelity (1 = 100% fidelity, 0 = less than 100% fidelity) and whether or not a document was turned in on time (onTimeDoc: 1 = turned in documents on time 0 = did not) over 4 time points (fall, winter, spring, and summer).

Additionally, I included a costs analysis to evaluate how income, costs, and revenue (income - costs) changed on a monthly basis for the implementation of the treatments.
```{r, echo=FALSE, message=FALSE, warning=FALSE}
library(Hmisc)
library(dplyr)
library(lme4)
library(nlme)
library(ggplot2)
library(scales)
library(quantmod)
library(psych)
treatmentSamp = c(1,2,3,4)

set.seed(12345)
treatment = sample(treatmentSamp, 250, replace = TRUE)
treatment = rep(treatment, 4)

datStatic = as.data.frame(cbind(treatment))
datStatic = as.data.frame(apply(datStatic, 2, function(x) rep(x, 4)))

onTimeDocSamp = c(1,0)
fidelitySamp = c(1,0)
set.seed(123456)
outcome1 = as.data.frame(round(c(rnorm(250, 80, 10), rnorm(250, 90, 10), rnorm(250, 100, 10), rnorm(250, 110, 10)), 0))
colnames(outcome1) = c("outcome1")
outcome2 = as.data.frame(round(c(rnorm(250, 75, 10), rnorm(250, 85, 10), rnorm(250, 95, 10), rnorm(250, 105, 10)), 0))
colnames(outcome2) = c("outcome2")
onTimeDoc = sample(onTimeDocSamp, 1000, replace = TRUE, prob = c(.7, .3))
fidelity = sample(fidelitySamp, 1000, replace = TRUE, prob = c(.7, .3))
time = rep(1:4, 250)
datNonStatic = cbind(onTimeDoc, fidelity, outcome1, outcome2, time)
dat = cbind(datStatic, datNonStatic)

set.seed(1234)
costs = round(rnorm(12, 10000, 100),0)
income = round(rnorm(12, 10000, 100),0)
peopleServed = round(rnorm(12,100,5), 0)
revenues= income - costs
time  = 1:12
datMonthly = data.frame(cbind(costs, income, revenues, peopleServed, time))
datMonthly$costsChange = round(Delt(datMonthly$costs),2)
datMonthly$incomeChange = round(Delt(datMonthly$income),2)
colnames(datMonthly) = c("costs","income", "revenues", "peopleServed","time", "costChange", "incomeChange")
```
Below are the income, costs, and revenues over time (i.e. months) graphs.  As the reader can see income is close to costs indicating that costs are generally being covered.
```{r, echo=FALSE, message=FALSE, warning=FALSE}
theme_set(theme_grey(base_size = 13))
p = ggplot(datMonthly, aes(x = time))
p = p+geom_line(aes(y = income, color = "Income"))
p = p+geom_line(aes(y = costs, color = "Costs"))
p = p+labs(y = "Income & Costs")
p = p+ scale_x_continuous(breaks=seq(0,12, 1))
p = p + scale_y_continuous(labels = dollar)
p = p+ggtitle("Income and Costs Over Time"); p

theme_set(theme_grey(base_size = 13))
p = ggplot(datMonthly, aes(x = time))
p = p+geom_line(aes(y = costChange, color = "% Change in Costs"))
p = p+geom_line(aes(y = incomeChange, color = "% Change in Income"))
p = p+labs(y = "% Change in Income and costs")
p = p +expand_limits(y = c(-.05, .05))
p = p+ scale_x_continuous(breaks=seq(0,12, 1))
p = p + scale_y_continuous(labels = percent)
p = p+ggtitle("% Change in Income and Costs Over Time");p

theme_set(theme_grey(base_size = 13))
p = ggplot(datMonthly, aes(x = time))
p = p+geom_line(aes(y = revenues))
p = p+labs(y = "Revenues")
p = p+ scale_x_continuous(breaks=seq(0,12, 1))
p = p +expand_limits(y = c(-400, 400))
p = p + scale_y_continuous(labels = scales::dollar)
p = p+ggtitle("Change in Revenue Over Time"); p

```
The graphs below are displaying the average scores for outcomes one and two, fidelity (percentage of 100% fidelity), and percentage of times documents were delivered on time per treatment over time.  Overall, the average scores over all of the treatments do not seem to change over time, indicating that treatments are not having the desired effect.  Both fidelity and percentage of documents turned in on time for all treatments over time are around 70% indicating room for improvement.
```{r, message=FALSE, warning=FALSE, echo=FALSE}
datTimeAgg = round(aggregate(dat, list(dat$time), mean),2)
datTimeAgg$outcome1Change = Delt(datTimeAgg$outcome1)
datTimeAgg$outcome2Change = Delt(datTimeAgg$outcome2)
theme_set(theme_grey(base_size = 13))
p = ggplot(datTimeAgg, aes(x = time))
p = p+geom_line(aes(y = outcome1, color = "Outcome 1"))
p = p+geom_line(aes(y = outcome2, color = "Outcome 2"))
p = p+labs(y = "Outcomes 1 & 2")
p = p +expand_limits(y = c(70, 100))
p = p+ggtitle("Outcomes 1 and 2 Over Time");p
```

```{r, message=FALSE, warning=FALSE, echo=FALSE}
theme_set(theme_grey(base_size = 13))
p = ggplot(datTimeAgg, aes(x = time))
p = p+geom_line(aes(y = outcome1Change, color = "% Change in Outcome 1"))
p = p+geom_line(aes(y = outcome2Change, color = "% Change in Outcome 2"))
p = p+labs(y = "% Change in Outcomes 1 & 2")
p = p +expand_limits(y = c(-.05, .05))
scale_x_continuous(limits = c(2, 4))
p = p+ scale_x_continuous(breaks=seq(2,4, 1))
p = p + scale_y_continuous(labels = percent)
p = p+ggtitle("% Change in Outcomes 1 and 2 Over Time");p
```

```{r, message=FALSE, warning=FALSE, echo=FALSE}
#Outcome 1 over time 
datTimeAgg = round(aggregate(dat, list(dat$time, dat$treatment), mean),2)
datTimeAgg$treatment = as.factor(datTimeAgg$treatment)
theme_set(theme_grey(base_size = 13))
p = ggplot(datTimeAgg, aes(x = time))
p = p+geom_line(aes(y = outcome1, group = treatment))
p = p + geom_line(aes(y = outcome1,shape=treatment, color=treatment))
p = p+labs(y = "Outcome 1")
p = p +expand_limits(y = c(70, 100))
p = p+ggtitle("Outcomes 1 and 2 by Treatment over Time");p
```

```{r, message=FALSE, warning=FALSE, echo=FALSE}
#Outcome 1 over time 
theme_set(theme_grey(base_size = 13))
p = ggplot(datTimeAgg, aes(x = time))
p = p+geom_line(aes(y = fidelity, group = treatment))
p = p + geom_line(aes(y = fidelity,shape=treatment, color=treatment))
p = p+labs(y = "% Fidelity")
p = p +expand_limits(y = c(.25, 1))
p = p + scale_y_continuous(labels = percent)
p = p+ggtitle("% Fidelity by Treatment over Time");p
```
```{r, message=FALSE, warning=FALSE, echo=FALSE}
theme_set(theme_grey(base_size = 13))
p = ggplot(datTimeAgg, aes(x = time))
p = p+geom_line(aes(y = onTimeDoc, group = treatment))
p = p + geom_line(aes(y = onTimeDoc,shape=treatment, color=treatment))
p = p+labs(y = "% onTimeDoc")
p = p +expand_limits(y = c(.25, 1))
p = p + scale_y_continuous(labels = percent)
p = p+ggtitle("% OnTimeDoc by Treatment over Time");p
```
