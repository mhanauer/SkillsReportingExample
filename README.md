---
title: "Example of Report"
output: html_document
---
Putting together the data
Means for each: treatment, onTimeDoc, fidelity, peopleServed, costs, revenues, outcome1, outcome2, timepoint

Rep what is static four times then we can add the time variable.  Or create four columns named one through four then use the long package.
```{r}
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
outcome2 = as.data.frame(round(c(rnorm(250, 70, 10), rnorm(250, 75, 10), rnorm(250, 75, 10), rnorm(250, 80, 10)), 0))
colnames(outcome2) = c("outcome2")
onTimeDoc = sample(onTimeDocSamp, 1000, replace = TRUE, prob = c(.7, .3)); onTimeDoc
fidelity = sample(fidelitySamp, 1000, replace = TRUE, prob = c(.7, .3)); fidelity
time = rep(1:4, 250); time
datNonStatic = cbind(onTimeDoc, fidelity, outcome1, outcome2, time)
# Combine all data sources
dat = cbind(datStatic, datNonStatic)
head(dat)
```
The following items will be measured on a monthly basis for one year: costs, revenues, peopleServed
```{r, echo=FALSE}
set.seed(12345)
costs = rnorm(12, 10000, 100)
revenues = rnorm(12, 10000, 200)
peopleServed = round(rnorm(12,100,5), 0)
datMonthly = as.data.frame(cbind(costs, revenues, peopleServed)); datMonthly

```
Plots for aggregate
```{r}
library(ggplot2)
ggplot(datMonthly, aes(x = costs)) +
  geom_histogram()
```

