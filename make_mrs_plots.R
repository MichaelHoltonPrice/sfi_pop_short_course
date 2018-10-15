rm(list=ls())

library(demogR)
data(goodman)

## create life table for Venezuela in 1965
vlt <- with(goodman, life.table(x=age, nDx=ven.nDx, nKx=ven.nKx))
mx <- goodman$ven.bx/goodman$ven.nKx
ven <- leslie.matrix(lx=vlt$nLx, mx=mx)

vea <- eigen.analysis(ven)

### plot stable age distribution
age <- seq(0,45,by=5)

lx <- vlt$lx[1:12]
lx <- lx[-2]

discRate <- (-log(lx[-1] / lx[1:(length(lx)-1)]) + log(vea$lambda1))/5

pdf('madagascar_1965_discount_rate.pdf')
plot(age,discRate*100,ylim=c(0,6),xlab='Age [yr]',ylab='Annual Discount Rate (%)')
lines(c(-10,50),c(1,1)*100*log(vea$lambda1)/5,col='grey',xlab='Age [yr]',ylab='Annual Discount Rate (%)')
dev.off()
