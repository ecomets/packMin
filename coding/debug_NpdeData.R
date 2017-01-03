library(testthat)

minDir<-"/home/eco/work/npde/packMin"
setwd(file.path(minDir,"testsDev"))

source(file.path(minDir,"R","NpdeData.R"))
source("test-NpdeData-class.R")

source(file.path(minDir,"R","qqplot.R"))
par(mfrow=c(3,3))
source("test-npde.qqplot.R")


#####################################################
# Additional debugging

# Figuring out do.call
if(FALSE) {
  xpl<-rnorm(50)
  sx <- sort(xpl)
  lenx <- length(sx)
  sy<-c(1:lenx)/(lenx+1)
  add.args<-NULL
  fun.args <- c(list(sy), add.args)
  ply<-do.call("qnorm", fun.args)
  summary(ply)
  
  fun.args <- c(list(sy), mean=1)
  ply<-do.call("qnorm", fun.args)
  summary(ply)
  
  add.args<-list(mean=1,sd=2)
  fun.args <- c(list(sy), add.args)
  ply<-do.call("qnorm", fun.args)
  summary(ply)
  
  add.args<-list(0,10)
  fun.args <- c(list(sy), add.args)
  ply<-do.call("qunif", fun.args)
  summary(ply)
}
