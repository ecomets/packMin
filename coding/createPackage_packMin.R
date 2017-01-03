library(roxygen2)
library(devtools)

minDir<-"/home/eco/work/npde/packMin"
setwd(minDir)
system("rm -r packMin")

fileList<-dir(file.path(minDir,"R"))

# Package skeleton and DESCRIPTION file
package.skeleton(name="packMin", code_files=file.path(minDir,"R",fileList))
system(paste("cp",file.path(minDir,"keepFiles","DESCRIPTION"),file.path(minDir,"packMin")))

# Data folder and files
system(paste("cp -r",file.path(minDir,"data"),file.path(minDir,"packMin")))

# Documentation
setwd(file.path(minDir,"packMin"))
document()
removeRd<-c(
  paste(c("showall"),c("default.Rd","NpdeData.Rd"),sep="."),
  paste(c("npde.qqplot","initialize","show","z[","z[_-"),"methods.Rd",sep="-"))
for(i in removeRd) file.remove(file.path(minDir,"packMin","man",i))

# Test files
devtools::use_testthat()
system(paste("cp",file.path(minDir,"testsDev","*"),file.path(minDir,"packMin","tests","testthat")))

# Building package
setwd(file.path(minDir))
file.remove(file.path(minDir,"packMin","Read-and-delete-me"))
system("R CMD build packMin")

# Testing package with stringent 
system("R CMD check --as-cran packMin_1.0.tar.gz")

# Local install, test development

if(FALSE) {
  dev_mode()
  install.packages("packMin_1.0.tar.gz",repos=NULL)
  # Help files
  ?NpdeData
  ?show
  ?showall
  ?print
  ?print.NpdeData
  
  # Access functions
  showMethods("show")
  selectMethod("show","ANY")
  selectMethod("show","numeric")
  showDefault
  selectMethod("show","NpdeData")
  
  # Data
  data(theopp)
  print(head(theopp))
  
  # Using the package - see test files in tests/testthat
  
  dev_mode()
}

if(FALSE) {
  install_github(username="ecomets", repo="ecomets/packMin")
  
}
