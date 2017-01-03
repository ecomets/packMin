##################################################################################

#' Class "NpdeData" representing the structure of the longitudinal data
#' 
#' A longitudinal data structure
#' 
#' @name NpdeData-class
#' @aliases NpdeData NpdeData-class print,NpdeData-method
#' summary,NpdeData-method npde.qqplot,NpdeData-method
#' @docType class
#' @section Objects from the Class: NpdeData objects contain the following slots:
#' 
#' \describe{
#' \item{name.data}{character string giving the name of the dataset}
#' \item{name.group}{character string giving the name of the grouping term (ID)}
#' \item{name.predictor}{character string giving the name of the predictor (X)}
#' \item{name.response}{character string giving the name of the response (Y)}
#' \item{name.cens}{character string giving the name of the censoring indicator}
#' \item{name.mdv}{character string giving the name of the missing data indicator}
#' \item{name.covariates}{vector of character string giving the name(s) of the covariates}
#' \item{name.ipred}{character string giving the name of the individual predictions}
#' \item{units}{(optional) a list with the units for X, Y, and covariates}
#' \item{data}{a dataframe containing the data}
#' \item{N}{number of subjects}
#' \item{ntot.obs}{total number of non-missing observations}
#' \item{nind.obs}{vector of size N giving the number of non-missing observations for each subject}
#' \item{ind}{index of non-missing observations}
#' \item{icens}{index of censored observations (non-missing)}
#' \item{not.miss}{a vector of boolean indicating for each observation whether it is missing (FALSE) or available (TRUE)}
#' \item{loq}{the censoring value}
#' }
#' @section Methods:
#' \describe{
#'   \item{show(npde.data):}{Prints a short summary of object npde.data}
#'   \item{qqplot.npde(npde.data):}{QQ-plot for NpdeData object (TODO: change for NpdeObject in final package)}
#' }
#' @keywords classes
#' @examples
#' 
#' methods(class="NpdeData")
#' 
#' showClass("NpdeData")
#' 
#' @exportClass NpdeData

setClass(
  Class="NpdeData",
  representation=representation(
    name.data="character",	# name of dataset
    name.group="character",	# name of column with ID
    name.predictor="character",# name of column(s) with predictors 
    name.response="character",	# name of column with response
    name.cens="character",	# name of column with censoring information
    name.miss="character",	# name of column indicating missing data (not censored)
    name.covariates="character",# name of column(s) with covariates (can be used for graphs)
    name.ipred="character",	# name of column indicating individual predictions (if available in the dataset)
    units="list",		# units (list with components for x, y, and cov), used in plots
    data="data.frame",          # the data: data frame with columns name.group (subject id), index (id renamed to 1:N), name.predictors (predictors), name.response (possibly transformed during fit), cens (1 if data is censored; the corresponding response is the censoring value), mdv (1 if the data is considered as missing), name.covariates (covariates, binary covariates are modified to 0/1), name.ipred (individual predictions, if available)
    ind="numeric",		# index of the non-missing observations # ECO TODO remove ?
    icens="numeric",		# index of the censored observations (non-missing)
    not.miss="logical",		# vector of logical, TRUE if present (=not missing), FALSE for missing data
    N="numeric",		# number of subjects
    ntot.obs="numeric",		# total number of observations
    nind.obs="numeric",		# number of observations for each subject
    loq="numeric"		# LOQ
  ),
  validity=function(object){
    #    cat ("--- Checking NpdeData object ---\n")
    if (length(object@name.data)==0) {
      stop ("[ NpdeData : validation ] Please provide a name for the data (dataset or datafile on disk).")
    }
    #     if(object@N>0) {
    #       N<-object@N
    #       if(N!=length(unique(object@id)) | N!=length(unique(object@index)) | N!=length(object@nind.obs)) {
    #         cat("Size mismatch: id, index and/or nind.obs no longer correspond to the number of subjects.\n")
    #       }
    #       nobs<-object@ntot.obs
    #       if(nobs!=dim(object@data)[1]) {
    #         cat("Check length of predictor and response.\n")
    #       }
    #     }
    return(TRUE)
  }
)

###############################
# ECO validity ne semble pas etre appele automatiquement quand on cree un objet => il faut l'appeler dans initialize

#' @importFrom methods validObject

setMethod(
  f="initialize",
  signature="NpdeData",
  definition= function (.Object,name.data,name.group,name.predictor, name.response,name.covariates,name.cens,name.miss,name.ipred,units,data){
    #    cat ("--- initialising NpdeData Object --- \n")
    if(missing(name.data) || length(name.data)==0) stop ("Please provide a name for the data (dataset or datafile on disk).")
    .Object@name.data<-name.data
    if(missing(name.group)) name.group<-character()
    # ECO TODO: reconnaissance automatique (avant affectation a la valeur 2) ?
    if(missing(name.predictor)) name.predictor<-character()
    if(missing(name.response)) name.response<-character()
    if(missing(name.covariates) || length(name.covariates)==0 || name.covariates[1]=="") name.covariates<-character()
    if(missing(name.cens) || length(name.cens)==0 || name.cens=="") name.cens<-character()
    if(missing(name.miss) || length(name.miss)==0 ||name.miss=="") name.miss<-character()
    if(missing(name.ipred) || length(name.ipred)==0 ||name.ipred=="") name.ipred<-character()
    .Object@name.group<-name.group
    .Object@name.predictor<-name.predictor
    .Object@name.response<-name.response
    .Object@name.covariates<-name.covariates
    .Object@name.cens<-name.cens
    .Object@name.miss<-name.miss
    .Object@name.ipred<-name.ipred
    if(missing(units)) units<-list(x="-",y="-")
    if(is.null(units$x)) units$x<-"-"
    if(is.null(units$y)) units$y<-"-"
    ncov<-length(name.covariates)
    if(ncov>0) {
      nunit<-length(units$covariates)
      if(nunit==0) units$covariates<-rep("-",ncov)
      if(nunit>ncov) units$covariates<-units$covariates[1:ncov]
      if(nunit<ncov) {
        length(units$covariates)<-ncov
        units$covariates[(nunit+1):ncov]<-"-"
      }
    }
    .Object@units<-units
    .Object@N<-0
    # Object validation
    validObject(.Object)
    return (.Object )
  }
)

##################################################################################

#' Get/set methods for NpdeData object
#' 
#' Access slots of a NpdeData using the object["slot"] format
#' 
#' @name [
#' 
#' @param x	object from which to extract element(s) or in which to replace element(s)
#' @param i,j,...	indices specifying elements to extract or replace. Indices are numeric or character vectors or empty (missing) or NULL
#' @param drop For matrices and arrays. If TRUE the result is coerced to the lowest possible dimension (see the examples). This only works for extracting elements, not for the replacement. See drop for further details
#' @param value typically an array-like R object of a similar class as x
#' 
#' @keywords methods
#' @exportMethod [
#' @exportMethod [<-
#' @aliases [,NpdeData-method
#' @docType methods
#' @rdname extract-methods

#### NpdeData
# Getteur
setMethod(
  f ="[",
  signature = "NpdeData" ,
  definition = function (x,i,j,drop ){
    switch (EXPR=i,
            "name.data"={return(x@name.data)},
            "header"={return(x@header)},
            "sep"={return(x@sep)},
            "na"={return(x@na)},
            "name.group"={return(x@name.group)},
            "name.predictor"={return(x@name.predictor)},
            "name.response"={return(x@name.response)},
            "name.cens"={return(x@name.cens)},
            "name.miss"={return(x@name.miss)},
            "name.covariates"={return(x@name.covariates)},
            "name.ipred"={return(x@name.ipred)},
            "units"={return(x@units)},
            "loq"={return(x@loq)},
            "data"={return(x@data)},
            "ind"={return(x@ind)},
            "icens"={return(x@icens)},
            "not.miss"={return(x@not.miss)},
            "N"={return(x@N)},
            "ntot.obs"={return(x@ntot.obs)},
            "nind.obs"={return(x@nind.obs)},
            stop("No such attribute\n")
    )
  }
)


#' replace names of NpdeData
#'
#' @name [
#' @aliases [<-,NpdeData-method
#' @docType methods
#' @rdname extract-methods

# Setteur
setReplaceMethod(
  f ="[",
  signature = "NpdeData" ,
  definition = function (x,i,j,value){
    switch (EXPR=i,
            "name.data"={x@name.data<-value},
            "name.group"={x@name.group<-value},
            "name.predictor"={x@name.predictor<-value},
            "name.response"={x@name.response<-value},
            "name.cens"={x@name.cens<-value},
            "name.miss"={x@name.miss<-value},
            "name.covariates"={x@name.covariates<-value},
            "name.ipred"={x@name.ipred<-value},
            "units"={x@units<-value},
            "loq"={x@loq<-value},
            "data"={x@data<-value},
            "ind"={x@ind<-value},	
            "icens"={x@icens<-value},	
            "not.miss"={x@not.miss<-value},		
            "N"={x@N<-value},
            "ntot.obs"={x@ntot.obs<-value},
            "nind.obs"={x@nind.obs<-value},
            stop("No such attribute\n")
    )
    validObject(x)
    return(x)
  }
)

##################################################################################
################### S4 methods

#' Shows a NpdeData object
#' 
#' Prints the structure of a NpdeData object
#' 
#' @param object A NpdeData object
#' 
#' @aliases show,NpdeData-method show.NpdeData
#' @docType methods
#' @keywords methods
#' 
#' @importFrom methods show
#' @exportMethod show

setMethod("show","NpdeData",
          # show.NpdeData<-
          function(object) {
            cat("Object of class NpdeData\n")
            if(length(object@ntot.obs)==0) cat("    no data\n") else {
              cat("Dataset",object@name.data,"\n")
              st1<-paste(object@name.response," ~ ",paste(object@name.predictor,collapse=" + ")," | ", object@name.group,sep="")
              cat("    Structured data:",st1,"\n")
              if(length(object@name.covariates)>0) cat("    Covariates:",object@name.covariates,"\n")
              cat("This object has the following components:\n") 
              cat("     data: data\n")
              cat("     with",object@N,"subjects\n")
              cat("     ",object@ntot.obs,"observations\n")
              cat("The data has the following components\n")
              cat("     X:",object@name.predictor,"\n")
              cat("     Y:",object@name.response,"\n")
              if(length(object@name.ipred)>0) cat("     individual model predictions:", object@name.ipred,"\n")
              if(length(object@name.miss)>0) cat("     missing data:",object@name.miss," (1=missing)\n")
              if(length(object@name.cens)>0) cat("     censored data:",object@name.cens," (1=censored)\n")
              if(length(object@loq)>0) cat("      LOQ:    ",object@loq,"\n")
              #      cat("     \n")
            }
          }
)


##################################################################################
################### S3 methods

# print/showall/summary/subset
# alias in class documentation

#' Prints a NpdeData object
#' 
#' prints a NpdeData object
#' 
#' @aliases print.NpdeData
#' @param x A NpdeData object
#' @param nlines number of lines from the dataset to print
#' @param ... Additional arguments (ignored)
#' @S3method print NpdeData

print.NpdeData <- function(x,nlines=10,...) {
  digits<-2;nsmall<-2
  cat("Object of class NpdeData\n")
  cat("    longitudinal data\n")
  if(length(x@name.data)>0)
    cat("Dataset",x@name.data,"\n")
  if(length(x@name.group)>0) {
    st1<-paste(x@name.response," ~ ",paste(x@name.predictor,collapse=" + ")," | ", x@name.group,sep="")
    cat("    Structured data:",st1,"\n")
    cat("    predictor:",x@name.predictor,paste("(",x@units$x,")",sep=""),"\n")
    if(length(x@name.covariates)>0) {
      cat("    covariates:",paste(paste(x@name.covariates," (",x@units$covariates,")",sep=""),collapse=", "),"\n")
    }
    if(dim(x@data)[1]>0) {
      if(nlines==0) return()
      cat("Dataset characteristics:\n")
      cat("    number of subjects:    ",x@N,"\n")
      cat("    number of non-missing observations:",x@ntot.obs,"\n")
      cat("    average/min/max nb obs:",format(mean(x@nind.obs),digits=digits, nsmall=nsmall), " / ", min(x@nind.obs)," / ",max(x@nind.obs),"\n")
      if(length(x@loq)>0) cat("      LOQ:    ",x@loq,"\n")
      #    if(length(x@tab)>0) print(x@tab)
      if(nlines==(-1)) {
        cat("Data:\n")
        print(x@data)
      } else {
        cat("First",nlines,"lines of data:\n")
        nrowShow <- min (nlines , nrow(x@data))
        print(x@data[1:nrowShow,])
      }
    } else cat("No data.\n")
  } else cat("Empty object\n")
}

# Could be print, with only head of data

#' Contents of an object
#'
#' Prints the contents of an object
#' 
#' @name showall
#' @aliases showall showall.NpdeData showall,NpdeData-method showall.default showall,method
#' @param object a NpdeData object
#' @keywords print
#' @S3method showall NpdeData
#' @export

#setMethod("showall","NpdeData",
# Need to create a showall 'generic' S3 function to dispatch
showall <- function(object)
  UseMethod("showall",object)

# Default showall is to print
showall.default <- function(object)
  print(object)

# Showall for NpdeData as an S3 method
showall.NpdeData <- function(object) {
  digits<-2;nsmall<-2
  cat("Object of class NpdeData\n")
  cat("    longitudinal data\n")
  if(length(object@name.data)>0) {
    cat("Dataset",object@name.data,"\n")
    st1<-paste(object@name.response," ~ ",paste(object@name.predictor,collapse=" + ")," | ", object@name.group,sep="")
    cat("    Structured data:",st1,"\n")
    cat("    subject identifier:    ",object@name.group,"\n")
    cat("    predictor:       ",object@name.predictor, paste("(",object@units$x,")",sep=""),"\n")
    cat("    response:        ",object@name.response,paste("(",object@units$y,")",sep=""),"\n")
    if(length(object@name.covariates)>0) {
      cat("    covariates:",paste(paste(object@name.covariates," (", object@units$covariates,")",sep=""),collapse=", "),"\n")
    }
    cat("This object has the following components:\n") 
    cat("     data: data\n")
    cat("     with",object@N,"subjects\n")
    cat("     ",object@ntot.obs,"observations\n")
    cat("The data has the following components\n")
    cat("     X:",object@name.predictor,"\n")
    cat("     Y:",object@name.response,"\n")
    if(length(object@name.ipred)>0) cat("     individual model predictions:", object@name.ipred,"\n")
    if(length(object@name.miss)>0) cat("     missing data:",object@name.miss," (1=missing)\n")
    if(length(object@name.cens)>0) cat("     censored data:",object@name.cens," (1=censored)\n")
    if(length(object@loq)>0) cat("      LOQ:    ",object@loq,"\n")
    cat("Dataset characteristics:\n")
    cat("    number of subjects:    ",object@N,"\n")
    if(object@N>0) {
      cat("    number of non-missing observations:",object@ntot.obs,"\n")
      cat("    average/min/max nb obs:",format(mean(object@nind.obs),digits=digits, nsmall=nsmall), " / ", min(object@nind.obs)," / ",max(object@nind.obs),"\n")
      #    if(length(object@orig)>0) print(object@orig)
    }
    if(dim(object@data)[1]>0) {
      cat("First lines of data:\n")
      nrowShow <- min (10 , nrow(object@data))
      print(object@data[1:nrowShow,])
    } else cat("No data.\n")
  } else cat("Empty object\n")
}
#)

#' Summary of a NpdeData object
#' 
#' Extracts elements from a NpdeData object
#' 
#' @aliases summary.NpdeData
#' @param object A NpdeData object
#' @param print whether to print to data to stdev
#' @param ... Additional arguments (ignored)
#' @S3method summary NpdeData

summary.NpdeData <- function(object, print=TRUE, ...) {
  if(length(object@data)==0) {
    cat("Object of class NpdeData, empty.\n")
    return()
  }
  res<-list(N=object@N,data=object@data, ntot.obs=object@ntot.obs,nind.obs=object@nind.obs)
  if(length(object@loq)>0) res$loq<-object@loq
  invisible(res)
}


#' Subsetting a NpdeData object
#' 
#' Return subset of data from a NpdeData object
#' 
#' @aliases subset.NpdeData
#' @param x A NpdeData object
#' @param subset logical expression indicating elements or rows to keep: missing values are taken as false.
#' @param ... Additional arguments (ignored)
#' @S3method subset NpdeData

subset.NpdeData<-function (x, subset, ...) {
  if (missing(subset)) 
    return(x)
  else {
    e <- substitute(subset)
    xdat<-x["data"]
    r <- eval(e, xdat, parent.frame())
    if (!is.logical(r)) 
      stop("'subset' must evaluate to logical")
    r <- r & !is.na(r)
  }
  x1<-x
  x1["data"]<-x["data"][r,,drop=FALSE]
  if(length(x1["not.miss"])>0) {
    x1["not.miss"]<-x["not.miss"][r]
    x1["icens"]<-which(!x1["not.miss"])
  }
  id<-x1["data"][,x1["name.group"]]
  x1["N"]<-length(unique(id))
  nind.obs<-tapply(id,id,length) # individual numbers of observations (1xN)
  nind.obs<-c(nind.obs[match(unique(id),names(nind.obs))])
  x1["nind.obs"]<-nind.obs
  x1["ntot.obs"]<-length(id)
  x1["ind"]<-rep(1:x1["N"],times=nind.obs)
  return(x1)
}
