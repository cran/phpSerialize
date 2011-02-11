###            Serialize S(3) classes to php
###
### Copyright Dieter Menne
### Dr. Menne Biomed Software, Tübingen, Germany
### dieter.menne@menne-biomed.de
### Version 0.8.01

phpSerialize = function(x,file=NULL,append=FALSE,associative="1D",
         simplifyMono=TRUE,phpTestCode=FALSE){
  ret = capture.output( {
    if (phpTestCode) cat("<?php\n$b='")
    phpSer(x,associative=associative,simplifyMono)
    if (phpTestCode) cat("';\nprint_r( unserialize($b));\n?>")
    else cat("\n");
    },  file=file,append=!is.null(file) && append)
  if (!is.null(file)) invisible(ret) else ret
}

phpSerializeAll = function(include=NULL,exclude=NULL,file=NULL,
     append=FALSE,associative="1D",simplifyMono=TRUE,
     phpTestCode=FALSE) {
  if (!append && !is.null(file) && file.exists(file)) {
    file.remove(file)
  }
  names=ls(1)
  objs=list()
  for (i in seq(along=names)) {
    name=names[i]
    xf=eval(parse(text=name))
    if (! (is.null(xf) || is.function(xf))){
      inInclude=is.null(include) || any(pmatch(include,name),na.rm=TRUE)
      if (inInclude) {
        inExclude=!is.null(exclude) && any(pmatch(exclude,name),na.rm=TRUE)
        if (!inExclude) {
          objs[[name]]=xf;
        }
      }
    }
  }
  phpSerialize(objs,file=file,associative=associative,
     simplifyMono=simplifyMono,phpTestCode=phpTestCode,append=TRUE)
}

#--- All the following are internal functions
phpSer = function(x,associative='1D',simplifyMono=TRUE,...){
  # Null is reduced to the minimum
  if (is.null(x)) {
    cat('N;');
  }
  else # dispatch the normal case
  {
    if (is.list(x)) class(x)="list" # works for qr and others
    UseMethod("phpSer",x)
  }
}

# Internal workhorse for double, integer, boolean
phpSerSimple = function (x,typ,associative="1D",simplifyMono=TRUE) {
  noNames=is.null(names(x))
  # Unnamed vectors of length 1 are simplified by default
  simplifyMono= simplifyMono && noNames && length(x)==1
  if (simplifyMono) {
    cat(typ,':',x[1],';',sep='')
  }
  else { # Named vectors of length 1 and all other vectors
    cat('a:',length(x),':{',sep='')
    if (associative =="no" || is.null(attr(x,"names"))) {
      # make a numeric array
      for (i in seq(along=x)) {
        cat('i:',i,';',typ,':',x[i],';',sep='')
        }
      }
    else {# make an associative array
      for (i in seq(along=x)) {
        labl=names(x)[i]
        cat('s:',nchar(labl),':"',labl,'";',typ,':',x[i],';',sep='')
      }
    }
    cat('}',sep='')
  }
}

# strings
phpSer.character = function(x,associative="1D",simplifyMono=TRUE,...) {
  simplifyMono= simplifyMono && is.null(names(x)) && length(x)==1
  if (simplifyMono) {
    cat('s:',nchar(x[1]),':"',x[1],'";',sep='')
  }
  else { # named strings or > 1 vectors
    cat('a:',length(x),':{',sep='')
    if (is.null(attr(x,"names"))) {
      # make a numeric array
      for (i in seq(along=x)) {
        cat('i:',i,';s:',nchar(x[i]),':"',x[i],'";',sep='')
      }
    }
    else {# make an associative array
      for (i in seq(along=x)) {
        labl=names(x)[i]
        cat('s:',nchar(labl),':"',labl,'";s:',nchar(x[i]),':"',x[i],'";',sep='')
      }
    }
    cat('}',sep='')
  }
}

# Dispatcher functions
phpSer.array = function(x,associative="1D",simplifyMono=TRUE,...) {
  stop("phpSer of a general array not implemented");
}

phpSer.complex = function(x,associative="1D",simplifyMono=TRUE,...) {
  stop("phpSer of complex numbers not implemented");
}

# Skip functions
phpSer.function = function(x,associative="1D",simplifyMono=TRUE,...) {
}

phpSer.double = function (x,associative="1D",simplifyMono=TRUE,...) {
  phpSerSimple(x,"d",associative,simplifyMono)
}

phpSer.numeric = function (x,associative="1D",simplifyMono=TRUE,...) {
  phpSerSimple(x,"d",associative,simplifyMono)
}

phpSer.integer = function (x,associative="1D",simplifyMono=TRUE,...) {
  phpSerSimple(x,"i",associative,simplifyMono)
}

phpSer.logical = function (x,associative="1D",simplifyMono=TRUE,...) {
  phpSerSimple(as.integer(x),"b",associative,simplifyMono)
}

phpSer.call = function (x,associative="1D",simplifyMono=TRUE,...) {
  phpSer.character(as.character(x),associative,simplifyMono)
}

phpSer.terms = function (x,associative="1D",simplifyMono=TRUE,...) {
  phpSer.character(as.character(x),associative,simplifyMono)
}

phpSer.formula = function (x,associative="1D",simplifyMono=TRUE,...) {
  phpSer.character(as.character(x),associative,simplifyMono)
}

phpSer.table = function (x,associative="1D",simplifyMono=TRUE,...) {
  phpSer.matrix(x,associative,simplifyMono)
}

phpSer.pdMat = function(x,associative="1D",simplifyMono=TRUE,...) {
  phpSer.matrix(x,associative,simplifyMono)
}

phpSer.factor = function (x,associative="1D",simplifyMono=TRUE,...) {
  if (associative=="no")
    phpSer(as.integer(x),associative,simplifyMono)
  else
    phpSer(as.character(x),associative,simplifyMono)
}

# Structure serialization
phpSer.list = function(x,associative="1D",simplifyMono=TRUE,...) {
  cat('a:',length(x),':{',sep='')
  showLabel=associative != "no"
  for (i in seq(along=x)) {
    if (showLabel) {
      labl=names(x)[i]
      if (is.null(labl)) labl=paste("list",i,sep='')
      cat('s:',nchar(labl),':"',labl,'";',sep='')
    }
    else
      cat('i:',i,';',sep='')
    phpSer(x[[i]],associative,simplifyMono)
  }
  cat('}',sep='')
}



phpSer.matrix = function(x,associative="1D",simplifyMono=TRUE,...) {
  # Degenerate case
  if (is.null(nrow(x))) {
    phpSer(as.vector(x))
  }
  else {
    cat('a:',nrow(x),':{',sep='')
    showLabel = associative=="2D"
    if (!showLabel) associative="no"
     for (i in 1:nrow(x)) { # really 2D
      if (showLabel){
        labl=names(x[i])
        cat('s:',nchar(labl),':"',labl,'";',sep='')
      }
      else
        cat('i:',i,';',sep='')
      phpSer(x[i,],associative,simplifyMono)
    }
    cat('}',sep='')
  }
}
