# function: DescribeByTable

# uses the describe and describeBy functions in the psych package
# default to having 25th and 75th percentiles.

# reformats the output to make it suitable for a table in a report.

DescribeByTable<-function(X,Y=NULL,quantiles=c(.25,.75)){
if(is.null(Y)){
TB1<-as.matrix(psych::describe(X,quant=quantiles))
rownames(TB1)<-""
TB1<-TB1[,-1]
}else{
  if(!is.factor(Y)){
    Y<-as.factor(Y)
  }
TB1<-psych::describeBy(X,group=Y,quant=quantiles,mat=TRUE)
rownames(TB1)<-levels(Y)
TB1<-TB1[,-(1:3)]
}
TB1
}

