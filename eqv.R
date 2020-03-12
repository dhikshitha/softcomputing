data("iris")
data = iris
normalize <- function(x,new_min,new_max) {
  return (((x - min(x)) / (max(x) - min(x)))*(new_max-new_min)+new_min)
}
makeSymm <- function(m) {
  m[lower.tri(m)] <- t(m)[lower.tri(m)]
  return(m)
}
composition <- function(r1,r2) {
  result = matrix(0,nrow(r1),ncol(r1))
  for(i in c(1:nrow(r1))) {
    for(j in c(1:ncol(r1))) {
      result[i,j] = max(pmin(r1[i,],r2[,j]))
    }
  }
  return(result)
}
checkTransitivity<-function(fuzzyMatrix){
  rows=nrow(fuzzyMatrix)
  cols=ncol(fuzzyMatrix)
  for(i in 1:(rows-1)){
    for(j in (i+1):rows){
      value1=fuzzyMatrix[i,j]
      for(x in 1:cols){
        value2=fuzzyMatrix[j,x]
        value3=fuzzyMatrix[i,x]
        if(value3<min(value1,value2)){
          return(FALSE)
        }
      }
    }
  }
  return(TRUE)
  
}
minMax<-function(){
  fuzzyMatrix=matrix(1,nrow(data),nrow(data))
  rows=nrow(fuzzyMatrix)
  for(i in 1:(rows-1)){
    for(j in (i+1):rows){
      up=0
      down=0
      for(x in 1:(ncol(data)-1)){
        up=up+min(data[i,x],data[j,x])
        down=down+max(data[i,x],data[j,x])
      }
      fuzzyMatrix[i,j]<-round(up/down,3)
      
    }
  }
  return(makeSymm(fuzzyMatrix))
}
cosineAmplitude<-function(){
  fuzzyMatrix=matrix(1,nrow(data),nrow(data))
  rows=nrow(fuzzyMatrix)
  for(i in 1:(rows-1)){
    for(j in (i+1):rows){
      up=0
      downFirst=0
      downSecond=0
      for(x in 1:(ncol(data)-1)){
        up=up+(data[i,x]*data[j,x])
        downFirst=downFirst+(data[i,x]^2)
        downSecond=downSecond+(data[j,x]^2)
      }
      down=sqrt(downFirst*downSecond)
      fuzzyMatrix[i,j]<-round(up/down,4)
      
    }
  }
  return(makeSymm(fuzzyMatrix))
}
lambdaCut<-function(fuzzyMatrix,cut){
  l=matrix(0,nrow(fuzzyMatrix),ncol(fuzzyMatrix))
  for(i in 1:nrow(fuzzyMatrix)){
    for(j in 1:ncol(fuzzyMatrix)){
      if(fuzzyMatrix[i,j]>=cut){
        l[i,j]=1
      }
    }
  }
  return(l)
}
# for(i in 1:(ncol(data)-1)){
#   data[,i]=normalize(data[,i],0,1)
# }
print(data)
fm=minMax()
flag=TRUE
result=fm
while(flag){
  tm=checkTransitivity(result)
  if(tm==TRUE){
    flag=FALSE
  }
  else{
    result=composition(result,fm)
  }
}
print(result)
cutMatrix=lambdaCut(result,0.75)
groups<-list()
output<-rep(0,nrow(data))
print('GROUPS')
for(i in c(1: ncol(cutMatrix))) {
  position = which(cutMatrix[,i] == 1)
  if(length(position) > 0)
    groups[[length(groups)+1]]<-position
  label<-length(groups)
  for(j in 1:length(position)){
    output[position[j]]<-label
  }
  cutMatrix[position,] = 0;
}
x<-vector()
labels<-vector()
for(i in 1:length(groups)){
  x=append(x,length(groups[[i]]))
  labels=append(labels,paste("group",i))
}
print(groups)
print(output)
par(mfrow=c(1,2))
pie(x,labels,main="Count of records in each Class",col=rainbow(length(x)))
plot(data[,1],data[,2],xlab="Sepal Length",ylab="Sepal Width",col=output,main="Sepal Length vs SepalWidth",pch=2)
legend("topleft",
       legend = labels, col=c("black","red","green"),pch=2)