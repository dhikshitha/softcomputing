# load and prepare data
library(clv)
library(cluster)
data(iris)
iris.data <- iris[,1:4]

# cluster data
predictedClusters<-function(k){
  agnes.mod <- agnes(iris.data) # create cluster tree 
  v.pred <- as.integer(cutree(agnes.mod,k)) # "cut" the tree 
  
  intraclust = "centroid"
  interclust = "centroid"
  
  cls.scatt <- cls.scatt.data(iris.data, v.pred, dist="manhattan")
  return(v.pred)
}
Dunn <- function(data,clust) 
  clv.Dunn( cls.scatt.data(data,clust),
            intracls = "centroid", 
            intercls = "centroid"
  )
Davies.Bouldin <- function(data,clust) 
  clv.Davies.Bouldin( cls.scatt.data(data,clust),
                      intracls = "centroid",
                      intercls ="centroid"
  )
dunnIndex<-function(k){
  v.pred<-predictedClusters(k)
  dunn <- Dunn(iris.data, v.pred)
  return(dunn)
}
dbIndex<-function(k){
  v.pred<-predictedClusters(k)
  davies <- Davies.Bouldin(iris.data, v.pred)
  return(davies)
}
dunn<-vector()
for(i in 2:10){
  d1<-dunnIndex(i)
  dunn<-append(dunn,d1)
}
db<-vector()
for(i in 2:10){
  d1<-dbIndex(i)
  db<-append(db,d1)
}

par(mfrow=c(1,2))
plot(c(2:10),dunn,xlab="No.of Clusters",ylab="DunnIndex",type="o")
plot(c(2:10),db,xlab="No.of Clusters",ylab="DBIndex",type="o")


#check plot
#check for Iris
par(mfrow=c(2,2))
library(philentropy)
library(cluster)
it=1
eps=0.1
m=2
no_of_clusters=3
no_of_dimensions=4
no_of_datapoints=150

datapoints<-c()
#for(i in 1:no_of_dimensions){
#  datapoints<-cbind(datapoints,round(runif(no_of_datapoints,min=1,max=10)))
#}
data(iris)
datapoints=cbind(iris[,1],iris[,2],iris[,3],iris[,4])

#step1
clusters<-c()
for(i in 1:no_of_clusters){
  clusters<-cbind(clusters,runif(no_of_datapoints))
}
for(i in 1:nrow(clusters)){
  clusters[i,]=clusters[i,]/sum(clusters[i,])
}
clusters

#datapoints<-cbind(c(1,2,3,4,5,6),c(6,5,8,4,7,9))
#clusters<-cbind(c(0.8,0.9,0.7,0.3,0.5,0.2),c(0.2,0.1,0.3,0.7,0.5,0.8))
centroid<-c()

repeat{
  
  #step2
  prev_centroid=centroid
  centroid<-c()
  for(i in 1:no_of_clusters){
    temp<-c()
    for(j in 1:no_of_dimensions){
      temp<-c(temp,sum((clusters[,i]^m)*datapoints[,j])/sum(clusters[,i]^m))
    }
    centroid<-rbind(centroid,temp)
  }
  rownames(centroid)<-c()
  centroid
  
  #step3
  dissimilarity<-c()
  for(i in 1:no_of_clusters){
    temp<-c()
    for(j in 1:no_of_datapoints){
      temp<-rbind(temp,distance(rbind(centroid[i,],datapoints[j,]),method="euclidean"))
    }
    dissimilarity<-cbind(dissimilarity,temp)
  }
  colnames(dissimilarity)<-c()
  dissimilarity
  
  #step4
  for(i in 1:nrow(dissimilarity)){
    dissimilarity[i,]=((1/dissimilarity[i,])/sum(1/dissimilarity[i,]))
  }
  dissimilarity
  
  cat('Iteration',it,'\n')
  print(cbind(datapoints,clusters,dissimilarity))
  #plot(1:150,rowMax(dissimilarity))
  #######
  k<-c()
  for(i in 1:no_of_datapoints){
    k<-c(k,which(dissimilarity[i,]==max(dissimilarity[i,])))
  }
  clusplot(datapoints, k, color=TRUE, shade=TRUE,
           labels=no_of_clusters, lines=0)
  ########
  clusters=dissimilarity
  
  #exit condition
  it=it+1
  if(it>=3){
    flag=0
    for(i in 1:no_of_clusters){
      if(distance(rbind(centroid[i,],prev_centroid[i,]),method="euclidean")<eps){
        flag=flag+1
      }
    }
    if(flag==no_of_clusters){
      break
    }
  }
}
