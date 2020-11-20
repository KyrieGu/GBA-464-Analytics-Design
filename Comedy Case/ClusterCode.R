require("cluster")
require("fpc")
require("factoextra")
require("gridExtra")
library(cluster)
library(fpc)
library(factoextra)
library(gridExtra)


##Evaluate number of clusters to use on data with visualizations
##Arguments: 
##  toClust, the data to do kmeans cluster analysis
##  maxClusts=15, the max number of clusters to consider
##  seed, the random number to initialize the clusters
##  iter.max, the max iterations for clustering algorithms to use
##  nstart, the number of starting points to consider
##Results:
##  a list of weighted sum of squares and the pamk output including optimal number of clusters (nc)
##  to create visualizations need to print tmp
clustTest = function(toClust,print=TRUE,scale=TRUE,maxClusts=15,seed=12345,nstart=20,iter.max=100){
  if(scale){ toClust = scale(toClust);}
  set.seed(seed);   # set random number seed before doing cluster analysis
  wss <- (nrow(toClust)-1)*sum(apply(toClust,2,var))
  for (i in 2:maxClusts) wss[i] <- sum(kmeans(toClust,centers=i,nstart=nstart,iter.max=iter.max)$withinss)
  ##gpw essentially does the following plot using wss above. 
  #plot(1:maxClusts, wss, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares")
  gpw = fviz_nbclust(toClust,kmeans,method="wss",iter.max=iter.max,nstart=nstart,k.max=maxClusts) #alternative way to get wss elbow chart.
  pm1 = pamk(toClust,scaling=TRUE)
  ## pm1$nc indicates the optimal number of clusters based on 
  ## lowest average silhoutte score (a measure of quality of clustering)
  #alternative way that presents it visually as well.
  gps = fviz_nbclust(toClust,kmeans,method="silhouette",iter.max=iter.max,nstart=nstart,k.max=maxClusts) 
  if(print){
    grid.arrange(gpw,gps, nrow = 1)
  }
  list(wss=wss,pm1=pm1$nc,gpw=gpw,gps=gps)
}
##Runs a set of clusters as kmeans
##Arguments:
##  toClust, data.frame with data to cluster
##  nClusts, vector of number of clusters, each run as separate kmeans 
##  ... some additional arguments to be passed to clusters
##Return:
##  list of 
##    kms, kmeans cluster output with length of nClusts
##    ps, list of plots of the clusters against first 2 principle components
runClusts = function(toClust,nClusts,print=TRUE,maxClusts=15,seed=12345,nstart=20,iter.max=100){
  if(length(nClusts)>4){
    warning("Using only first 4 elements of nClusts.")
  }
  kms=list(); ps=list();
  for(i in 1:length(nClusts)){
    kms[[i]] = kmeans(toClust,nClusts[i],iter.max = iter.max, nstart=nstart)
    ps[[i]] = fviz_cluster(kms[[i]], geom = "point", data = toClust) + ggtitle(paste("k =",nClusts[i]))
   
  }
  library(gridExtra)
  if(print){
    tmp = marrangeGrob(ps, nrow = 2,ncol=2)
    print(tmp)
  }
  list(kms=kms,ps=ps)
}

##Plots a kmeans cluster as three plot report
##  pie chart with membership percentages
##  plot that indicates cluster definitions against principle components
##  barplot of the cluster means, which by default standardizes the cluster means
plotClust = function(km,toClust,
                     discPlot=FALSE,standardize=TRUE,margins = c(7,4,4,2)){
  nc = length(km$size)
  #if(discPlot){par(mfrow=c(2,2))}
  #else {par(mfrow=c(2,2))}
  percsize = paste(1:nc," = ",format(km$size/sum(km$size)*100,digits=2),"%",sep="")
  pie(km$size,labels=percsize,col=1:nc)
  
  gg = fviz_cluster(km, geom = "point", data = toClust) + ggtitle(paste("k =",nc))
  print(gg)
  #clusplot(toClust, km$cluster, color=TRUE, shade=TRUE,
  #         labels=2, lines=0,col.clus=1:nc); #plot clusters against principal components
  
  if(discPlot){
    plotcluster(toClust, km$cluster,col=km$cluster); #plot against discriminant functions ()
  }
  if(!standardize){
    rng = range(km$centers)
    dist = rng[2]-rng[1]
    locs = km$centers+.05*dist*ifelse(km$centers>0,1,-1)
    bm = barplot(km$centers,beside=TRUE,col=1:nc,main="Cluster Means",ylim=rng+dist*c(-.1,.1))
    text(bm,locs,formatC(km$centers,format="f",digits=1))
  } else {
    kmc = (km$centers-rep(colMeans(toClust),each=nc))/rep(apply(toClust,2,sd),each=nc)
    rng = range(kmc)
    dist = rng[2]-rng[1]
    locs = kmc+.05*dist*ifelse(kmc>0,1,-1)
    par(mar=margins)
    bm = barplot(kmc,col=1:nc,beside=TRUE,las=2,main="Cluster Means",ylim=rng+dist*c(-.1,.1))
    text(bm,locs,formatC(kmc,format="f",digits=1))
  }
}