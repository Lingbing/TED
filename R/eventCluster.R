#' Cluster detected events
#' 
#' This function groups the detected events into clusters. The clustering is based on statistical characteristics of
#' event.

#' 
#' @param x a vector or time series
#' @param a a vector consisting of the starting points of all the detected events
#' @param b a vector consisting of the ending points of all the detected events
#' @param k0 the number of clusters
#' @export
#' @examples
#' set.seed(12345)
#' n=128
#' types=c('box','rc','cr','sine')
#' shapes=matrix(NA,20,n)
#' for (i in 1:20){
#'   shapes[i,]=cbfs(type=types[sample(1:4,1)])
#' }
#' whitenoise=ts2mat(rnorm(128*20),128)
#' x=c(rnorm(128),t(cbind(shapes,whitenoise)))
#' plot(x,ty='l')
#' w=128
#' alpha=0.05
#' events=eventDetection(x,w,alpha,'art')
#' a=events$start
#' b=events$end
#' cc=eventCluster(x,a,b,4)
#' myclkm=cc$cl

eventCluster<-function(x,a,b,k0){
  eventmeasures=foreach (i=1:length(a),.combine=rbind) %dopar% {
    event=x[a[i]:b[i]]
    measures(event)
  }  
  tranmeasures=matrix(0,length(a),dim(eventmeasures)[2])
  for (i in 1:dim(eventmeasures)[2]){
    x=eventmeasures[,i]
    if (min(x,na.rm=TRUE)<=0){
      x=1+eventmeasures[,i]-min(eventmeasures[,i],na.rm=TRUE)
    }else{
      x=x}
    tranmeasures[,i]=(x)^boxcoxfit(x)$lambda}
  pc.cr <- princomp(scale(tranmeasures))
  ratio_kmeans=rep(NA,100)
  for (i in 1:100){
    assign(paste('cc',i,sep=''),kmeans(pc.cr$scores[, 1:5],k0,iter.max=1000))
    ratio_kmeans[i]=get(paste('cc',i,sep=''))$tot.withinss/get(paste('cc',i,sep=''))$totss}
  index=which.min(ratio_kmeans)
  cc=get(paste('cc',index,sep=''))
  myclkm=cc$cluster
  index=list()
  center=matrix(NA,k0,dim(eventmeasures)[2])
  for (i in 1:k0){
    index[[i]]=which(myclkm==i)
    if (!is.array(eventmeasures[index[[i]],])){
      center[i,]=(eventmeasures[index[[i]],])
    }else{ 
      center[i,]=colMeans(eventmeasures[index[[i]],])}}
  colnames(center)=colnames(eventmeasures)
  return(list(cl=myclkm,center=center))
}