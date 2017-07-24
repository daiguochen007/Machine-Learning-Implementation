
#--------------------------------------------- PCA
# data USArrests
apply(USArrests,2,mean)
apply(USArrests,2,var)

pr.out = prcomp(USArrests,scale =T)

pr.out$center
pr.out$scale
pr.out$rotation

biplot(pr.out,scale=0)

pr.var = pr.out$sdev^2
pve = pr.var/sum(pr.var)

plot(pve,type="b")
plot(cumsum(pve),type="b")


#--------------------------------------------- hierachical clustering
x= matrix(rnorm(50*2),ncol=2)
x[1:25,1] = x[1:25,1]+3
x[1:25,2] = x[1:25,2]-4

km.out = kmeans(x,2,nstart=20)
km.out$cluster

plot(x,col=km.out$cluster+1,pch=20,cex=2)

km.out = kmeans(x,3,nstart=20)
plot(x,col=km.out$cluster+1,pch=20,cex=2)


hc.complete = hclust(dist(x),method="complete")
hc.average = hclust(dist(x),method="average")
hc.single = hclust(dist(x),method="single")

plot(hc.complete,cex=.9)
plot(hc.average,cex=.9)
plot(hc.single,cex=.9)

cutree(hc.complete,2)

xsc = scale(x)
plot(hclust(dist(xsc),method="complete"),main="Scaled features")

#---------real data
library(ISLR)
nci.labs = NCI60$labs
nci.data = NCI60$data

pr.out = prcomp(nci.data,scale =T)

Cols = function(vec){
  cols = rainbow(length(unique(vec)))
  return(cols[as.numeric(as.factor(vec))])
}

plot(pr.out$x[,1:2],col = Cols(nci.labs),pch=19,xlab="Z1",ylab="Z2")
plot(pr.out$x[,c(1,3)],col = Cols(nci.labs),pch=19,xlab="Z1",ylab="Z3")

summary(pr.out)

plot(pr.out)

pve = 100*pr.out$sdev^2/sum(pr.out$sdev^2)
plot(pve,type="b")
plot(cumsum(pve),type="b")

sd.data = scale(nci.data)
data.dist = dist(sd.data)
plot(hclust(data.dist),labels=nci.labs,main = "Complete")
plot(hclust(data.dist,method="average"),labels=nci.labs,main = "Avg")
plot(hclust(data.dist,method ="single"),labels=nci.labs,main = "Single")

hc.out= hclust(data.dist)
hc.clusters = cutree(hc.out,4)
table(hc.clusters,nci.labs)

plot(hc.out,labels=nci.labs)
abline(h=139,col="red")

#compare
km.out = kmeans(sd.data,4,nstart=20)
km.clusters = km.out$cluster
table(km.clusters,hc.clusters)

hc.out=hclust(dist(pr.out$x[,1:5]))
plot(hc.out,labels = nci.labs,main="hier clust on first five")
table(cutree(hc.out,4),nci.labs)

