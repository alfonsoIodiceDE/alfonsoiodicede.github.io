data(iris)
# Create the group matrix
X <- iris[,1:4]
n <-nrow(X)
p <-ncol(X)
G <- apply(X[iris[,5]=="setosa",],2,mean)
G <- rbind(G,apply(X[iris[,5]=="versicolor",],2,mean)) 
G <- rbind(G,apply(X[iris[,5]=="virginica",],2,mean)) 
g <- nrow(G)
rownames(G) <- c("setosa", "versicolor", "virginica") 
colnames(G) <- c("SepL","SepW","PetL","PetW") 
colnames(X) <- c("SepL","SepW","PetL","PetW")


C1 <- (49/50)*cov(X[iris[,5]=="setosa",])
C2 <- (49/50)*cov(X[iris[,5]=="versicolor",]) 
C3 <- (49/50)*cov(X[iris[,5]=="virginica",])

C <- (C1+C2+C3)/3


C.svd <- svd(C)
Cminushalf <- C.svd$u %*% diag(1/sqrt(C.svd$d)) %*% t(C.svd$v)

oneg <- rep(1,g)

Ig  =  diag(oneg)
S<- diag(rep(sqrt(1/g),g)) %*% (Ig - (1/g)* oneg %*% t(oneg)) %*%
  G %*% Cminushalf * sqrt(1/ncol(G))  
S.svd <- svd(S)
S.rpc <- sqrt(g) * S.svd$u %*% diag(S.svd$d) 
S.cbp <- S.svd$v


onen <- rep(1,n)
In <- diag(onen)


S.rsup <- (In - (1/n)* onen %*% t(onen)) %*% as.matrix(X) %*% Cminushalf %*% S.svd$v * sqrt(1/p)



plot(S.rsup, type = "n", asp=1)

text(S.rsup, labels = "·", col = c(rep("green",50),rep("violet",50), rep("brown",50)), cex=2, font = 2)

text(S.rpc, labels = rownames(G),col = c("green","violet","brown"), font = 2, adj=c(0.5,0.5))
text(S.cbp, labels = colnames(G), col = "brown", cex=0.8, font = 2)
segments(0,0,S.cbp[,1],S.cbp[,2],col="brown")



