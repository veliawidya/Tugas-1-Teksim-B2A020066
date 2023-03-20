#Velia Arni Widyasari_B2A020066_Genap
#Multiplicative dan Bernouli 1

multiplicative_RNG<-function(a,z0,m,n) {
  xj<-matrix(NA,n,3)
  colnames(xj)<-c("aZ","Xj","Uj")
  for (j in 1:n)
  {
    xj[j,1]<-(a*z0)
    xj[j,2]<-xj[j,1]%%m
    xj[j,3]<-xj[j,2]/m
    z0<-xj[j,2]
  }
  hist(xj[,3])
  View(xj)
}

multiplicative_RNG(45,21139,417,150)

Bernouli_1<-function(n,p) {
  i<-n
  p<-p
  X<-runif(i)
  Y<-NULL
  for (k in 1:i) ifelse (X[k]<=p, Y[k]<-1, Y[k]<-0)
  (tabel<-table(Y)/length(Y))
}
Bernouli_1(150, 0.83)