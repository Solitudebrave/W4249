
#Question 4
particle<-c(0:10)
pms<-dpois(particle, 3.87, log = FALSE)*2608
round(pms, digits=0)
2608-sum(round(pms,digits=0))
particle.obs<-c(57, 203, 383, 525, 532, 408, 273, 139, 45, 27, 10, 6)
particle.exp<-c(round(pms, digits=0), 2608-sum(round(pms,digits=0)))
particle.exp1<-c(pms, 2608-sum(pms,digits=0))
round((particle.obs-particle.exp1)^2/particle.exp1, digits=2)
sum((particle.obs-particle.exp1)^2/particle.exp1)
qchisq(0.95, df=11)

#Newton Method

f<-function (x) {
  x*exp(-10*x)-10/sum(1/factorial(c(0:10)))
}

newton<-function(f, tol, x0, N=20) {
  h<-0.001
  i <- 1; x1<-x0
  p <- numeric(N)
  while (i<=N) {
    df.dx <- (f(x0+h)-f(x0))/h
    x1<-(x0-(f(x0)-df.dx))
    p[i] <-x1
    i+ i+1
    if ((x1-x0)^2 <= tol^2) break
    x0<-x1
  }
  return(p[1:(i-1)])
}

newton(f,tol=0.1^8, x0=3.8, N=20) 


x<-c(1, 1 , -1, 0, 0, 1,1,0,1,-1,1,1, -1, 0, 1,1,0,0,1,1,-1,0,-1, 1,1, 0,1,-1,-1,-1, 1,1,1,1, 0,1,1, 0,0,0, 1,1,-1,-1, 0,0,1,1,0,1);
table(x)
ss<-chisq.test(table(x))

chisq.test(particle.exp, particle.obs)

particle.num<-c(0:11)
particle.obs<-c(57, 203, 383, 525, 532, 408, 273, 139, 45, 27, 10, 6)
sum(particle.num*particle.obs)
