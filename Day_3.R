par(mfrow=c(1,1))
curve(dnorm(x,mean = 2,sd=1),-2,6,col="red",lwd=2)
curve(dnorm(x),-3,3,col="red",lwd=2,main="N(0,1)")
abline(v=0,col="black",lwd=3,lty=2)
pnorm(0)
qnorm(0.5)  # Quantile function


curve(dunif(x),col="red",lwd=2,-0.5,1.5)
abline(v=0.5,col="black",lwd=3,lty=2)
punif(0.5)
abline(v=0.8,col="green",lwd=4,lty=3)
punif(0.8)


x=seq(-4,4,by=0.01)
plot(x,dnorm(x),col="red",lwd=2,type="l",ylim =c(0,1))
pnorm(x)
lines(x,pnorm(x),col="blue",lwd=3,lty=2)
legend("topleft",legend=c(expression(F[x](x)),expression(f[x](x))),col=c("blue","red"),lwd=c(3,2),lty=c(2,1),bty="n",cex=1.5)
title("Standard Gaussian Distribution")
points(-4,0.8,pch=19,col="magenta",cex=1.5)
segments(x0=-4,y0=0.8,x1=qnorm(0.8),y1=0.8,col="green",lwd=3)
points(qnorm(0.8),pnorm(0.8),pch=19,col="magenta",cex=1.5)
segments(x0=qnorm(0.8),y0=pnorm(0.8),x1=qnorm(0.8),y1=0,col="green",lwd=3)
points(qnorm(0.8),0,pch=19,col="black",cex=1.5)
text(-4,0.75,expression(q),cex = 1.2)
text(-0.4,0,expression(F[x]^{-1}*(q)),cex=1.3)



curve(dnorm(x),col="red",lwd=2,-4,4)
abline(v=c(-1,1),col="blue",lwd=3,lty=2)
integrate(dnorm,-1,1)
pnorm(1)-pnorm(-1)

p=0.4
n=10
x=0:n
p_x=choose(n,x)*p^x*(1-p)^(n-x)
plot(x,p_x,type = "h",col="red",lwd=2,ylab="P(X=x)")
points(x,p_x,pch=19,col="blue",cex=1.5)
abline(v=3.5,col="magenta",lwd=3,lty=2)
pbinom(3.5,size=n,prob=p)
p_x[1]+p_x[2]+p_x[3]+p_x[4]
p_x[1:4]
sum(p_x[1:4])


x=seq(-0.5,11.5,by=0.01)
plot(x,pbinom(x,size=n,prob=p),type = "l",col="red",lwd=2)
points(0:10,cumsum(p_x),pch=19,cex=1.5,col="blue")
sum(1:4)
cumsum(1:4)
points(0,0.5,pch=19,col="magenta",cex=1.5)
segments(0,0.5,qbinom(0.5,size=n,prob=p),0.5,col="green",lwd=2)


qbinom(0.5,size=n,prob=p)
points(0,0.7,pch=19,col="magenta",cex=1.5)
segments(0,0.7,qbinom(0.7,size=n,prob=p),0.7,col="green",lwd=2)
qbinom(0.7,size=n,prob=p)
text(0,0.65,expression(q),cex=1.2)
segments(x0=5,y0=pbinom(5,size = n,prob=p),x1=5,y1=0,col="green",lwd = 2)
points(5,0,pch=19,col="magenta",cex=1.5)
text(6.2,0.02,expression(F[x]^{-1}*(q)),cex=1.2) 


# Computing integral
g=function(x){
  x^2
}
a=0
b=2
curve(g(x),0,2,col="red",led=2)
n=100
x=runif(n=n,min=a,max = b)
mean(x)

mean(g(x))
I=(b-a)*mean(g(x))
I

n_vals=1:2000
I=numeric(length=length(n_vals))
for(n in n_vals){
  x=runif(n=n,min=a,max = b)
  I[n]=(b-a)*mean(g(x))
}
print(I)
plot(n_vals,I,col="red",type="l",lwd=2)
abline(h=8/3,col="blue",lwd=3,lty=2)


g=function(x) {
  abs(sin(100*x))
  
}
curve(g(x),0,1,lwd=2,col="violet")
integrate(g,0,1)


n_vals=1:1000
I=numeric(length=length(n_vals))
for(n in n_vals){
  x=x=runif(n=n,min=0,max = 1)
  I[n]=(1-0)*mean(g(x))
}
plot(n_vals,I,type="l",col="red")


# the 2nd example
h=function(x){
  exp(-x^3)
}
integrate(h,0,Inf)

g=function(x){
  exp(x-x^3)
}
n_vals=1:1000
I=numeric(length=length(n_vals))
for(n in n_vals){
  x=rexp(n=n,rate = 1)
  I[n]=mean(g(x))
}
plot(n_vals,I,col="red",lwd=2,type="l")
abline(h=0.8929795,col="black",lwd=3,lty=2)




h=function(x){
  (x^2)*exp(-x^2)
}
integrate(h,0,Inf)

g=function(x){
  (x^2)*exp(x-x^2)
}
n_vals=1:1000
I=numeric(length=length(n_vals))
for(n in n_vals){
  x=rexp(n=n,rate = 1)
  I[n]=mean(g(x))
}
plot(n_vals,I,col="red",lwd=2,type="l")
abline(h=0.8929795,col="black",lwd=3,lty=2)
