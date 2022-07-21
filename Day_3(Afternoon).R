# Plot the graph of f(x)=x^3-3x+5

f=function(x){
  x^3-3*x+5
}
curve(f,-4,4)
abline(h=0,v=0,col="red")


polyroot(c(5,-3,0,1))



f=function(x){
  x^4+4*x^3-3*x^2+x+6
}
curve(f,-3,3)
abline(h=0,v=0,col="red")


polyroot(c(6,1,-3,4,1))


# Roots of f(x)=1+(3+2i)x+(3-7i)x^2=0
f=function(x){
  1+(3+2i)x+(3-7i)x^2=0
}
curve(f,-2,2)
abline(h=0,v=0,col="red")
polyroot(c(1,(3+2i),(3-7i)))

f=function(x){
  x^3-3*x-1
}
curve(f,1.85,1.90,col="magenta")
abline(h=0,v=0,col="red")


# Bisection method to find a root
f=function(x){
  x^3-3*x-1
}
a=1
b=3
f(a)*f(b)   # Since f(a)*f(b)<0, f has real root between a and b
mid=(a+b)/2;mid
f(mid)
if(f(a)*f(mid)<0){
  b=mid
}else{
  a=mid
}
print(c(a,b))



# In a for loop
for (k in 1:10) {
  mid=(a+b)/2;mid
  f(mid)
  if(f(a)*f(mid)<0){
    b=mid
  }else{
    a=mid
  }
  cat("iteration",k,"a=",a,"b=",b,"\n")
  
}


# User define function for bisection method
bisect=function(f,a,b,maxit=100){
  for (k in 1:maxit) {
    mid=(a+b)/2
    if(f(a)*f(mid)<0){
      b=mid
    }else{
      a=mid
    }
  }
  root=(a+b)/2
  return(root)
  } 
f <- function(x) {x^3-3*x-1}
bisect(f,1,3,20)

polyroot(c(-1,-3,0,1))
  
  
# Roots using Newtons-Raphson Method
f=function(x){x^3-5*x^2+2*x+6}
curve(f,-2,5,col="magenta")
abline(h=0,v=0,col="red")
df=function(x){3*x^2-10*x+2}
x0=0

# itration 1
x1=x0-f(x0)/df(x0)
print(x1)

# itration 2
x2=x1-f(x1)/df(x1)
print(x2)

#itration 3
x3=x2-f(x2)/df(x2)
print(x3)


x0=1
maxit=10
for (k in 1:maxit) {
  x1=x0-f(x0)/df(x0)
  cat("itration:" ,k,"xk=",x1,"\n")
  x0=x1
}


# User define function for Newton-Raphson
newton=function(f,df,x0,maxit=100){
  for (k in 1:maxit) {
    x1=x0-f(x0)/df(x0)
  }
  return(x0)
  
}
f=function(x){x^3-5*x^2+2*x+6}
df=function(x){3*x^2-10*x+2}
newton(f,df,0.5,300)    


# Solving system of linear equations iteratively
A=matrix(c(4,-2,1,2,-7,3,1,5,-20),byrow = TRUE,nrow = 3)
A
B=matrix(c(7.5,8,10),nrow = 3)
B
solve(A,B)
D=diag(diag(A))
D
L<-U<-A
L[upper.tri(A,diag=TRUE)]<-0
U[lower.tri(A,diag=TRUE)]<-0
L
U
x0=matrix(c(1,1,1),3);x0
D1=solve(D)
H=-D1%*%(L+U)
C=D1%*%B
x1=H%*%x0+C
print(x1)

x2=H%*%x1+C
print(x2)

x3=H%*%x2+C
print(x3)

### Looping 
x0=matrix(c(0,0,0),3)
maxit=10
for (k in 1:maxit) {
  x0=H%*%x0+C
  cat("itration:",k,"xk=",x0,"\n")
}

## Trapezoidal Rule
trap<-function(f,a,b,m=100){
  x=seq(a,b,length.out=m+1)
  y=f(x)
  p.area=sum(y[2:(m+1)]+y[1:m])
p.area=p.area*abs(b-a)/(2*m)
return(p.area)

}
phi=function(x)exp(-x^2/2)/sqrt(2*pi)
curve(phi,-3,3)
trap(phi,-3,3,100)