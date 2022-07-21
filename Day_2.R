u=c(1,-1,2)
v=c(3,-1,1)
u+v
2*u-3*v
u*v
sum(u*v)
sqrt(sum(u*u))  #length of the vector u

vec_length=function(a){
  return(sqrt(sum(a*a)))
}
vec_length(u)
p=sum(u*v)/vec_length(u)^2*u
p

sum((v-u)*u)

orth_proj=function(a,b){
  p=sum(a*b)/vec_length(a)^2*a
  return(p)
}
orth_proj(u,v)

library(matlib)
help(matlib)
Proj(v,u)

########### Working with matrices##########

A=matrix(c(2,3,4,6,8,9,12,3,5),nrow=3,byrow=TRUE)
A
B=matrix(c(12,2,5,8,13,-12,24,8,9),nrow = 3)
B
t(A)  #return transpose of A
2*A+3*B
A*B   # elementwise multiplication
A%*%B  #Matrix multiplication
det(A)
solve(A)
A1=solve(A)
A%*%A1
round(A%*%B)
mpower(A,2)
help(mpower)

matrix_power=function(A,p){
  R=A
  for(i in 2:p){
    R=R%*%A
  }
  return(R)
}

P=matrix(c(0.2,0.3,0.4,0.1,
         0.3,0.1,0.5,0.1,
         0.25,0.25,0.25,0.25,
         0.25,0.25,0.25,0.15),nrow=4)
P
X0=matrix(c(2500,2500,2500,2500),nrow=4)
X0
matrix_power(P,5000)%*%X0

################ Reduced row echlon form ##############
library(pracma)
rref(A)
A=matrix(c(1,2,3,4,
           4,6,8,3,
           3,8,5,6,
           9,4,7,6),nrow=4)
A
B=matrix(c(21,23,12,10))
Aug=cbind(A,B)
rref(Aug)
solve(A,B)
Aug1=rref(Aug)
s=Aug1[,5]
s
A%*%s
I4=diag(4)  # 4x4 identity matrix
cbind(A,I4)
rref(cbind(A,I4))
Ainv=rref(cbind(A,I4))[,5:8]
Ainv

## Check whether the following vectors are linearly independent or not
v1=c(1,2,-3)
v2=c(3,-2,1)
v3=c(5,3,2)
A=cbind(v1,v2,v3);A
det(A)
v=c(1,1,1)
A1=cbind(A,v);A1
rref(A1)
alpha=rref(A1)[,4]  # co-ordinates of vectors w.r.t. the basis {v1,v2,v3}
alpha

## Extending a linearly independent vector to a basis

v1=c(1,2,-3,4,2)
v2=c(2,2,-3,4,2)
v3=c(2,2,-2,4,2)
rref(rbind(v1,v2,v3))
cbind(v1,v2,v3,diag(5))
M=cbind(v1,v2,v3,diag(5))
rref(M)


T=function(x){c(2*x[1]-3*x[2]+x[3],x[1]+2*x[2]-3*x[3],x[1]+x[2]+x[3])}
## Basis B={u1,u2,u3} of the domain
u1=c(1,-1,2)
u2=c(3,-1,5)
u3=c(-2,4,7)

## Basis c={v1,v2,v3} of the co-domain
v1=c(2,0,1)
v2=c(-1,3,2)
v3=c(1,0,3)

## M column matrix whose column are v1,v2,v3 and T(u1),T(u2),T(u3)
M=cbind(v1,v2,v3,T(u1),T(u2),T(u3))
rref(M)
MA=rref(M)[,4:6]
MA

############## Eigenvalue and eigenvectors #############
A=matrix(c(4,-1,2,3,
           -1,7,2,-1,
           2,2,-1,0,
           3,-1,0,7),nrow = 4,byrow = TRUE)
eig(A)
eigen(A)
eigen(A)$values
eigen(A)$vector

P=eigen(A)$vectors
D=solve(P)%*%A%*%P
print(D)
