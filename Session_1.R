# My first programme
# Date: 18th July 2021
# Session - IA

x = 1
print(x)

y=2
print(y)

x=c(1,2,3,4)
print(x)
class(x)

y=c(4,3,2,1)
print(y)
class(y)

length(x)
length(y)

z=x+y
print(z)

x=1:10
print(x)
y=10:1
print(y)

z=x+y
print(z)

x=seq(from=1, to=10,by=2)
print(x)
y=seq(from=2,to=10,by=2)
print(y)
lenght(x)
length(y)
z=x+y
print(z)

x=1:5
y=1:10
z=x+y
print(z)
x=1:10
y=10:1
x-y
x/y

x*y

length(x)
length(y)

x=seq(0,1,by=0.001)
print(x)

x=c("a","b","c","d","e")
print(x)
class(x)

x=letters[1:5]
print(x)
x=letters[1:26]
print(x)

x=LETTERS[1:5]
print(x)
length(x)
class(x)

x=1:10
print(x)
x[3]
x[3:5]
x[1]
x[c(3,5,6)]
print(x)
max(x)
min(x)
sum(x)
mean(x)
sd(x)
median(x)
x^2
x^3
sqrt(x)
cos(x)
sin(x)
tan(x)

A=matrix(data=NA,nrow=3,ncol=4)
print(A)
A[1,1]=1
A[1,2]=2
A[1,3]=3
A[1,4]=4
A[2,1]=4
A[2,2]=5
A[2,3]=6
A[2,4]

A=matrix(data=NA,nrow=3,ncol=4)
print(A)
A[1,1]=1
A[1,2]=2
A[1,3]=3
A[1,4]=4
A[2,1]=5
A[2,2]=6
A[2,3]=7
A[2,4]=8
A[3,1]=9
A[3,2]=10
A[3,3]=11
A[3,4]=12
print(A)

A=matrix(data=1:12,nrow=3,ncol=4)
print(A)
 
B=matrix(data=1:12,nrow=3,ncol=4,byrow=TRUE)
print(B)

C=A+B #by matrix addition
print(C)
D=A-B #by matrix sub
print(D)

#some function on  matrices
dim(A)
A[1,3]
max(A)
min(A)
row.names(A)=c("r1","r2","r3")
print(A)          
colnames(A)=c("c1","c2","c3","c4")
print(A)
Veiw(A)

colMeans(A)
rowMeans(A)
colSums(A)
rowSums(A)
new_A=rbind(A,colSums(A))
print(new_A)
row.names(new_A)=c(row.names((A)),"sums")
new_A

new_A=cbind(A,rowSums((A)))
print(new_A)
colnames(new_A)=c(colnames(A),"sum")
print(new_A)
sum(A)
sum(1:12)



# Session - 1B

names=LETTERS[1:5]
print(names)
age=c(19,20,21,20.5,22)
print(age)
food_status=c(TRUE,FALSE,TRUE,FALSE,FALSE)
print(food_status)
class(names)
class(age)
class(food_status)
length(names)
length(age)
length(food_status)

data=matrix(data=c(names,age,food_status),nrow=5,ncol=3)
print(data)
data[,2]
mean(data[,2])

# we have learned that matrix can not hold 
#different kind of data types

# data.frame
data=data.frame(names,age,food_status)
print(data)
data$age
data$names
data$food_status
class(data$names)
class(data$age)
class(data$food_status)

class(data)
summary(data)

# data with missing information
names=LETTERS[1:5]
print(names)
age=c(19,20,21,NA,22)
print(age)
food_status=c(TRUE,FALSE,TRUE,FALSE,NA)
print(food_status)
data=data.frame(names,age,food_status)
print(data)
summary(data)
is.na.data.frame(data)
!is.na.data.frame(data)
sum(is.na.data.frame(data))

#Summary statistics for vectors with missing information
x=c(1:10,NA,20:30,NA,31:35)
print(x)
length(x)
is.na(x)
sum(is.na(x))
mean(x)
help("mean")
mean(x,na.rm = TRUE)

# operation on matrices

A=matrix(dat=1:4,nrow=2,ncol = 2)
print(A)
B=matrix(dat=1:6,nrow=2,ncol = 3)
print(B)
dim(A)
dim(B)
ncol(A)==nrow(B)
M = A%*%B
print(A)
sum(A[1,]*B[,1])

B%*%A
t(A)      #transpose
det(A)    #determinant of A
C=solve(A)  #inverse of A
print(C)
A%*%C

m=3
n=2
A=matrix(data = 1:6,nrow = m,ncol = n)
B=matrix(data = 6:1,nrow = m,ncol = n)
print(A)
print(B)
c=matrix(data = NA,nrow = m,ncol = n)
print(c)
for (i in 1:m){
  for(j in 1:n){
    c[i,j]=A[i,j]*B[i,j]
  }
}
print(c)  
c==A*B

B=matrix(data = 6:1,nrow = 2,ncol = 3)
print(A)
print(B)

M=matrix(data = NA,nrow=nrow(A),ncol=ncol(B))
print(M)
dim(A)
dim(B)
for(i in 1:nrow(A)){
  for (j in 1:ncol(B)) {
    M[i,j]=sum(A[i,]*B[,j])
    
  }
}
print(M)
A%*%B

M==A%*%B

m=3
n=2
p=3
A=matrix(data = 1:6,nrow = m,ncol = n)
B=matrix(data = 6:1,nrow = n,ncol = p)
print(A)
print(B)
ncol(A)==nrow(B)
M=matrix(data = NA,nrow = nrow(A),ncol = ncol(B))
print(M)
dim(A)
dim(B)
for (i in 1:nrow(A)){
  for(j in 1:ncol(B)){
    M[i,j]=sum(A[i,]*B[,j])
  }
}
print(M)
A%*%B

M==A%*%B




f=function(x){
  return(x^2)
}
f(1)
f(2)

f=function(x){
  
}
x=-10:5
print(x)
x>0
sum(x>0)
x[x>0]

f=function(x){
  x[x>0]
}
f=-30:3

f=function(x){
  if(!is.numeric(x)){
    return("the data is not numeric")
  }
  x[x>0]
}
f(-1:1)
f(letters[1:4])

f=function(x){
  x*sin(1/x)
}
x=seq(0.001,0.5,by=0.0001)
par(mfrow=c(2,3))
plot(x,f(x))
plot(x,f(x),col="red")
plot(x,f(x),col="red",type="l")
plot(x,f(x),col="red",type="l",lwd=2)
plot(x,f(x),col="red",type="l",lwd=2,lty=2)
plot(x,f(x),col="blue",type="l",lwd=2,lty=2)