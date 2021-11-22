### The Matrix

# the matrix function
# R wants the data to be entered by columns starting with column one
# 1st arg: c(2,3,-2,1,2,2) the values of the elements filling the columns
# 2nd arg: 3 the number of rows
# 3rd arg: 2 the number of columns

A <- matrix(c(2,3,-2,1,2,2),3,2)

## Is Something a Matrix

is.matrix(A)
is.vector(A)

## Multiplication by a Scalar

c <- 3
print(c*A)

## Matrix Addition & Subtraction

B <- matrix(c(1,4,-2,1,2,1),3,2)

C <- A + B
print(C) 

D <- A - B
print(D)

## Matrix Multiplication

D <- matrix(c(2,-2,1,2,3,1),2,3)
print(D)

C <- D %*% A
print(C)

C <- A %*% D
print(C)

D <- matrix(c(2,1,3),1,3)
print(D)

C <- D %*% A
print(C)

C <- A %*% D

## Transpose of a Matrix

AT <- t(A)
print(AT)

ATT <- t(AT)
print(ATT)

## Common Vectors
# Unit Vector

U <- matrix(1,3,1)
print(U)

# Zero Vector

Z <- matrix(0,3,1)
print(Z)

## Common Matrices
# Unit Matrix

U <- matrix(1,3,2)
print(U)

# Zero Matrix

Z <- matrix(0,3,2)
print(Z)

# Diagonal Matrix

S <- matrix(c(2,3,-2,1,2,2,4,2,3),3,3)
print(S)

D <- diag(S)
print(D)

D <- diag(diag(S))
print(D)

# Identity Matrix

I <- diag(c(1,1,1))
print(I)

# Symmetric Matrix

C <- matrix(c(2,1,5,1,3,4,5,4,-2),3,3)
print(C)

CT <- t(C)
print(CT)

# Inverse of a Matrix

A <- matrix(c(4,4,-2,2,6,2,2,8,4),3,3)
print(A)

AI <- solve(A)
print(AI)

A %*% AI

AI %*% A

# Inverse & Determinant of a Matrix

C <- matrix(c(2,1,6,1,3,4,6,4,-2),3,3)
print(C)

CI <- solve(C)
print(CI)

d <- det(C)
print(d)

# Rank of a Matrix
  
A <- matrix(c(2,3,-2,1,2,2,4,7,0),3,3)
print(A)

matA <- qr(A)
print(matA$rank)

A <- matrix(c(2,3,-2,1,2,2,4,6,-4),3,3)
print(A)

matA <- qr(A)
print(matA$rank)

# note column 3 is 2 times column 1

## Number of Rows & Columns

X <- matrix(c(3,2,4,3,2,-2,6,1),4,2)
print(X)

dim(X)

r <- nrow(X)
print(r)

c <- ncol(X)
print(c)

## Computing Column & Row Sums

# note the uppercase S

A <- matrix(c(2,3,-2,1,2,2),3,2)
print(A)

c <- colSums(A)
print(c)

r <- rowSums(A)
print(r)

a <- sum(A)
print(a)

## Computing Column & Row Means

# note the uppercase M

cm <- colMeans(A)
print(cm)

rm <- rowMeans(A)
print(rm)

m <- mean(A)
print(m)

## Horizontal Concatenation

print(A)

B <- matrix(c(1,3,2,1,4,2),3,2)
print(B)

C <- cbind(A,B)
print(C)

## Vertical Concatenation (Appending)

C <- rbind(A,B)
print(C)