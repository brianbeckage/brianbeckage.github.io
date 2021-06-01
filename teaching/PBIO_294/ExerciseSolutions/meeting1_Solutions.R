# PBIO 294 Exercise Meeting 1.          26 May 2021

# 1. Create a vector that is a sequence of from 1 to 100 and that is of length 200.
myV<-seq(from=1,to=100,len=200)

# 2.  Multiply each element in this list by 2 and find the sum of the resulting vector.
sum(myV*2)
[1] 20200

# 3. Square each element of this vector and find the summation of this transformed vector.
sum(myV^2)
[1] 338350

# 4. Select all elements of the transformed vector (from above) that are less than 50.
myV<-seq(from=1,to=100,len=200)
myV<-myV^2
myV[myV<50]
[1]  1  4  9 16 25 36 49

# 5.  Create a 3 (rows) by 4 (cols) matrix of values 1:12
myMat<-matrix(1:12,nrow=3,ncol=4)

[,1] [,2] [,3] [,4]
[1,]    1    4    7   10
[2,]    2    5    8   11
[3,]    3    6    9   12

# 6. Multiple the component at the location [2,3] and the component at [3,2].
myMat[2,3]*myMat[3,2]
[1] 48

# 7. Name the rows (a,b,c) and the columns (1,2,3,4).
rownames(myMat)<-c('a','b','c')
colnames(myMat)<-c('1','2','3','4')

1 2 3  4
a 1 4 7 10
b 2 5 8 11
c 3 6 9 12

# or

dimnames(myMat)<-list(c('a','b','c'),c('1','2','3','4'))
myMat

# 8. Create a 3 dimensional array that replicates the matrix created above 
# three times in the 3rd dimensions. Name the dimensions of the array.

my3d<-array(c(rep(1:12,3)),dim=c(3,4,3))
dimnames(my3d)<-list(c('a','b','c'),c('1','2','3','4'),
                     c('1d','2d','3d'))

, , 1d

1 2 3  4
a 1 4 7 10
b 2 5 8 11
c 3 6 9 12

, , 2d

1 2 3  4
a 1 4 7 10
b 2 5 8 11
c 3 6 9 12

, , 3d

1 2 3  4
a 1 4 7 10
b 2 5 8 11
c 3 6 9 12

# 9.  Multiply the 2nd row of the 2 dimension by the [3,3] element of the 4th dimension.

my3d[2,,2]*my3d[3,3,3]
1  2  3  4 
18 45 72 99 


# 10. Create a vector of (1,2) replicated to length 10. 
rep(c(1,2),5)
[1] 1 2 1 2 1 2 1 2 1 2

# 11. Create a vector of integers 1 to 10 and then create vectors 
# that are the square and cube of that vector. 
myVect<-1:10
myVect^2
myVect^3

# 12. Create a data frame that combines these three vectors into 
# the columns of a dataframe.
myDataFrame<-data.frame(myVect,myVect^2,myVect^3)

head(myDataFrame)

# 13 Name the columns of the data frame as: ‘Original’, ‘Squared’, and ‘Cubed’.
colnames(myDataFrame)<-c('Original', 'Squared', 'Cubed')

# 14. Plot the original vs cubed columns.
plot(Original~Cubed,data=myDataFrame)

