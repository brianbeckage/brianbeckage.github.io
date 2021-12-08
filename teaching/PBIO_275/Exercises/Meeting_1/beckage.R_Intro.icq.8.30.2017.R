# setwd("~/Documents/Teaching/PBIO294_DataAnalysis")
setwd("~/Documents/Documents - Brian’s Mac Pro/Web/Teaching/PBIO_294/Exercises")

# 1. Create a vector that is a sequence of from 1 to 100 and that is of length 200.
myV<-seq(from=1,to=100,len=200)

# 2.  Multiply each element in this list by 2 and find the sum of the resulting vector.
sum(myV*2)
[1] 20200

# 3. Create a sequence of integers from 1 to 100.  
myV<-1:100

# 4. Square each element of this vector and find the summation of this transformed vector.
sum(myV^2)
[1] 338350

# 5. Select all elements of the transformed vector (from 4 above) that are less than 50.
myV<-myV^2
myV[myV<50]
[1]  1  4  9 16 25 36 49

# 6.  Create a 3 (rows) by 4 (cols) matrix of values 1:12
myMat<-matrix(1:12,nrow=3,ncol=4)

      [,1] [,2] [,3] [,4]
[1,]    1    4    7   10
[2,]    2    5    8   11
[3,]    3    6    9   12

# 7.  Multiple the component at the location [2,3] and the component at [3,2].
myMat[2,3]*myMat[3,2]
[1] 48

# 8. Name the rows (a,b,c) and the columns (1,2,3,4).
rownames(myMat)<-c('a','b','c')
colnames(myMat)<-c('1','2','3','4')

  1 2 3  4
a 1 4 7 10
b 2 5 8 11
c 3 6 9 12

# or

dimnames(myMat)<-list(c('a','b','c'),c('1','2','3','4'))
myMat

# 9. Create a 3 dimensional array that replicates the matrix created in 4 above 
# three times in the 3rd dimensions.  Continue the sequence of numbers to 48. 
# Name the dimensions of the array.

my3d<-array(1:48,dim=c(3,4,4))
dimnames(my3d)<-list(c('a','b','c'),c('1','2','3','4'),
                     c('1d','2d','3d','4d'))

, , 1d

  1 2 3  4
a 1 4 7 10
b 2 5 8 11
c 3 6 9 12

, , 2d

  1  2  3  4
a 13 16 19 22
b 14 17 20 23
c 15 18 21 24

, , 3d

  1  2  3  4
a 25 28 31 34
b 26 29 32 35
c 27 30 33 36

, , 4d

  1  2  3  4
a 37 40 43 46
b 38 41 44 47
c 39 42 45 48

# 10.  Multiply the 2nd row of the 2 dimension by the [3,3] element of the 4th dimension.

my3d[2,,2]*my3d[3,3,4]
1    2    3    4 
630  765  900 1035 


# 11. Create a vector of (1,2) replicated to length 10.  Transform the vector to a factor. 
# Change the names of the level 1 to ‘low’ and level 2 to ‘high’

myFactor<-rep(c(1,2),5)
myFactor<-as.factor(myFactor)

[1] 1 2 1 2 1 2 1 2 1 2
Levels: 1 2

# Changing names of 1 to 'low', and 2 to 'high'
levels(myFactor)<-c('low','high')
myFactor
[1] low  high low  high low  high low  high low  high
Levels: low high

# 12. Create a vector of integers 1 to 10.  Create a data frame that 
# combines this vector with the factor variable created in 9 above.
myVect<-1:10
myDataFrame<-data.frame(myVect,myFactor)

# 13. Name the columns of 10: ‘id’, ‘treatment’.
colnames(myDataFrame)<-c('id','treatment')
head(myDataFrame)

class(myDataFrame)
[1] "data.frame"
is.data.frame(myDataFrame)
[1] TRUE

# 14.  Extract positions 1,2,3 and 7 of the treatment column.

myDataFrame$treatment[c(1:3,7)]
[1] low  high low  low 
Levels: low high

myDataFrame[[2]][c(1:3,7)]
[1] low  high low  low 
Levels: low high

