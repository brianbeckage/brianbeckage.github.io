# 01-Getting Started
# This exercise is designed to get you familiar with our working environment for R 
# and begin to use R.  This exercise will be graded on a satisfactory/unsatisfactory 
# basis and the expectation is that you make a reasonable  attempt at completing 
# the assignment.  
#
# The solutions will be posted after the due date. Examine the solutions 
# for any aspects of the exercise that you were not able to complete.  
# You are allowed to use AI to help with understanding R coding but you should 
# not cut and paste from AI. You should use AI to help you understand the solution 
# and should type the code into R yourself. 

# 01-Getting Started is due on 20 Jan at midnight. Download your R file from
# Posit Cloud and turn it in using the Assignment tool in Brightspace, 
# which should appear in your calendar of upcoming due dates in the homepage for
# this class within Brightspace.


################################################################################
# Below are a set of short exercises.  These are intended
# to be simple exercises to allow you to become familiar with this learning
# environment before we move onto assignments from the textbook next week.

# This flie is a R script, which is a plain text file.  The hash on the left means
# that these lines are comments to be read by people and not the computer.


# 1. Let's compute 2 + 2.  We type 2+2 on the next line and evaluate that line by 
# placing our cursor on line 10 below, then using the pull down menu 'Code' 
# above, then 'run selected line' or 'command return' on my Mac.

2+2


# the result '4' should appear in the 'Console' window below.


# 2. Create a vector that is a sequence of 1,2,and 3 and assign it to an object
# called myVector by running the following line of code

myVector<-c(1,2,3,4,5,6)

# as you did above in (1).  To see the result, run the following line:

myVector

# You should see [1] 1 2 3 4 5 6 in the Console window below.  You could also type
# myVector in the Console directly and then hit return.

################################################################################
# Please continue with the exercises below on your own, attempt to complete items
# 3 to 17 below. Write your solutions in new lines beneath each item and 
# save this file to download and turn in as described above.
################################################################################

# 3. Multiply each element in myVector by 2 and assign this to a new object 
# called myVector2.
myVector2<-myVector*2

# 4 Find the sum of the elements of myVector2.
sum(myVector2)

# 5. Square each element myVector2, call this new vector myVector3 and find 
# the summation of this transformed vector.
myVector3<-myVector^2

# 6.  Make a scatter plot of myVector on the x axis versus myVector3 on the y axis.
plot(x=myVector, y=myVector3)

# 7. Select all elements of the myVector3 that are less than 30.
myVector3[myVector3<30]

# 8. Create a 3 (rows) by 4 (cols) matrix of values 1:12.
myMat<-matrix(1:12,nrow=3,ncol=4)

> myMat
[,1] [,2] [,3] [,4]
[1,]    1    4    7   10
[2,]    2    5    8   11
[3,]    3    6    9   12


# 9. Multiple the component at the location [2,3] and the component at [3,2].
myMat[2,3]*myMat[3,2]

[1] 48


# 10. Name the rows (a,b,c) and the columns (1,2,3,4).

rownames(myMat)<-c('a','b','c')
colnames(myMat)<-c('1','2','3','4')
myMat

1 2 3  4
a 1 4 7 10
b 2 5 8 11
c 3 6 9 12

# OR

dimnames(myMat)<-list(c('a','b','c'),c('1','2','3','4'))
myMat
1 2 3  4
a 1 4 7 10
b 2 5 8 11
c 3 6 9 12

# 11. Create a 3 dimensional array that replicates the matrix created above three 
# times in the 3rd dimension.  Name the dimensions of the array.
my3d<-array(c(rep(1:12,3)),dim=c(3,4,3))
dimnames(my3d)<-list(c('a','b','c'),c('1','2','3','4'),
                     c('1d','2d','3d'))
my3d
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

# 12. Multiply the 2nd row of the 2 dimension by the [3,3] element of the 3rd dimension.
my3d[2,,2]*my3d[3,3,3]

1  2  3  4 
18 45 72 99 

# 13. Create a vector of 1's replicated to be a vector of length 10. 
rep(1,10)
[1] 1 1 1 1 1 1 1 1 1 1

# 14. Create a vector of integers 1 to 100 in steps of 1.
1:100

# OR

seq(from=1, to=100, by=1)

# 15. Create a data frame that combines these three vectors (myVector, myVector2,
# and myVector3) into the columns of a dataframe.
myDataFrame<-data.frame(myVect,myVect^2,myVect^3)
head(myDataFrame)

myVect myVect.2 myVect.3
1      1        1        1
2      2        4        8
3      3        9       27
4      4       16       64
5      5       25      125
6      6       36      216

# 16. Name the columns of the dataframe as: ‘Original’, 'Modified' and ‘Squared’.
colnames(myDataFrame)<-c('Original', 'Modified', 'Squared')
head(myDataFrame)

Original Modified Squared
1        1        1       1
2        2        4       8
3        3        9      27
4        4       16      64
5        5       25     125
6        6       36     216

# 17. Plot the original vs squared columns.
plot(Original~Squared,data=myDataFrame)











