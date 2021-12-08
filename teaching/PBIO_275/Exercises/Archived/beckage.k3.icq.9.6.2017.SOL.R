# 1. Read in the weight data that is posted on webpage as ‘Weight data’.

wtData<-read.table("http://www.uvm.edu/~bbeckage/Teaching/DataAnalysis/Data/wts.txt",header=TRUE)


# 2. What type of object is the file read in as? If it is not a data frame, coerce it to one.

# 3. What are object/data types of each of the columns?

str(myData)

'data.frame':  15 obs. of  3 variables:
  $ gender    : Factor w/ 2 levels "f","m": 2 2 2 1 1 2 1 1 1 1 ...
$ height_in : int  72 67 68 64 66 69 68 63 65 69 ...
$ weight_lbs: int  165 160 160 114 135 148 120 138 135 150 ...


# 4. Plot weight (on y axis) vs height (on x axis) with different colored symbols for each gender.

plot(myData[myData$gender=='f',]$height_in, myData[myData$gender=='f',]$weight_lbs,
     col="red",xlim=range(myData$height_in),ylim=range(myData$weight_lbs), ylab="Wt",xlab="Ht")
points(myData[myData$gender=='m',]$height_in,myData[myData$gender=='m',]$weight_lbs,col="blue",pch=2)

library(lattice)
xyplot(height_in~weight_lbs|gender,data=myData)
detach()


# 5. Write a function to calculate ‘10!’, i.e., 10 factorial (10*9*8*...*1). Show the result for 10 and 20.

myFactorial<-function(x){
  myProd<-1 
  for(i in x:1){ # this causes the index i to vary from 1 to 10
    myProd<-myProd*i
  }
  return(myProd)
  # cat(paste("index= ",i,"; myProd= ",myProd,sep=""),fill=T) #
}

myFactorial2<-function(x){
  return(tail(cumprod(1:x),1))
}


myFactorial(10)
[1] 3628800
myFactorial2(10)
[1] 3628800
factorial(10)
[1] 3628800



# timing them 
library(microbenchmark)

microbenchmark(myFactorial(10))
Unit: microseconds
expr   min     lq    mean median    uq    max neval
myFactorial(10) 2.512 2.7065 3.39323 2.9025 3.072 14.664   100

microbenchmark(myFactorial2(10))
Unit: microseconds
expr    min     lq     mean  median      uq    max neval
myFactorial2(10) 11.445 11.998 12.78863 12.3425 12.7025 51.924   100

microbenchmark(factorial(10))
Unit: nanoseconds
expr min    lq   mean median  uq  max neval
factorial(10) 286 312.5 437.07    362 400 5961   100





