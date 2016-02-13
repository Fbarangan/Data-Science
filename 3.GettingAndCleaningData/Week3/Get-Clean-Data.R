# Subsetting and Sorting Lecture
set.seed(13435)
X <- data.frame("var1" = sample(1:5), "var2" = sample(6:10), "var3"= sample(11:15))
X <- X[sample(1:5),] ; X$var2[c(1:3)] = NA
X
# Run to see result
        X[,1]
        X[,"var1"]
        X[1:2, "var2"]

#Logical ands and or

        X[X$var1 <= 3 & X$var3 > 11,]
        X[X$var1 <=3 | X$var3> 15,]

# Dealing with missing Values. Will not return the NA's

        X[which(X$var 2>8),]

# Sorting

        sort(X$var1)
        sort(X$var1, decreasing = TRUE)

# will sort and put NA last
        sort(X$var2, na.last = TRUE)

# Ordering(also multiple) the first column close to the "order functon will be ordered first

        X[order(X$var1, X$var3) ,]

# Ordering with plyr

        library(plyr)
        arrange(X, var1)

# Add desc function
        arrange(X, desc(var1))

# Adding rows and column

        X$var4 = rnorm(5)

# By using column bind cbind when X is place on the left, then you see it in left and vice-versa
        Y <- cbind(X, rnorm(5))
        Y <- cbind(rnorm(5), X)

#
