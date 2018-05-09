setwd('/users/home/dropbox/website/yu/ids3000-capstone/63spr18-ids3000-capstone/finalProjects/DovidSimpser-AviBorgen-AmazonEmployeeChallenge')

test <- read.csv("test.csv", stringsAsFactors = FALSE)
train <- read.csv("train.csv", stringsAsFactors = FALSE)


# s1 - a vector
# s2 - a vector
#
# return TRUE if all values from s1 are contained in s2
# return FALSE otherwise
isSubset <- function ( s1 , s2) {
  all(unique(s1) %in% unique(s2))   # using unique is not necessary but makes this function more efficient
}

# df1 - a data.frame
# df2 - a data.frame with the same column names as df1 but with different rows of data
#
# returns a list that contains TRUE for columns from df1 that are subsets of corresponding columns in df2
#                 and contains FALSE for columns from df1 that are NOT subsets of corresponding columns in df2
areColumnsInDf1SubsetsOfColsInDf2 <- function(df1, df2)  {
  if ( !("data.frame" %in% class(df1)) ||
       !("data.frame" %in% class(df2))  )  {
    stop("df1 and df2 must both be dataframes")
  }
  if (!all( 
        sort(colnames(df1)) == sort(colnames(df2))    
      )){
    stop("df1 and df2 must have the same column names")
  }
  df1_uniques <- lapply(df, unique)
  df2_uniques <- lapply(df2, unique)
  answer <- list()
  lapply(colnames(df1), 
    function(colname){
      # NOTE about <<-
      #
      # The variable "answer" was defined on the line above this lapply. The function
      # inside of this lapply will be called multiple times. Each time it is called it 
      # needs to add a new column to the answer variable.
      #
      # The "double headed arrow <<-  in the line below allows the function to change the
      # variable, answer, that appear OUTSIDE of the function (right above this lapply)
      #
      # see the section entitled "Accessing global variables" at the end of 
      # this webpage:  https://www.datamentor.io/r-programming/environment-scope
      #
      # See the following for the official R documentation of <<-
      # https://stat.ethz.ch/R-manual/R-patched/library/base/html/assignOps.html
      
      answer[[colname]] <<- isSubset(df2_uniques[[colname]], df1_uniques[[colname]])
    }
  )
  
  answer
}

# Test the functions
first <- data.frame( a=c(1,1,2,5), b=c(3,4,3,4))
second <- data.frame(a=c(1,5,5,1), b=c(3,6,4,5))
areColumnsInDf1SubsetsOfColsInDf2 ( second , first)

# Now compare the columns in the train and test datasets. 
# For each column, I want to know if the values in the test dataset
# are all contained in the train dataset. If they are then we can use
# the acutual values from the train dataset to engineer columns for our 
# model. 
#
# It turns out that all of the values in all of the columns
# in test are in train too.
#
# Note that test[,-1]  is the complete test dataframe, minus the first column, i.e. the "id" column
# Note that train[,-1]  is the complete train dataframe, minus the first column, i.e. the "ACTION" column

areColumnsInDf1SubsetsOfColsInDf2 ( test[,-1] , train[, -1])

# It is interesting to know that the opposite is true too. 
# This means that for each column in train, the exact same values 
# appear in the corresponding column for test.
areColumnsInDf1SubsetsOfColsInDf2 ( train[,-1] , test[, -1])

