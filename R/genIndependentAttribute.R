#' Generate synthetic data for an independent attribute
#' @param data the data.frame containing the original data
#' @param independent the column index for the independent attribute in data
#' @param dataPrivacyConstant the scale of the laplace distribution. A higher value
#' means a synthetic data distribution farther from the true data distribution.
#' Defaults to 20/9.
#' @details 
#' We assume that our data is split into different subsets.
#' These subsets are determined by the value of an "independent attribute".
#' Each subset has its own distribution of attributes.
#' 
#' Example: let's say that we have three different attribute, occupation, age, and race
#' Occupation is our category variable
#' So the distribution of age and the distribution of race depend on what Occupation a person is in
#' We assume age and race are independent to each other
#' @return 
#' a vector of synthetic data for the independent attribute
genIndependentAttribute <- function(data, independent, dataPrivacyConstant){
    independentTable <- table(data[independent])  #table with number of occurances of each value of the independent variable
    
    groups <- names(independentTable)  #the values of the independent variable
    counts <- independentTable      #number of occurances of each value
    
    #add laplace noise to the counts
    dPcounts <- sapply(counts, function(x){
        x <- x + deamer::rlaplace(1, b = dataPrivacyConstant)
        if(x<0) x=0
        x
    })
    
    #create a cumulative probability distribution from the dPcounts
    total <- sum(dPcounts)
    probDist <- dPcounts / total
    cumProbDist <- sapply(1:length(probDist), function(x){
        sum(probDist[1:x])
    })
    
    #generate synthetic data from the cumulative probability distributions
    syntheticData <- sapply(1:total, function(x){
        r <- runif(1)                               #r is a random number [0,1)
        group <- groups[which(r<cumProbDist)[1]]    #find first group which r falls under
    })
    
    #return the synthetic data
    syntheticData
}