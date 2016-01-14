if(!("deamer" %in% installed.packages()[,"Package"])) install.packages("deamer")
library(deamer)

#' Generate synthetic data
#'
#' @param data the data.frame containing the original data
#' @param independent the column index for the independent attribute. Defaults to 1. See details.
#' @details 
#'  To generate synthetic data and ensure an appearance of realism, we must
#'  ensure that relationships between major attributes stay consistent.
#'  For example, if our data set contains employee information with age, 
#'  job title, and location, it would be strange if our synthetic data had several 
#'  Senior Product Managers in their early twenties living in college towns.
#'  
#'  This is where the "independent attribute" comes in. The independent attribute
#'  determines the distribution of the other "dependent" attributes.
#'  For each entity in the synthetic data set, the value of the independent attribute 
#'  is generated randomly based on the distribution in the original data.
#'  Then, to generate the dependent attributes, we choose the value randomly from
#'  its distribution, given the independent attribute.
#'  For example, suppose our data set contained the attributes "Job Title", 
#'  "Age", and "Location". We randomly select the "Senior Programmer" job title.
#'  In our data set, Senior Programmers mostly are above 40 years old and live in
#'  San Francisco. So the Senior Programmers in our synthetic datra will reflect
#'  this distribution.
#'  
#'  Note that all attributes are treated as categorical.
#' @return a data.frame of synthetic data
generateSynthetic <- function(data, independent = 1){
    dataPrivacyConstant <- 2/0.9    #may be changed at a later release if necessary

    independentCol <- data.frame(genIndependentAttribute(data, independent, dataPrivacyConstant))
    
    #we grow the synthetic data column-by-column
    for(i in 1:ncol(data)){
        print(i)
        
        #genAttribute() fills in the dependent columns
        if(i==independent){
            newCol <- independentCol
        }else{
            newCol <- genDependentAttribute(data, independent, i, independentCol, dataPrivacyConstant)
        }
        
        #the first column initializes the data frame
        if(i==1){
            syntheticData <- data.frame(newCol)
        }else{
            syntheticData[ , i] <- newCol
        }
    }
    
    if(independent!=1){
        originalColumnOrder <- c(2:independent, 1, (independent+1):ncol(data))    #insert the independent column back in its original position
        syntheticData <- syntheticData[, originalColumnOrder]
    }
    
    names(syntheticData) <- names(data)
    
    syntheticData
}

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
        x <- x + rlaplace(1, b = dataPrivacyConstant)
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

#' Generate synthetic data for a dependent attribute
#' @param data the data.frame containing the original data
#' @param independent the column index for the independent attribute in data
#' @param dependent the column index for the dependent attribute in data
#' @param independentCol the synthetic data for the independent atribute
#' @param dataPrivacyConstant the scale of the laplace distribution. A higher value
#' means a synthetic data distribution farther from the true data distribution.
#' Defaults to 20/9.
#' @return 
#' a vector of synthetic data for the dependent attribute
genDependentAttribute <- function(data, independent, dependent, independentCol, dataPrivacyConstant){
    
    #we assume data is categorical
    data[dependent] <- as.factor(unlist(data[dependent]))
    
    #unique independent attribute values
    independentVals <- unique(unlist(data[independent]))
    
    #iterate through each of the independentVals
        #at each one, return a vector of dependent values for that independent value
    dependentsForIndependents <- lapply(independentVals, function(independentVal){
            filteredRows <- unlist(data[independent]) == independentVal
            data[filteredRows, dependent]
    })
    names(dependentsForIndependents) <- independentVals
    
    #create a matrix from dependentsForIndependents
        #Cols: each unique value of the independent attribute
        #Rows: number of occurances of each dependent value at that independent value
    counts <- sapply(dependentsForIndependents, table)
    
    #add laplace noise to the counts
    dPcounts <- apply(counts, c(1, 2), function(x){  #x = num of occurances of value in distribution
        oldx <- x
        newx <- x
        if(x!=0) {                          #we only want to add noise to the values that actually occur
            newx <- x + rlaplace(1, b = dataPrivacyConstant)
            if(newx<=0) newx <- x/1000      #we don't want to "lose" a value
            #If it falls <=0, then set the probability to a very small value but keep it
            #if x is allowed to become 0, we may have every value have a 0% chance of occuring - and that's no good!
        }
        newx
    })
    
    #for each distribution, convert the counts to cumulative probability distributions 
    probDist <- apply(dPcounts, 2, function(col) col/sum(col))
    cumProbDist <- apply(probDist, 2, cumsum)
    
    #unique dependent values
    dependentVals <- rownames(cumProbDist)
    
    #generate new column from the cumulative probability distributions
    dependentCol <- sapply(unlist(independentCol), function(independentVal){    #iterate over independent column
        
        #cumulative probability distribution of dependent attribute for this independent value
        cPDforthisindependent <- cumProbDist[, toString(independentVal)]
        
        #generate random val from the attribute's distribution
        r <- runif(1)   #random[0,1)
        dvIndex <- which(r<cPDforthisindependent)[1] #index of first dependent value r falls under
        dependentVals[dvIndex]    #add this value to the synthetic dependent column
    })
    
    #return the synthetic data
    dependentCol
}