#THINGS TO FIX:
    #CURRENTLY, BASELINE != 1 DOES NOT WORK
    #DOES NOT WORK WITH THE as.data.frame(hairEyeColor) DATA SET

#' Generate synthetic data
#'
#' @param data the data.frame containing the original data
#' @param baseline the column index for the baseline attribute. Defaults to 1. See details.
#' @details 
#'  To generate synthetic data and ensure an appearance of realism, we must
#'  ensure that relationships between major attributes stay consistent.
#'  For example, if our data set contained employee information with age and 
#'  job title, it would be strange if our synthetic data had several 
#'  Senior Product Managers in their early twenties.
#'  
#'  This is where the "baseline attribute" comes in. The baseline attribute
#'  determines the distribution of the other attributes. When generating a
#'  synthetic data point, the value of the baseline attribute is chosen
#'  randomly based on the distribution in the original data.
#'  Then, to generate every other attribute, we choose the value randomly from
#'  its distribution, given the baseline attribute.
#'  For example, suppose our data set contained the attributes "Job Title", 
#'  "Age", and "Location". We randomly select the "Senior Programmer" job title.
#'  In our data set, Senior Programmers mostly are above 40 years old and live in
#'  San Francisco. So the Senior Programmers in our synthetic datra will reflect
#'  this distribution.
#'  
#'  Note that all attributes are treated as categorical.
#' @return a vector of synthetic data

if(!("deamer" %in% installed.packages()[,"Package"])) install.packages("deamer")
library(deamer)

generateSynthetic <- function(data, baseline = 1){
    dataPrivacyConstant <- 2/0.9    #may be changed at a later release if necessary
    
    #create a distribution of elements for the first column
    syntheticData <- data.frame(genIndependentAttribute(data, baseline, dataPrivacyConstant))
    
    #we grow the synthetic data column-by-column over the non-base attributes
    nonBaseAttributes <- setdiff(1:ncol(data), baseline)
    for(i in nonBaseAttributes){
        print(i)
        generatedAttribute <- genAttribute(data, baseline, i, syntheticData[1], dataPrivacyConstant)
        syntheticData[ , i] <- generatedAttribute
    }
    
    if(baseline!=1){
        originalColumnOrder <- c(2:baseline, 1, (baseline+1):ncol(data))    #insert the baseline column back in its original position
        syntheticData <- syntheticData[, originalColumnOrder]
    }
    
    names(syntheticData) <- names(data)
    
    syntheticData
}

#' We assume that our data is split into different subsets.
#' These subsets are determined by the value of an "independent attribute".
#' Each subset has its own distribution of attributes.
#' 
#' Example: let's say that we have three different attribute, occupation, age, and race
#' Occupation is our category variable
#' So the distribution of age and the distribution of race depend on what Occupation a person is in
#' We assume age and race are independent to each other
genIndependentAttribute <- function(data, baseline, dataPrivacyConstant){
    baselineTable <- table(data[baseline])  #table with number of occurances of each value of the baseline variable
    
    groups <- names(baselineTable)  #the values of the baseline variable
    counts <- baselineTable      #number of occurances of each value
    
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

#Each new column is generated based on the categories 
genAttribute <- function(data, baseline, attrIndex, baselineVals, dataPrivacyConstant){
    
    #generation works on the assumption that data is categorical
    data[attrIndex] <- as.factor(unlist(data[attrIndex]))
    
    #each of the unique baseline values has its own distribution
    distributions <- unique(unlist(data[baseline]))
    
    #iterate through each of the distributions (unique values of baseline)
        #at each one, return a vector of values for the attribute
    valAtEachDistribution <- sapply(distributions, function(baselineVal){
        #sameBaselineRows <- data[baseline] == baselineVal
        sameBaselineRows <- unlist(data[baseline]) == baselineVal
        data[sameBaselineRows, attrIndex]
    })
    
    names(valAtEachDistribution) <- distributions
    
    #create a matrix from valAtEachDistribution
        #Rows: number of occurances for each value of the new attribute
        #Cols: each distribution / each unique value of the baseline attribute
    counts <- sapply(valAtEachDistribution, table)
    attrVals <- rownames(counts)
    
    #add laplace noise to the counts
    dPcounts <- apply(counts, c(1,2), function(x){  #x = num of occurances of value in distribution
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
    
    #generate synthetic data from the cumulative probability distributions
    newColumn <- sapply(unlist(baselineVals), function(baselineVal){    #iterate over the values for the baseline column
        #print(baselineVal)
        cPDforthisBaseline <- cumProbDist[, toString(baselineVal)]  #the cumulative probability distribution for this baseline value
        
        #generate random val from the attribute's distribution
        r <- runif(1)                               #r is a random number [0,1)
        attVal <- attrVals[which(r<cPDforthisBaseline)[1]]  #find first group which r falls under
    })
    
    #return the synthetic data
    newColumn
}