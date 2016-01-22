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
genDependentAttribute <- function(data, independent, dependent, independentCol,
                                  dataPrivacyConstant){

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
            newx <- x + deamer::rlaplace(1, b = dataPrivacyConstant)
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