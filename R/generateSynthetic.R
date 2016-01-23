#if(!("deamer" %in% installed.packages()[,"Package"])) install.packages("deamer")
#library(deamer)

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
.generateSynthetic <- function(data, independent = 1){
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

#' Generate random val from a distribution
#' @param values the possible values of the attribute
#' @param cPD the cumulative probability distribution for the values
selectFromCPD <- function(values, cPD){
    
    r <- runif(1) #random[0,1)
    
    values[which(r<cPD)[1]]
}
