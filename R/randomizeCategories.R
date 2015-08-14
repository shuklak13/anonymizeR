#' Randomize Assignment of Categories to Values in a Column
#'
#' @param x a data.frame
#' @param cols the columns that are to be affected. 
#' If no value is given, all columns are affected.
#' @param magnitude a shortcut for the probability parameter - see details
#' @param probability - the probability that an element will be randomized
#' @return data.frame with some of the elements in specified columns randomized
#' @details By default, each observation has a 50% chance of having its value replaced by
#' the value of another observation in the data. The replaced value is determined randomly
#' following the distribution of the data, so the distribution of categories after randomization
#' should be similar to the distribution of categories before randomization.
#' The chance of replacement can be altered with the probability parameter.
#' magnitude is a shortcut for the probability parameter:
#' \itemize{
#'      \item LOW:      proability  = .25
#'      \item MEDIUM:   probability = .5
#'      \item HIGH:     probability = .75
#' }
#' @examples
#' randomizeCategories(data)
#' randomizeCategories(data, magnitude="LOW")
#' randomizeCategories(data, 1:5, probability = .7)
#' @export
randomizeCategories <- function(x, cols = 1:ncol(x), magnitude = "MEDIUM", probability = NULL){
    
    legitimateCols <- (cols %in% c(1:ncol(x))) | (cols %in% names(x))
    if(sum(legitimateCols) != length(cols))
        stop("cols must be the indices or names of columns in x")
    
    #determine how much noise to add
    if(is.null(probability))
        if(toupper(magnitude) == "LOW") probability = .25
        else if(toupper(magnitude) == "MEDIUM") probability = .5
        else if(toupper(magnitude) == "HIGH") probability = .75
        
    #add random noise
    for(col in cols)
        #for each element there is a random chance that it will be replaced by another element from the distribution.
        #probability determines how often elements are replaced.
        x[col] <- sapply(x[[col]], function(element){
            if(sample(c(1, 100), 1) < probability)
                sample(x[[col]], 1)
            else 
                element
        })
    
    x
}