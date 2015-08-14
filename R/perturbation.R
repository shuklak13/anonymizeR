#' Add Random Noise to Data
#'
#' @param x a data.frame
#' @param cols the columns that are to be affected. Must be numeric. 
#' If no value is given, all columns are affected.
#' @param magnitude a shortcut for the potential size of noise - see details
#' @param minNoise the smallest possible value for noise that may be added.
#' If neither minNoise nor minNoiseQuantile is provided, minNoise defaults to 0.
#' @param maxNoise the largest possible value for noise that may be added
#' @param minNoiseQuantile the smallest possible value for noise that may be added.
#' Rather than an actual value, this is a decimal, representing a quantile of the
#' values in that column.
#' If both minNoise and minNoiseQuantile are provided, maxNoise takes priority.
#' @param maxNoiseQuantile the largest possible value for noise that may be added.
#' Rather than an actual value, this is a decimal, representing a quantile of the
#' values in that column.
#' If both maxNoise and maxNoiseQuantile are provided, maxNoise takes priority.
#' @return data.frame with with noise added all elements in specified columns
#' @details Adds a random value between minNoise and maxNoise to each element in each of the 
#' specified columns.
#' If minNoiseQuantile or maxNoiseQuantile is provided instead of minNoise or 
#' maxNoise, then minNoise and maxNoise are calculated for every column based on 
#' the quantiles provided.
#' magnitude is a shortcut for minNoise and maxNoiseQuantile parameters:
#' \itemize{
#'      \item LOW:      minNoise = 0   maxNoiseQuantile = .25
#'      \item MEDIUM:   minNoise = 0   maxNoiseQuantile = .5
#'      \item HIGH:     minNoise = 0   maxNoiseQuantile = .75
#' }
#' This makes magnitude ideal when the user desires to have a noise value between
#' 0 and some quantile for all specified columns.
#' @examples
#' perturbation(data)
#' perturbation(data, 1:5)
#' perturbation(data, magnitude="HIGH")
#' perturbation(data, "Gender", minNoise = 10, maxNoise = 50)
#' perturbation(data, minNoiseQuantile = .1, maxNoise = .9)
#' @export
perturbation <- function(x, cols = 1:ncol(x), magnitude = "MEDIUM", 
                         minNoise = NULL, maxNoise = NULL, minNoiseQuantile = NULL, maxNoiseQuantile = NULL){
    
    legitimateCols <- (cols %in% c(1:ncol(x))) | (cols %in% names(x))
    if(sum(legitimateCols) != length(cols))
        stop("cols must be the indices or names of columns in x")
    
    #determine how much noise to add
    if(is.null(minNoise) & is.null(minNoiseQuantile))
        minNoise <- 0
    
    if(is.null(maxNoise) & is.null(maxNoiseQuantile))
        if(toupper(magnitude) == "LOW") maxNoiseQuantile <- .25 
        else if(toupper(magnitude) == "MEDIUM") maxNoiseQuantile <- .5
        else if(toupper(magnitude) == "HIGH") maxNoiseQuantile <- .75 
        
    #add random noise to each element in each column
    for(col in cols){
        if(is.numeric(x[[col]])){
            if(is.null(minNoise))
                minNoise <- quantile(x[[col]], minNoiseQuantile)
            if(is.null(maxNoise))
                maxNoise <- quantile(x[[col]], maxNoiseQuantile)
            x[col] <- sapply(x[[col]], function(element) element + sample(minNoise:maxNoise, 1) )
        }
        else
            print(paste("Column ",col," not numeric, skipping."))
    }
    
    x
}

#' @describeIn perturbation identical to perturbation
#' @export
randomNoise <- function(x, cols = 1:ncol(x), magnitude = "MEDIUM", 
                            minNoise = NULL, maxNoise = NULL, minNoiseQuantile = NULL, maxNoiseQuantile = NULL){
    perturbation(x, cols, magnitude, minNoise, maxNoise, minNoiseQuantile, maxNoiseQuantile)
}