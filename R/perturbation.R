#' Perturb Data
#'
#' @param x a data.frame
#' @param cols the columns that are to be affected. If no value is given,
#' all columns are affected.
#' @param method the type of perturbation method to use. May be "VALUE", "PROBABILITY",
#' or "BOTH." "BOTH" is equivalent to c("VALUE", "PROBABILITY"). When both methods
#' are selected, numeric columns are value perturbated, while other columns are
#' probability perturbated. For the difference between value and probability
#' perturbation, see details.
#' @param magnitude a shortcut for the minNoise, maxNoiseQuantile, and probability
#' parameters - see details
#' @param minNoise the smallest possible value for noise that may be added.
#' If neither minNoise nor minNoiseQuantile is provided, minNoise defaults to 0.
#' Used in value perturbation.
#' @param maxNoise the largest possible value for noise that may be added.
#' Used in value perturbation.
#' @param minNoiseQuantile the smallest possible value for noise that may be added.
#' Rather than an actual value, this is a decimal, representing a quantile of the
#' values in that column.
#' If both minNoise and minNoiseQuantile are provided, maxNoise takes priority.
#' Used in value perturbation.
#' @param maxNoiseQuantile the largest possible value for noise that may be added.
#' Rather than an actual value, this is a decimal, representing a quantile of the
#' values in that column.
#' If both maxNoise and maxNoiseQuantile are provided, maxNoise takes priority.
#' Used in value perturbation.
#' @param probability - the probability that an element will be randomized. Used in
#' probability perturbation.
#' @return data.frame with with specified perturbation methods applied on specified columns
#' @details
#' Perturbation is a form of data anonymization where elements are modified in
#' order to mask their true values without significant altering the overall
#' distribution of values.
#' There are two methods of perturbation: value perturbation and probability
#' perturbation.
#'
#' In value perturbation, a random value between minNoise and maxNoise is added to
#' each element. If minNoiseQuantile or maxNoiseQuantile is provided instead of
#' minNoise or maxNoise, then minNoise and maxNoise are calculated for every
#' column using the quantiles provided. Value perturbation may only be applied
#' to numeric data.
#'
#' In probability perturbation, elements are randomly replaced with the values of
#' other elements within the same data set, so that the overall probability remains
#' mostly similar. By default, each observation has a 50% chance of having its value
#' replaced by the value of another observation in the data. The chance of
#' replacement can be altered with the probability parameter.
#'
#' If both value perturbation and probability perturbation are selected, value
#' perturbation is used on numeric columns, while probability perturbation is
#' used on all other columns.
#'
#' magnitude is a user-friendly shortcut for the minNoise, maxNoiseQuantile, and
#' probability parameters:
#' \itemize{
#'      \item LOW:      minNoise = 0   maxNoiseQuantile = .25   proability  = .25
#'      \item MEDIUM:   minNoise = 0   maxNoiseQuantile = .5    probability = .5
#'      \item HIGH:     minNoise = 0   maxNoiseQuantile = .75   probability = .75
#' }
#'
#' @examples
#' perturbation(data)
#' perturbation(data, 1:5)
#' perturbation(data, magnitude="HIGH")
#' perturbation(data, "Gender", minNoise = 10, maxNoise = 50)
#' perturbation(data, minNoiseQuantile = .1, maxNoise = .9)
#' randomizeCategories(data, 1:5, probability = .7)
#' @export
perturbation <- function(x, cols = 1:ncol(x),
                         method = c("VALUE", "PROBABILITY"),
                         magnitude = "MEDIUM",
                         minNoise = NULL, maxNoise = NULL,
                         minNoiseQuantile = NULL, maxNoiseQuantile = NULL,
                         probability = NULL){

    if(sum(method == "BOTH") > 0)
        method <- c("VALUE", "PROBABILITY")

    for(m in method)
        if(!(toupper(m) %in% c("VALUE","PROBABILITY")))
            stop("method must be \"VALUE\" or \"PROBABILITY\"")
    if(!(toupper(magnitude) %in% c("LOW","MEDIUM","HIGH")))
        stop("magnitude must be \"LOW\", \"MEDIUM\", or \"HIGH\"")

    legitimateCols <- (cols %in% c(1:ncol(x))) | (cols %in% names(x))
    if(sum(legitimateCols) != length(cols))
        stop("cols must be the indices or names of columns in x")

    #if min max, or probability were not set, use magnitude to determine
    if(is.null(minNoise) & is.null(minNoiseQuantile))
        minNoise <- 0
    if(is.null(maxNoise) & is.null(maxNoiseQuantile))
        if(toupper(magnitude) == "LOW") maxNoiseQuantile <- .25
        else if(toupper(magnitude) == "MEDIUM") maxNoiseQuantile <- .5
        else if(toupper(magnitude) == "HIGH") maxNoiseQuantile <- .75
    if(is.null(probability))
        if(toupper(magnitude) == "LOW") probability <- .25
        else if(toupper(magnitude) == "MEDIUM") probability <- .5
        else if(toupper(magnitude) == "HIGH") probability <- .75

    for(col in cols){
            #add random noise
        if(is.numeric(x[[col]]) & sum(toupper(method) == "VALUE") > 0){
            print(paste("Value perturbation on column", names(x[col])))
            if(is.null(minNoise))
                minNoise <- quantile(x[[col]], minNoiseQuantile)
            if(is.null(maxNoise))
                maxNoise <- quantile(x[[col]], maxNoiseQuantile)
            x[col] <- sapply(x[[col]], function(element)
                                        element + sample(minNoise:maxNoise, 1))
        }
            #For each element, there is a random chance that it will be 
                #replaced by another element from the distribution.
            #probability determines how often elements are replaced.
        else if(sum(toupper(method) == "PROBABILITY") > 0){
            oldVals <- x[[col]]

            x[col] <- sapply(x[[col]], function(element){
                if(sample(1:100, 1) < probability  * 100)
                    sample(oldVals, 1)
                else
                    element
            })
        }
    }

    x
}
