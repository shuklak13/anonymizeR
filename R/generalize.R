#' Generalize a Column
#'
#' @param x a data.frame
#' @param col the column that is to be generalized - may be numeric (column indices) 
#' or character (column names)
#' @param splits numeric - numeric vector containing the numbers which should serve as
#' boundary points between each numeric category. Either this or numSplits is a
#' required argument for numeric generalization.
#' @param numSplits numeric - number of splits desired. Either this or numeric is a
#' required argument for numeric generalization
#' @param rightClosed numeric - should boundaries be closed right? False by default
#' @param newCategories categorical - new categories to replace the old
#' @param mapping categorical - a numeric vector containing a mapping of old categories
#' to new categories. See details for more information.
#' @return data.frame with with specified column generalized
#' @details This function calls either generalize_numeric or generalize_categorical
#' depending on which arguments are provided.
#' splits or numSplits are required for generalize_numeric, so if either of 
#' these arguments are provided, generalize_numeric will be called.
#' newCategories and mapping are required for generalize_categorical. If 
#' neither splits nor numSplits is provided, and both newCategories and mapping are 
#' provided, then generalize_categorical is called.
#' @family generalize functions
#' @examples
#' age <- c(51, 42, 23, 44, 25)
#' maritalStatus <- c("Married", "Single", "Single", "Divorced", "Married")
#' data <- data.frame(age, maritalStatus)
#' 
#' generalize(data, 1, splits=c(30, 50))
#' generalize(data, 2, newCategories=c("Has Been Married","Has Not Been Married"), mapping=c(1,2,1,1))
#' @export
generalize <- function(x, col, 
                           splits = NULL, numSplits = NULL, rightClosed = FALSE,
                           newCategories = NULL, mapping = NULL){
    
    if(!is.null(splits) | !is.null(numSplits))
        generalize_numeric(x, col, splits, numSplits, rightClosed)
    else if(!is.null(newCategories) & !is.null(mapping))
        generalize_categorical(x, col, newCategories=newCategories, mapping=mapping)
    else stop("Incorrect arguments \neither provide splits argument for numeric generalization,
              \nor provide oldCategories, newCategories, and mapping arguments for categorical generalization")
}