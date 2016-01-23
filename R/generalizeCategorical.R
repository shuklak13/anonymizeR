#' Generalize a Categorical Column
#'
#' @param x a data.frame
#' @param col the column that is to be generalized - may be numeric (column indices)
#' or character (column names)
#' @param newCategories new categories to replace the old
#' @param oldCategories categories being replaced. If not provided, all categories in the
#' column are used in order of appearance.
#' @param mapping a numeric vector containing a mapping of old categories
#' to new categories. See details for more information.
#' @return data.frame with with specified column generalized
#' @details This function generalizes the categories of a column to a smaller
#' set of categories by merging categories together.
#' The initial column is assumed to be categorical. The function iterates through
#' each of the original categories and replaces all instances of it
#' with the new category it is mapped to.
#'
#' Each element in mapping can be thought of as having two values:
#'      1) The position of the element in mappings, which represents
#'      the index of the old category in unique(x[[col]])
#'      2) The value of the element, which represents
#'      the index of the new category in newCategories
#'
#' These two values determine how old categories are mapped to new categories.
#' Note that the legth of mapping must be equal to the number of unique categories.
#'
#' @family generalize functions
#' @examples
#' maritalStatus <- c("Married", "Single", "Single", "Divorced", "Married", "Divorced")
#' race <- c("Caucasian", "Hispanic", "Black", "Asian", "Other", "Caucasian")
#' education <- c("High School or Less", "High School Grad", "Undergraduate", "Master's", "PhD", "Undergraduate")
#' data <- data.frame(ages, maritalStatus)
#'
#' generalize_categorical(data, 1, newCategories=c("Has Been Married","Has Not Been Married"), mapping=c(1,2,1,1))
#' generalize_categorical(data, 2, newCategories=c("Human"), mapping=c(1,1,1,1))
#' generalize_categorical(data, 3, newCategories=c("High School", "College", "Advanced Degree"), mapping=c(1,1,2,3,3))
#' @export
generalizeCategorical <- function(x, col, newCategories, 
                                  oldCategories = unique(x[[col]]), mapping){

    if(!(col %in% c(1:ncol(x))) & !(col %in% names(x)))
        stop("col must be the index number or the name of a column in x")

    if(length(mapping) < length(oldCategories))
        stop("mapping incomplete - 
             need one mapping for each category currently in data")
    if(length(mapping) > length(oldCategories))
        stop("too many mappings - 
             more mappings than categories currently in data")

    newCol <- as.character(x[[col]])

    #iterate through every old category, replacing it with the new category
    for(oldCatIndex in 1:length(oldCategories)){
        oldCategory <- oldCategories[oldCatIndex]
        newCategory <- newCategories[mapping[oldCatIndex]]
        newCol <- replace(newCol, newCol == oldCategory, newCategory)
    }

    x$col <- as.factor(newCol)

    x
}
