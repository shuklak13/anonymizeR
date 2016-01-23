#' Suppress Identifiable Information
#'
#' @param x a data.frame
#' @param rows the rows that are to be removed - may be numeric (row indices) or
#' character (row names). An optional parameter - use if automated suppression is
#' not desired
#' @param cols the columns that are to be removed - may be numeric (column indices)
#' or character (column names). An optional parameter - use if automated suppression
#' is not desired
#' @param over use "row" or "rows" if suppression of rows is desired, "col" or "cols is
#' suppression of columns is desired, or "both" for suppression of both
#' @param order the order of suppression. See details
#' @return data.frame with with rows and/or columns removed
#' @details When anonymizing data, it is often desirable to remove variables that allow for
#' individuals to be easily identified. It may also be desirable to remove
#' individuals from the data if they possess unique attributes that make them easy
#' to identify. This function assists in anonymization by removing rows or columns
#' that fall under these categories.
#' The rows and columns that are desired to be removed may be specified as a
#' parameter, in which case the function simply removes the specified rows and
#' columns. If no rows or columns are supplied, the function will remove all rows
#' containing completely unique observations and/or columns containing
#' identifiable information.
#' The order parameter is used to increase the anonymity of the data. It can be
#' thought of as the minimum number of times a value must occur for it to be
#' considered unidentifiable.
#' If a row or column contains a variable that does not occur
#' at least (order) number of times, that row or column is removed.
#' @examples
#' suppress(data)
#' suppress(data, over=c("rows"))
#' suppressRows(data, rows=c(29,34,70))
#' suppressCols(data, order=3)

#' @describeIn suppress Shortcut function that may execute suppressRows, suppressCols, or both
#' @export
suppress <- function(x, rows = NULL, cols = NULL, over = c("both"), order = 1){
    if("row" %in% over | "rows" %in% over | "both" %in% over)
        suppressRow(x, rows, order)
    if("col" %in% over | "cols" %in% over | "both" %in% over)
        suppressCol(x, cols, order)
}

#' @describeIn suppress Suppress rows containing unique observations
#' @export
suppressRows <- function(x, rows = NULL, order = 1){

    if(!is.null(rows & !(rows %in% c(1:nrow(x))) & !(rows %in% names(x))))
       stop("rows must be the indices or names of rows in x")

        #if rows are specified, remove them
    if(!is.null(rows)){
        if(is.character(rows)) suppressedX <- x[!rownames(x) %in% rows,]
        else if(is.numeric(rows)) suppressedX <- x[-rows,]
    }

        #if rows are not specified, find rows with insufficient number of occurances
        #and remove them
    else{
        rowValues <- apply(x, 1, function(x) paste(x, collapse = ""))
        numOccurances <- table(rowValues)
        uniques <- names ( numOccurances[which(numOccurances <= order)] )
        rows <- which(rowValues %in% uniques)

        suppressedX <- x[-rows,]
    }

    suppressedX
}

#' @describeIn suppress Suppress columns with Identifying Values
#' @export
suppressCols <- function(x, cols = NULL, order = 1){

    if(!is.null(cols & !(cols %in% c(1:ncol(x))) & !(cols %in% names(x))))
        stop("cols must be the indices or names of columns in x")

        #if columns unspecified, find columns with elements occuring an insufficient number of times
    if(is.null(cols))
        for(colname in names(x)){
            numOccurances <- table(x[colname])
            numUniques <- sum(numOccurances <= order)
            if(numUniques != 0)
                cols <- c(cols, colname)
        }

        #if no columns were identified, nothing needs to be removed
    if(is.null(cols))
        suppressedX <- x

        #otherwise, remove identified columns
    else{
        if(!is.numeric(cols))
            cols <- which(names(x) %in% cols)
        suppressedX <- x[-cols]
    }

    suppressedX
}

#' @describeIn suppress identical to suppressCols
#' @export
suppressColumns <- function(x, cols = NULL, order = 1)   suppressCols(x, cols, order)
