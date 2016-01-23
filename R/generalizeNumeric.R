#' Generalize a Numeric Column
#'
#' @param x a data.frame
#' @param col the column that is to be generalized - may be numeric (column indices)
#' or character (column names)
#' @param splits numeric vector containing the numbers which should serve as
#' boundary points between each numeric rangeory. Either splits or numSplits is a
#' required argument for numeric generalization. If splits is provided, then
#' numSplits is ignored.
#' @param numSplits number of splits desired. Either splits or numSplits is a
#' required argument for numeric generalization. If splits is provided, then
#' numSplits is ignored.
#' @param rightClosed should boundaries be closed right? False by default
#' @param newranges rangeorical - new ranges to replace the old
#' @return data.frame with with specified column generalized
#' @details The function generalizes a numeric column by converting it into a
#' rangeorical column, in which each rangeory contains a range of numbers.
#' The numbers in splits is used as the boundary points between these ranges.
#' If the numSplits argument is used instead of splits, then numSplit ranges
#' of equal range are created, spanning from the smallest number in the column to
#' the largest.
#' By default, these ranges are left-closed, meaning that a rangeory captures
#' elements that fall on its left boundary but not on its right boundary.
#' rightClosed provides a right-closed option.
#' @family generalize functions
#' @examples
#' age <- c(51, 42, 23)
#' weight <- c(127, 150, 188)
#' IQ <- c(119, 101, 95)
#' data <- data.frame(age, weight, IQ)
#'
#' generalize(data, 1, splits=c(30, 50))
#' generalize(data, 2, splits=180, rightClosed=TRUE)
#' generalize(data, 2, numSplits=2)
#' @export
generalizeNumeric <- function(x, col,
                           splits = NULL, numSplits = 2, rightClosed = FALSE){

    if(!(col %in% c(1:ncol(x))) & !(col %in% names(x)))
        stop("col must be the index number or the name of a column in x")

    if(!is.numeric(x[[col]])) stop("This column is not numeric. It cannot be generalized numerically.")

    if(is.null(splits) & is.null(numSplits)) stop("Either splits or numSplits must be provided.")

    #If number of splits is provided instead of the splits themselves,
    #calculate the splits using quantiles
    if(is.null(splits) & is.numeric(numSplits)){
        inc <- 1 / (numSplits+1)
        quants <- inc
        while(quants[length(quants)] + inc <= 1)
            quants <- c(quants, quants[length(quants)] + inc)
        splits <- quantile(x[[col]], quants[-length(quants)])
    }

    #ranges is a vector containing the name of each range
    ranges <- character(length = length(splits) + 1)

    #left-closed ranges
    if(!rightClosed){
        ranges[1] <- paste("x <", splits[1])
        for(s in 2:(length(splits)))
            ranges[s] <- paste(splits[s-1], "<= x <", splits[s])
        ranges[length(ranges)] <- paste(splits[length(splits)], "<= x")
    }
    #right-closed ranges
    else if(rightClosed){
        ranges[1] <- paste("x <=",splits[1])
        for(s in 2:(length(splits)))
            ranges[s] <- paste(splits[s-1],"< x <=",splits[s])
        ranges[length(ranges)] <- paste(splits[length(splits)],"< x")
    }

    #replace each value with the range it falls into
    newCol <- sapply(x[[col]], function(num){
        range <- NULL
        if(!rightClosed){
            if(num < splits[1])
                range <- ranges[1]
            else if (splits[length(splits)] <= num)
                range <- ranges[length(ranges)]
            else
                for(c in 2:(length(ranges) - 1))
                    if(splits[c-1] <= num & num < splits[c]){
                        range <- ranges[c]
                        break
                    }
        }
        else if(rightClosed){
            if(num <= splits[1])
                range <- ranges[1]
            else if (splits[length(splits)] < num)
                range <- ranges[length(ranges)]
            else
                for(c in 2:(length(ranges)-1))
                    if(splits[c-1] < num & num <= splits[c]){
                        range <- ranges[c]
                        break
                    }
        }
        range
    })

    x$col <- as.factor(newCol)

    x
}
