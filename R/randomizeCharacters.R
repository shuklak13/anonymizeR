#' Replace Data with Random Character Strings
#'
#' @param x a data.frame
#' @param cols the columns that are to be replaced - may be numeric (column indices) 
#' or character (column names). If no value is given, all columns are replaced.
#' @param randomizeNames whether or not column names should be randomized as well
#' @param consistent whether or not reocurring values continue to hold the same value as
#' each other after they are randomized
#' @param numChars the number of characters the randomized data should contain
#' @return data.frame with with specified columns replaced by random strings
#' @details Replaces all values within the specified columns with random 10-length 
#' character strings. 
#' By default, if a value reoccurs within the column, that value will be assigned 
#' the same random character string each time. Column names are also randomized.
#' Both of these options may be changed.
#' @examples
#' randomCharacters(data)
#' randomCharacters(data, 2, consistent=FALSE)
#' randomCharacters(data, c("Gender","Education"), randomizeNames=TRUE)
#' @export
randomizeCharacters <- function(x, cols = 1:ncol(x), randomizeNames = FALSE, consistent = TRUE, numChars = 10){
    
    if(!(cols %in% c(1:ncol(x))) & !(cols %in% names(x)))
        stop("cols must be the indices or names of columns in x")
    
    #function generates a random sequence of numChars characters
    randChars <- function() paste(sample(LETTERS, numChars), collapse = "")
    
    if(randomizeNames)
        for(i in cols)
            names(x)[i] <- randChars()
       
    #replace columns with random characters 
    if(consistent)
        x[cols] <- lapply(x[cols], function(column){
            column <- as.character(unlist(column))
            
            #create a random character replacement for each unique element
                #so elements that reoccur are mapped to the same random character sequence
            uniqueElements <- unique(as.character(unlist(column)))
            randomReplacements <- uniqueElements
            for(i in 1:length(randomReplacements))
                randomReplacements[i] <- randChars()
            
            for(c in 1:length(column)){
                #find that element's random replacement, and insert it into the column
                index <- match(column[c], uniqueElements)
                column[c] <- randomReplacements[index]
            }
                
            column
        })
    else
        x[cols] <- lapply(x[cols], function(column){
            column <- as.character(unlist(column))
            for(c in 1:length(column))
                column[c] <- randChars()
            column
        })
    
    x
}
