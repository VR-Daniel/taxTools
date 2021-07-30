#' Summarize taxonomic information
#'
#' @description \code{summary.Tax} summarize the taxonomic information contained in a table.
#' @param data A \code{"data.frame"} object.
#' @param columnIndex A numeric vector of length 1 to 4 indicating the position of the Species, Genus, and Family columns in the data frame. By default \code{"columnIndex = c(5, 4, 3)"} according to the table structure of the \code{websearch.Tax()} output.
#' @export summary.Tax
summary.Tax <- function(data, columnsIndex){
  
  # Arguments validation ------
  if(missing(data)){
    cat("\n")
    stop("Missing argument with no default value\n\n", call. = FALSE)
  }
  if(missing(columnsIndex)){
    columnsIndex = c(5, 4, 3)
  }
  if(is.data.frame(data) == FALSE){
    cat("\n")
    stop("Input must be a \"data.frame\" object\n\n", call. = FALSE)
  }
  if(is.numeric(columnsIndex) == FALSE | length(columnsIndex) > 4){
    cat("\n")
    stop("columnIndex must be a \"numeric\" vector of lenght 1 to 4 \n\n", call. = FALSE)
  }
  
  # Summary ------
  summMatrix <- matrix(nrow = 3, ncol = 3, dimnames = list(c(seq(1:3)), c("tmp", "Level", "Taxa")))
  summMatrix[, 1] <- c("Families:", "Genus:", "Species:")
  summMatrix[, 2] <- c("Families", "Genus", "Species")
  summMatrix[1, 3] <- length(unique(data[!is.na(data[, columnsIndex[3]]), columnsIndex[3]]))
  summMatrix[2, 3] <- length(unique(data[!is.na(data[, columnsIndex[2]]), columnsIndex[2]]))
  summMatrix[3, 3] <- length(unique(data[!is.na(data[, columnsIndex[1]]), columnsIndex[1]]))
  
  ## Print data summary and return
  cat("\n")
  cat("Data summary:\n")
  prmatrix(summMatrix[, c(1, 3)], rowlab = rep("-", 3), collab = rep("", 3), quote = FALSE)
  cat("\n")
  if(all(!is.na(data)) == FALSE){
    message("Data may contain NA values\n")
  }
  return(invisible(summMatrix[, 2:3]))
}
