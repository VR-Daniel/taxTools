#' Update taxonomic names
#'
#' @description \code{updateNames.Tax} updates the user's input taxonomic names based on a validation matrix.
#' @param names A vector of type \code{"character"} with the input names to be updated.
#' @param validationMatrix A \code{"data.frame"} object obtained from \link[taxTools]{validate.Tax} function.
#' @param threshold A number between (0, 1] indicating the proportion of similitude to be considered as the cutoff value for update the input names. In case the validation matrix contains multi-match and more than one name agrees with the threshold criteria, the update will be made using the name with the highest score.
#' @param markNoUpdated Boolean indicating if the names that won't be updated must be marked to identify them. When \code{markNoUpdated = TRUE} an * will be added at the end of the name, on the contrary, the names with scores lower than threshold value will keep as is.
#' @export updateNames.Tax
updateNames.Tax <- function(names, validationMatrix, threshold = 0.8, markNoUpdated = TRUE){
  
  # Arguments validation ------
  
  ## Validate for missing arguments
  if (missing(names) | missing(validationMatrix)){
    cat("\n")
    stop(paste("One or more missing arguments with no default value", "\n\n"), call. = FALSE)
  }
  
  ## Validate input names are character type
  if(is.character(names) == TRUE){
    
    ## Validate validationMatrix format
    if(is.data.frame(validationMatrix) == TRUE){
      if(ncol(validationMatrix) == 3){
        
        ## Validate reference names are character type
        if(all(is.character(validationMatrix[[1]])) == TRUE & all(is.character(validationMatrix[[2]])) == TRUE & all((is.numeric(validationMatrix[[3]]) & (validationMatrix[[3]] >= 0 & validationMatrix[[3]] <= 1))) == TRUE){
          
          ## Validate threshold be a number between (0, 1]
          if(threshold > 0 & threshold <= 1){
            
            # Update names ------
            
            validationMatrix <- dplyr::distinct(validationMatrix, `Input name`, .keep_all = TRUE)
            if(markNoUpdated == TRUE){
              
              ## Update names and mark the not updated
              for(i in 1:nrow(validationMatrix)){
                if(validationMatrix[i, 3] >= threshold & validationMatrix[i, 3] < 1){
                  names <- gsub(validationMatrix[i, 1], validationMatrix[i, 2], names)
                } else{
                  if(validationMatrix[i, 1] != "" & validationMatrix[i, 3] < 1){
                    names <- gsub(validationMatrix[i, 1], paste(validationMatrix[i, 1], "*", sep = ""), names, fixed = TRUE)
                  } else{
                    next
                  }
                }
              }
              
              ## Return output
              names <- gsub("* ", " ", names, fixed = TRUE)
              return(invisible(names))
            } else {
              
              ## Update names and do not mark the not updated
              for(i in 1:nrow(validationMatrix)){
                if(validationMatrix[i, 3] >= threshold & validationMatrix[i, 3] < 1){
                  names <- gsub(validationMatrix[i, 1], validationMatrix[i, 2], names)
                } else{
                  next
                }
              }
              
              ## Return output
              return(invisible(names))
            }
            
            ## Data validation error messages
          } else{
            cat("\n")
            stop("Threshold must be a number greater than 0 and fewer or equal than 1", "\n\n", call. = FALSE)
          }
        } else{
          cat("\n")
          stop("Reference data must be a \"character\" vector or a data frame column", "\n\n", call. = FALSE)
        }
      } else{
        cat("\n")
        stop("Wrong validationMatrix format", "\n\n", call. = FALSE)
      }
    } else{
      cat("\n")
      stop("Wrong validationMatrix format", "\n\n", call. = FALSE)
    }
  } else{
    cat("\n")
    stop("Data must be a \"character\" vector or a data frame column", "\n\n", call. = FALSE)
  }
}
