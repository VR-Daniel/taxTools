#' Validation of taxonomic names by string similarity
#'
#' @description \code{validate.Tax} computes a string similitude test to compare a vector with user's taxonomic names against a reference dictionary.
#' @param names A vector of type \code{"character"} with the input names to be compared.
#' @param reference A vector of type \code{"character"} with the dictionary of reference names. It may be assigned from an user's input database, or from a database retrieved using \link[taxTools]{websearch.Tax}.
#' @param method Method used for string comparison. The available ones are: Optimal String Alignment (\code{method = "osa"}), Levenshtein distance (\code{method = "lv"}), Damerau-Levenshtein distance (\code{method = "dl"}), Hamming distance (\code{method = "hamming"}), and Longest Common Substring (\code{method = "lcs"}). These methods are inherited from the function \link[stringdist]{stringdist}
#' @param threshold A number between (0, 1] indicating the proportion of similitude to be considered as the cutoff value. The output will show the matching names with scores equal or greater scores than the threshold. Lower threshold values will produce less accurate matches.
#' @param dropMulti Boolean indicating if the less scored matches for a given name should be dropped in case of multiple matching. By default only the highest scored match is retained (\code{dropMulti = TRUE}). Otherwise (\code{dropMulti = FALSE}), all possible matches are retained and displayed in score descending order (for large data sets it will be more demanding and will take much longer). 
#' @param ordered Boolean indicating if the result should be given in alphabetic order (\code{ordered = TRUE}), or like in the user's input (\code{ordered = FALSE}).
#' @details This function inherits some of the string comparison metrics from the \link[stringdist]{stringdist} function.
#' 
#' \strong{Hamming distance} (\code{method = "hamming"}): counts the number of character substitutions that turns "B" (reference) into "A" (input). It requires both strings have the same length, if not, the distance is \code{Inf}. This method may generate erroneous matching when strings "A" and "B" differ in the number of characters, since "A" will be compared with the "B" strings of the same length, even if they are very quite different. 
#'
#' \strong{Levenshtein distance} (\code{method = "lv"}): counts the number of deletions, insertions and substitutions necessary to turn "B" into "A".
#'
#' \strong{Optimal String Alignment} (\code{method = "osa"}): is like the Levenshtein distance but also allows transposition of adjacent characters. Each character may be edited only once (for example, a character cannot be transposed twice to move it forward in the string).
#'
#' \strong{Damerau-Levenshtein distance} (\code{method = "dl"}): is like the Optimal String Alignment distance except that it allows for multiple edits on characters.
#' 
#' For a more detailed description of these metrics see \href{https://journal.r-project.org/archive/2014-1/loo.pdf}{van der Loo (2014)}.
#' @return \code{validate.Tax} returns a three columns \code{"data.frame"} object with the user's input names, the best matching names in the reference dictionary, and the string similitude score according to the selected method (a value between 0 and 1, where 0 is totally different and 1 perfect matching).
#' @references van der Loo, M. P. J. (2014). The \code{stringdist} Package for Approximate String Matching. \emph{The R Journal}, 6(1): 111-122.
#' @export validate.Tax
validate.Tax <- function(names, reference, method = c("osa", "lv", "dl", "hamming", "lcs"), threshold = 0.8, dropMulti = TRUE, ordered = FALSE){
  
  # Arguments validation ------
  
  ## Validate for missing arguments
  if (missing(names) | missing(reference)){
    cat("\n")
    stop(paste("One or more missing arguments with no default value", "\n\n"), call. = FALSE)
  }
  if(missing(method)){
    method = "osa"
  }
  
  ## Validate input names are character type
  if(is.character(names) == TRUE){
    
    ## Validate reference names are character type
    if(is.character(reference) == TRUE){
      
      ## Validate threshold be a number between (0, 1]
      if(threshold > 0 & threshold <= 1){
        
        # Names comparison ------
        
        ## Create the validation matrix for output and fill with input names
        names <- unique(names)
        names <- names[!is.na(names)]
        validationMatrix <- as.data.frame(matrix(nrow = length(names), ncol = 3))
        validationMatrix[, 1] <- names
        colnames(validationMatrix) <- c("Input name", "Best match", "Score")
        reference <- unique(reference)
        reference <- reference[!is.na(reference)]
        
        ## Loop for calculate similitude score for each input name
        for(i in 1:length(names)){
          
          ## Create the comparison matrix between name[i] and all reference names
          tmpScores <- as.data.frame(cbind(reference, apply(as.data.frame(reference), 1, function(x) round(stringsim(names[i], x, method = method), 4))))
          tmpScores <- dplyr::filter(tmpScores, V2 >= threshold)
          
          ## Get the min index for the name[i] in the validation matrix
          tmpInd <- min(which(validationMatrix$`Input name` == names[i]))
          
          ## Validate if there are no matches and assign empty values if TRUE
          if(length(which(tmpScores$V2 >= threshold)) == 0){
            validationMatrix[tmpInd, 2] <- ""
            validationMatrix[tmpInd, 3] <- 0
            
            ## Validate if there matches and assign values according to
          } else{
            
            ## Get the number of matches
            tmpMatches <- length(which(tmpScores$V2 >= threshold))
            
            ## Validate if there are a single match and assign values
            if(tmpMatches == 1){
              validationMatrix[tmpInd, 2] <- tmpScores[which(tmpScores$V2 >= threshold), 1]
              validationMatrix[tmpInd, 3] <- tmpScores[which(tmpScores$V2 >= threshold), 2]
              
              ## Validate if there are a multiple matches and assign values
            } else{
              
              ## In case of multi-matches validate dropMulti parameter
              if(dropMulti == TRUE){
                
                ## Validate if multi-matches have different scores
                if(length(tmpScores$V2) == length(unique(tmpScores$V2))){
                  validationMatrix[tmpInd, 2] <- tmpScores[which(tmpScores$V2 == max(tmpScores[which(tmpScores$V2 >= threshold), 2])), 1]
                  validationMatrix[tmpInd, 3] <- max(tmpScores[which(tmpScores$V2 >= threshold), 2])
                } else{
                  
                  ## Validate if multi-matches have the same score
                  if(length(unique(tmpScores$V2)) == 1){
                    validationMatrix[tmpInd, 2] <- "[MULTI]"
                    validationMatrix[tmpInd, 3] <- max(tmpScores[which(tmpScores$V2 >= threshold), 2])
                    
                    ## Validate if all multi-matches do not have the same score, but at least one is 1
                  } else{
                    if(max(tmpScores[which(tmpScores$V2 >= threshold), 2]) == 1){
                      validationMatrix[tmpInd, 2] <- tmpScores[which(tmpScores$V2 == max(tmpScores[which(tmpScores$V2 >= threshold), 2])), 1]
                      validationMatrix[tmpInd, 3] <- max(tmpScores[which(tmpScores$V2 >= threshold), 2])
                      
                      ## Validate if some multi-matches have the same score and none is 1
                    } else{
                      validationMatrix[tmpInd, 2] <- "[MULTI]"
                      validationMatrix[tmpInd, 3] <- max(tmpScores[which(tmpScores$V2 >= threshold), 2])
                    }
                  }
                }
              } else{
                
                ## Calculate the number of additional rows for name [i] when dropMulti = FALSE
                tmpDuplicates <- length(which(tmpScores$V2 >= threshold)) - 1
                
                ## Create a temporal matrix for store multi-matches when dropMulti = FALSE
                tmpMatrix <- as.data.frame(matrix(nrow = (tmpDuplicates + 1), ncol = 3))
                
                ## Fill the multi-matches temporal matrix when dropMulti = FALSE
                n <- 1
                for(n in n:(tmpDuplicates + 1)){
                  tmpMatrix[n, 1] <- names[i]
                  tmpMatrix[n, 2] <- tmpScores[which(tmpScores$V2 >= threshold), 1][n]
                  tmpMatrix[n, 3] <- tmpScores[which(tmpScores$V2 >= threshold), 2][n]
                }
                
                ## Descending order the temporal matrix of multi-matches when dropMulti = FALSE
                tmpMatrix <- dplyr::arrange(tmpMatrix, V2)
                tmpMatrix <- dplyr::arrange(tmpMatrix, desc(V3))
                
                ## Add the multi-match rows for name[i] when it is not the last one
                if(i < length(names)){
                  validationMatrix <- rbind(validationMatrix[1:tmpInd,],
                                            validationMatrix[rep(tmpInd, tmpDuplicates),],
                                            validationMatrix[(tmpInd + 1):nrow(validationMatrix),])
                  rownames(validationMatrix) <- seq(1:nrow(validationMatrix))
                  validationMatrix[tmpInd:(tmpInd + tmpDuplicates),] <- tmpMatrix[,]
                  
                  ## Add the multi-match rows for name[i] when it is the last one
                } else{
                  validationMatrix <- rbind(validationMatrix[1:tmpInd,],
                                            validationMatrix[rep(tmpInd, tmpDuplicates),])
                  rownames(validationMatrix) <- seq(1:nrow(validationMatrix))
                  validationMatrix[tmpInd:(tmpInd + tmpDuplicates),] <- tmpMatrix[,]
                }
              }
            }
          }
        }
        
        ## Convert scores to double type
        validationMatrix[, 3] <- as.double(validationMatrix[, 3])
        
        ## Validate ordered parameter and order when TRUE
        if (ordered == TRUE){
          validationMatrix <- as.data.frame(dplyr::group_by(validationMatrix, `Input name`) %>% 
                                              dplyr::arrange(`Best match`, .by_group = TRUE) %>% 
                                              dplyr::arrange(desc(Score), .by_group = TRUE))
        }
        
        ## Validate the size of the validation matrix output and return with messages according to it
        if(nrow(validationMatrix) <= 10){
          if(threshold < 0.75){
            cat("\n")
            message("Message: Lower threshold values will produce less accurate matches")
            cat("\n")
            print(validationMatrix)
            cat("\n")
            message(paste("Mean score: ", round(mean(validationMatrix[, 3]), 4), "\n"))
            return(invisible(validationMatrix))
          } else{
            cat("\n")
            print(validationMatrix)
            cat("\n")
            message(paste("Mean score: ", round(mean(validationMatrix[, 3]), 4), "\n"))
            return(invisible(validationMatrix))
          }
        } else{
          if(threshold < 0.75){
            cat("\n")
            message("Message: Lower threshold values will produce less accurate matches")
            message(paste("Message: Showing the first 10 of ", nrow(validationMatrix), "rows"))
            cat("\n")
            print(validationMatrix[1:10,])
            cat("\n")
            return(invisible(validationMatrix))
          } else{
            cat("\n")
            message(paste("Message: Showing the first 10 of ", nrow(validationMatrix), "rows"))
            cat("\n")
            print(validationMatrix[1:10,])
            cat("\n")
            return(invisible(validationMatrix))
          }
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
    stop("Data must be a \"character\" vector or a data frame column", "\n\n", call. = FALSE)
  }
}