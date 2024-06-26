% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/validate.Tax.R
\name{validate.Tax}
\alias{validate.Tax}
\title{Validation of taxonomic names by string similarity}
\usage{
validate.Tax(
  names,
  reference,
  method = "osa",
  threshold = 0.8,
  dropMulti = TRUE,
  ordered = FALSE)
}
\arguments{
\item{names}{A vector of type \code{"character"} with the input names to be compared.}

\item{reference}{A vector of type \code{"character"} with the dictionary of reference names. It must be assigned from an user's input database.}

\item{method}{Method used for string comparison. The available ones are: Optimal String Alignment (\code{method = "osa"}), Levenshtein distance (\code{method = "lv"}), Damerau-Levenshtein distance (\code{method = "dl"}), and Hamming distance (\code{method = "hamming"}). These methods are inherited from the function \link[stringdist]{stringdist}. Default \code{method  = "osa"}.}

\item{threshold}{A number between 0–1 indicating the proportion of similarity to be considered as the cutoff value. The output will show the matching names with scores equal to or greater than the threshold. Lower threshold values will produce less accurate matches. Default \code{threshold  = 0.8}.}

\item{dropMulti}{Boolean indicating whether low-scored matches for a given name should be dropped in case of multiple matches. By default, only the highest-scored match is retained (\code{dropMulti = TRUE}). Otherwise (\code{dropMulti = FALSE}), all possible matches are retained and displayed in descending order (for large datasets, this will be more demanding and take much longer). Default \code{dropMulti  = TRUE}.}

\item{ordered}{Boolean indicating whether the result should be given in alphabetical order (\code{ordered = TRUE}) or remain in the order of the user's input (\code{ordered = FALSE}). Default \code{ordered  = FALSE}.}
}
\value{
\code{validate.Tax} returns a three-columns \code{"data.frame"} object with the user's input names, the best matching names in the reference dictionary, and the string similarity score according to the selected method. The similarity score varies between 0 and 1, where 0 indicates complete dissimilarity and 1 indicates a perfect match.
}
\description{
\code{validate.Tax} performs a string similarity test to compare a vector containing the user's taxonomic names against a reference dictionary.
}
\details{
This function inherits some string comparison metrics from \link[stringdist]{stringdist}.

\strong{Hamming distance} (\code{method = "hamming"}): counts the number of character substitutions that turns "B" (reference) into "A" (input). It requires both strings have the same length, if not, the distance is \code{Inf}. 

\strong{Levenshtein distance} (\code{method = "lv"}): counts the number of deletions, insertions, and substitutions necessary to turn "B" into "A".

\strong{Optimal String Alignment} (\code{method = "osa"}): like the Levenshtein distance but allowing the transposition of adjacent characters (once by character).

\strong{Damerau-Levenshtein distance} (\code{method = "dl"}): like the Optimal String Alignment distance except that it allows for multiple transpositions of characters.
}