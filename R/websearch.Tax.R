#' Download the most recent taxonomy from online databases
#'
#' @description \code{websearch.Tax} retrieves the most recent taxonomic classification for a given Class from online reference databases. This function requires an active internet connection.
#' 
#' \strong{Note:} Currently, this function is only supported for amphibians and reptiles.
#' @param class A string of type \code{"character"} indicating the taxonomic Class of interest. Available ones are: \code{"Amphibia"} or shorter \code{"Amp"} for amphibians, and \code{"Reptilia"} or shorter \code{"Rep"} for reptiles.
#' @param source A string of type \code{"character"} indicating the source for retrieving the taxonomy. Available ones are: \code{"AMNH"} (\href{https://amphibiansoftheworld.amnh.org}{Amphibians of the World}) and \code{"AB"} (\href{https://amphibiaweb.org}{AmphibiaWeb}) for amphibians, and \code{"TRDB"} (\href{http://www.reptile-database.org}{The Reptile Database}) for reptiles.
#' @param country A string or a vector of type \code{"character"} indicating the country or countries for filtering the search. Country names must be written as in the reference sources search options (see each source website).
#' @details This function uses the web scraping method for retireve the data, therefore, a large number of queries, or queries for many countries will take more time or may temporary block the access to the website.
#' 
#' Given the script for each source depends on the HTML structure of each webpage, any change in its structure may be the queries to stop working suddenly. In this last case, please email the package author.
#' @return \code{websearch.Tax} returns a \code{"data.frame"} object with the most recent taxonomy for the selected Class, including Order, Class, Family, Genus, Species, authorship (author + year), and country.
#' @export websearch.Tax
websearch.Tax <- function(class, source = c("AMNH", "AB", "TRDB"), country){
  
  # Arguments validation ------
  
  ## Validate for missing arguments
  if (missing(class) | missing(source) | missing(country)){
    cat("\n")
    stop(paste("One or more missing arguments with no default value", "\n\n"), call. = FALSE)
  }
  
  ## Validate class argument
  if(class %in% c("Amphibia", "Amp", "Reptilia", "Rep") == FALSE){
    cat("\n")
    stop("Wrong \"class\" argument", "\n\n", call. = FALSE)
  } else{
    if(class == "Amphibia" | class == "Amp"){
      class <- "Amphibia"
    }
    if(class == "Reptilia" | class == "Rep"){
      class <- "Reptilia"
    }
  }
  
  ## Define data paths
  websearchDB <- data.frame(Class = c("Amphibia",
                                      "Amphibia",
                                      "Reptilia"),
                            Source = c("AMNH",
                                       "AB",
                                       "TRDB"),
                            URL = c("https://amphibiansoftheworld.amnh.org",
                                    "",
                                    "https://reptile-database.reptarium.cz"),
                            Search = c("/content/search?taxon=&subtree=&subtree_id=&english_name=&author=&year=&country=",
                                       "",
                                       "/advanced_search?location=[COUNTRY]&exact[0]=location&submit=Search"))
  
  ## Validate for the combination of class and source arguments
  if(length(websearchDB[which(websearchDB$Class == class & websearchDB$Source == source), 3]) == 1){
    path <- websearchDB[which(websearchDB$Class == class & websearchDB$Source == source), 3]
    
  } else{
    cat("\n")
    stop("Invalid combination of parameters \"class\" and \"source\"", "\n\n", call. = FALSE)
  }
  
  ## Check if data directory exist
  if(dir.exists(system.file("data", package="taxTools")) == FALSE){
    dir.create(paste(system.file(package="taxTools"), "data", sep = "/"), showWarnings = FALSE)
  }
  
  # Web scraping ------
  
  ## Web scraping when source is AMNH ------
  if(source == "AMNH"){
    
    ## Get the search string
    tmpSearch <- websearchDB[which(websearchDB$Class == class & websearchDB$Source == source), 4]
    
    ## Read website
    tryCatch({
      url <- url(path, "rb")
      site <- read_html(url)
      close(url)
    }, error = function(e){
      close(url)
      cat("\n")
      stop("There was a problem trying to connect to the website", "\n", call. = FALSE)
    })
    
    ## Retrieve AMNH countries and codes
    tmpCountries <- data.frame(Code = (html_nodes(site, "option") %>% html_attr("value"))[-1],
                               Name = (html_nodes(site, "option") %>% html_text)[-1])
    
    ## Validate if all input country names are in the tmpCountries
    if(all(country %in% tmpCountries$Name) == TRUE){
      
      ## Retrieve updated taxonomy when a single country
      if(length(country) == 1){
        
        ## Read web site by country
        tryCatch({
          searchPath <- paste(path, tmpSearch, tmpCountries[which(tmpCountries$Name == country), 1], sep = "")
          url <- url(searchPath, "rb")
          site <- read_html(url)
        }, error = function(e){
          close(url)
          cat("\n")
          stop("There was a problem trying to connect to the website", "\n", call. = FALSE)
        })
        
        ## Get species names and URL
        tmpNames <- data.frame(Species = gsub("\n", "", str_trim((html_nodes(site, ".Species") %>% html_nodes("a") %>% html_text()))),
                               URL = (html_nodes(site, ".Species") %>% html_nodes("a") %>% html_attr("href")))
        
        ## Fill table with the taxonomic data retrieved
        tmpMatrix <- data.frame(Class = websearchDB[which(websearchDB$Class == class & websearchDB$Source == source), 1],
                                Order = word(tmpNames$URL, 3, sep = "/"),
                                Family = gsub("/", "", str_extract(tmpNames$URL, "\\/\\w*idae\\b\\/")),
                                Genus = word(tmpNames$Species, 1),
                                Species = word(tmpNames$Species, 1, 2),
                                Authorship = word(tmpNames$Species, -((str_count(tmpNames$Species, " ") + 1) - 2), -1),
                                Country = country)
        
        ## Summarize data and return
        close(url)
        summary.Tax(tmpMatrix)
        return(invisible(tmpMatrix))
        
        ## Retrieve updated taxonomy for multiple countries
      } else{
        
        ## Temporal matrix for store each country data
        tmpMulti <- as.data.frame(matrix(nrow = 0, ncol = 7))
        
        ## Loop for extract each country data
        for(i in 1:length(country)){
          
          ## Read website by country[i]
          tryCatch({
            searchPath <- paste(path, tmpSearch, tmpCountries[which(tmpCountries$Name == country[i]), 1], sep = "")
            url <- url(searchPath, "rb")
            site <- read_html(url)
          }, error = function(e){
            close(url)
            cat("\n")
            stop("There was a problem trying to connect to the website", "\n", call. = FALSE)
          })
          
          ## Get species names and URL
          tmpNames <- data.frame(Species = gsub("\n", "", str_trim((html_nodes(site, ".Species") %>% html_nodes("a") %>% html_text()))),
                                 URL = (html_nodes(site, ".Species") %>% html_nodes("a") %>% html_attr("href")))
          
          ## Fill table with the taxonomic data retrieved
          tmpMatrix <- data.frame(Class = websearchDB[which(websearchDB$Class == class & websearchDB$Source == source), 1],
                                  Order = word(tmpNames$URL, 3, sep = "/"),
                                  Family = gsub("/", "", str_extract(tmpNames$URL, "\\/\\w*idae\\b\\/")),
                                  Genus = word(tmpNames$Species, 1),
                                  Species = word(tmpNames$Species, 1, 2),
                                  Authorship = word(tmpNames$Species, -((str_count(tmpNames$Species, " ") + 1) - 2), -1),
                                  country = country[i])
          
          ## Paste each country data in a single table
          tmpMulti <- rbind(tmpMulti, tmpMatrix)
        }
        
        ## Summarize data and return
        close(url)
        summary.Tax(tmpMulti)
        return(invisible(tmpMulti))
      }
      
      ## Error message when country names do not match tmpCountries
    } else{
      cat("\n")
      stop("Some country names are not as in the AMNH search: ", paste(country[which(country %in% tmpCountries$Name == FALSE)], collapse = ", "), "\n\n", call. = FALSE)
    }
  }
  
  if(source == "AB"){
    cat("\n")
    stop("This option is not implemented yet", "\n", call. = FALSE)
  }
  
  ## Web scraping when source is TRDB ------
  if(source == "TRDB"){
    
    ## Download base .xlsx file
    tryCatch({
      tmpPath <- "http://www.reptile-database.org/data"
      url <- url(tmpPath, "rb")
      site <- read_html(url)
      close(url)
    }, error = function(e){
      close(url)
      cat("\n")
      stop("There was a problem trying to connect to the website", "\n", call. = FALSE)
    })
    
    ## Get the link to the most recent Excel
    tmpFile <- (html_nodes(site, "li") %>% html_nodes("a") %>% html_attr("href"))[1]
    
    ## Check if the file already exist and download it if not
    if(!file.exists(paste(system.file("data", package="taxTools"), tmpFile, sep = "/"))){
      download.file(paste(tmpPath, tmpFile, sep = "/"),
                    destfile = paste(system.file("data", package="taxTools"), tmpFile, sep = "/"),
                    mode = "wb",
                    quiet = TRUE)
      cat("\n")
      message("Message: Downloading a local version of the base file")
    } else{
      cat("\n")
      message("Message: Base file already exist, using the local version")
    }
    
    ## Read base .xlsx file
    repExcel <- read_excel(paste(system.file("data", package="taxTools"), tmpFile, sep = "/"))
    
    ## Fill table with the taxonomic data retrieved
    repMatrix <- data.frame(Class = websearchDB[which(websearchDB$Class == class & websearchDB$Source == source), 1],
                            Order = word(repExcel$Familyetc, -2, -1),
                            Family = str_extract(repExcel$Familyetc, "\\w*idae\\b"),
                            Genus = word(repExcel$Species, 1),
                            Species = repExcel$Species,
                            Authorship = str_to_title(repExcel$Author),
                            Country = NA)
    tmpMatrix <- repMatrix
    
    ## Get the search string #
    tmpSearch <- websearchDB[which(websearchDB$Class == class & websearchDB$Source == source), 4]
    
    ## Filter data by country
    if(length(country) == 1){
      
      # Read website
      tryCatch({
        tmpPath <- paste(path, gsub("[COUNTRY]", country, tmpSearch, fixed = TRUE), sep = "")
        url <- url(tmpPath, "rb")
        site <- read_html(url)
        close(url)
      }, error = function(e){
        close(url)
        cat("\n")
        stop("There was a problem trying to connect to the website", "\n", call. = FALSE)
      })
      
      ## Get the species list by country
      tmpResults <- html_nodes(site, "ul > li > a > em") %>% html_text()
      
      ## Define a non-matches list for show error if it occurs
      noMatches <- NULL
      
      ## Filter data for the search results > 0
      if(length(tmpResults >= 1)){
        tmpMatrix <- dplyr::filter(repMatrix, Species %in% tmpResults)
        tmpMatrix$Country <- country
      } else{
        noMatches <- c(noMatches, country[i])
      }
      
      ## Summarize and return
      summary.Tax(tmpMatrix)
      return(invisible(tmpMatrix))
      
      ## Filter the data for multiple countries
    } else{
      
      ## Temporal matrix for store each country data
      tmpMulti <- as.data.frame(matrix(nrow = 0, ncol = 7))
      
      ## Define a non-matches list for show error if it occurs
      noMatches <- NULL
      
      ## Loop for extract each country data
      for(i in 1:length(country)){
        
        ## Read website
        tryCatch({
          tmpPath <- paste(path, gsub("[COUNTRY]", country[i], tmpSearch, fixed = TRUE), sep = "")
          url <- url(tmpPath, "rb")
          site <- read_html(url)
          close(url)
        }, error = function(e){
          close(url)
          cat("\n")
          stop("There was a problem trying to connect to the website", "\n", call. = FALSE)
        })
        
        ## Get the species list by country[i]
        tmpResults <- html_nodes(site, "ul > li > a > em") %>% html_text()
        
        ## Filter data for the search results > 0
        if(length(tmpResults >= 1)){
          tmpMatrix <- dplyr::filter(repMatrix, Species %in% tmpResults)
          tmpMatrix$Country <- country[i]
          tmpMulti <- rbind(tmpMulti, tmpMatrix)
        } else{
          noMatches <- c(noMatches, country[i])
        }
      }
      
      ## Summarize
      summary.Tax(tmpMulti)
      
      ## Give non-matching message if search results = 0
      if(length(noMatches) != 0){
        message("Message: Some queries do not retrieve results: ", paste(noMatches, collapse = ", "))
        cat("\n")
      }
      
      ## Validate if output contain data
      if(nrow(tmpMulti) !=0){
        
        ## Return the data
        return(invisible(tmpMulti))
        
        ## If data is empty stop and do not give output
      } else{
        opt <- options(show.error.messages = FALSE)
        on.exit(options(opt))
        stop()
      }
    }
  }
}