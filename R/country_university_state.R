#' Find country on affiliation field based on country name
#'
#' Matches country names on free text affiliation field
#'
#' This function was designed to internal use in the package. The aim is to find a match of a country name on the free-text affiliation field.
#'
#' @param x free-text (affiliation field)
#'
#' @author Hugo Fitipaldi
#'
#'
#' @return This function returns a character string with the name of the country
#' @export detecting_country_country
#' @examples
#' detecting_country_country("Lund University Diabetes Centre, Malmö, Skåne, Sweden")
#'
detecting_country_country <- function(x) {

  text <- x
  text <- tolower(text)
  text <- stringi::stri_trans_general(str = text, id = "Latin-ASCII")
  text <- gsub('[[:digit:]]+', '', text)
  text <- qdap::rm_stopwords(text, separate = FALSE)
  text <- gsub('[[:punct:] ]+',' ', text)
  text <- gsub('centre','', text)
  text <- gsub('university','', text)

  countries <- affiliation::countries_list

  countries$country_name <- countries$name
  countries$name <- tolower(countries$name)
  countries$name <- stringi::stri_trans_general(str = countries$name, id = "Latin-ASCII")
  countries$name <- paste0('\\b' ,countries$name, '\\b')

  return(toString(unique(countries[stringr::str_detect(text, countries$name) == TRUE,]$country_name)))

}

#' Find country on affiliation field based on university name
#'
#' Find country names on free text affiliation field based on university names
#'
#' This function was designed to internal use in the package. The aim is to find the country name by matchin with the university location.
#'
#' @param x free-text (affiliation field)
#'
#' @author Hugo Fitipaldi
#'
#'
#' @return This function returns a character string with the name of the country
#' @export detecting_country_university
#' @examples
#' detecting_country_university("Lund University Diabetes Centre, Malmö, Skåne, Sweden")
#'

detecting_country_university <- function(x) {

  text <- x
  text <- tolower(text)
  text <- stringi::stri_trans_general(str = text, id = "Latin-ASCII")
  text <- gsub('[[:digit:]]+', '', text)
  text <- gsub('[[:punct:] ]+',' ', text)

  universities <- affiliation::universities_list


  universities$university_name <- tolower(universities$university_name)
  universities$university_name <- stringi::stri_trans_general(str = universities$university_name, id = "Latin-ASCII")

  return(toString(unique(universities[stringr::str_detect(text, universities$university_name) == TRUE,]$country_name)))
}

#' Find country on affiliation field based on name of state
#'
#' Find country name on free text affiliation field based on state name
#'
#' This function was designed to internal use in the package. The aim is to find the country name by matching with a name of state.
#'
#' @param x free-text (affiliation field)
#'
#' @author Hugo Fitipaldi
#'
#'
#' @return This function returns a character string with the name of the country
#' @export detecting_country_state
#' @examples
#' detecting_country_state("Lund University Diabetes Centre, Malmö, Skåne, Sweden")
#'

detecting_country_state <- function(x) {

  text <- x
  text <- tolower(text)
  text <- stringi::stri_trans_general(str = text, id = "Latin-ASCII")
  text <- gsub('[[:digit:]]+', '', text)
  text <- qdap::rm_stopwords(text, separate = FALSE)
  text <- gsub('[[:punct:] ]+',' ', text)
  text <- gsub('centre','', text)
  text <- gsub('university','', text)

  states <- affiliation::states_list

  states$name <- tolower(states$name)
  states$name <- stringi::stri_trans_general(str = states$name, id = "Latin-ASCII")
  states$name <- paste0('\\b' ,states$name, '\\b')

  return(toString(unique(states[stringr::str_detect(text, states$name) == TRUE,]$country_name)))

}

