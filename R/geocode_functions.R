#'
#'
#' Matches country names on free text affiliation field
#'
#' This function was designed to internal use in the package. The aim is to find a match of a country name on the free-text affiliation field.
#'
#' @param search_query_url
#' @param country_url
#' @param language_url
#' @param email_url
#'
#' @author Hugo Fitipaldi
#'
#'
#' @return This function returns a character string with the name of the country
#' @export
#'

url_nominatim_search <- function(search_query_url, country_url,
                                 language_url, email_url) {
  # nominatim search api url
  url_nominatim_search_api <- "https://nominatim.openstreetmap.org/search/"
  # convert input into a list
  search_query_url <- sapply(search_query_url, as.list)
  # percent-encode search request
  search_query_url <- sapply(search_query_url, utils::URLencode)
  # parameters
  if (!is.null(country_url)) {
    country_url <- paste0("&countrycodes=", country_url)
  }
  parameters_url <- paste0("?format=json",
                           "&addressdetails=1&extratags=1&limit=1",
                           country_url, "&accept-language=", language_url,
                           "&email=", email_url)
  # construct search request for geocode
  url_nominatim_search_call <- paste0(url_nominatim_search_api,
                                      search_query_url, parameters_url)
  return(url_nominatim_search_call)
}

#' Pre-processing of json transformed query
#'
#' Matches country names on free text affiliation field
#'
#' This function was designed to internal use in the package. The aim is to find a match of a country name on the free-text affiliation field.
#'
#' @param geodata_json
#'
#' @author Hugo Fitipaldi
#'
#' @return
#' @export
#'

get_geodata_from_json_nominatim <- function(geodata_json) {

  # convert json output into r object
  geodata <- lapply(geodata_json, jsonlite::fromJSON, simplifyVector = FALSE)
  # extract coordinates, address and contacts
  lat_lng_a_c <- data.frame(lat = NA, lng = NA, address = NA, pub_name = NA,
                            street_name = NA, house_number = NA, suburb = NA,
                            postcode = NA, state_district = NA, website_1 = NA,
                            website_2 = NA, website_3 = NA, phone_1 = NA,
                            phone_2 = NA, email_1 = NA, email_2 = NA)
  for(i in 1:length(geodata)) {
    if(length(geodata[[i]]) != 0) {
      # get data
      lat <- geodata[[i]][[1]]$lat
      lng <- geodata[[i]][[1]]$lon
      address <- geodata[[i]][[1]]$display_name
      pub_name <- geodata[[i]][[1]]$address$pub
      street_name <- geodata[[i]][[1]]$address$road
      house_number <- geodata[[i]][[1]]$address$house_number
      suburb <- geodata[[i]][[1]]$address$suburb
      postcode <- geodata[[i]][[1]]$address$postcode
      state_district <- geodata[[i]][[1]]$address$state_district
      website_1 <- geodata[[i]][[1]]$extratags$website
      website_2 <- geodata[[i]][[1]]$extratags$url
      website_3 <- geodata[[i]][[1]]$extratags$`contact:website`
      phone_1 <- geodata[[i]][[1]]$extratags$phone
      phone_2 <- geodata[[i]][[1]]$extratags$`contact:phone`
      email_1 <- geodata[[i]][[1]]$extratags$email
      email_2 <- geodata[[i]][[1]]$extratags$`contact:website`
      # get rid of NULLs
      info <- list(lat, lng, address, pub_name, street_name,
                   house_number, suburb, postcode, state_district,
                   website_1, website_2, website_3,
                   phone_1, phone_2, email_1, email_2)
      for (j in 1:length(info)) {
        if (is.null(info[[j]])) info[[j]] <- NA
      }
      # create output data frame
      lat_lng_a_c[i, ] <- info
    } else {
      lat_lng_a_c[i, ] <- NA
    }
  }
  return(lat_lng_a_c)
}

#' Aggregation of queries to find country name based on address
#'
#' Queries openstreet API to find latitude, longitude and country based on address
#'
#' This function was designed to internal use in the package. It uses free-text affiliation field as query for open street API
#'
#' @param search_query free-text (affiliation field)
#' @param country
#' @param language
#' @param fields
#' @param email your email to access the openstreet API
#'
#' @author Hugo Fitipaldi
#'
#' @return This function returns a data frame with searched query, latitude, longitude and assigned country
#' @export
#'
geocode_nominatim <- function(search_query, country = NULL, language = "en",
                              fields = "coordinates", email) {

  search_query <- paste0(tail(unlist(strsplit(search_query, ",")), 2)[1], ",", tail(unlist(strsplit(search_query, ",")), 1))
  # construct url for geocoding
  url_geocode <- url_nominatim_search(search_query, country, language, email)
  # get data from nominatim
  # wait 3 seconds between each call
  geodata_json <- list()
  for (i in 1:length(url_geocode)) {
    geodata_json[i] <- RCurl::getURL(url_geocode[i])
    Sys.sleep(3)
  }

  pattern <- '\"country\":\"\\s*(.*?)\\s*\",\"country_code\"'
  result <- regmatches(geodata_json, regexec(pattern, geodata_json))
  country_match <- result[[1]][2]

  # get data from json output
  geodata_df <- as.data.frame(sapply(search_query, as.character),
                              stringsAsFactors = FALSE)
  names(geodata_df) <- "search query"
  rownames(geodata_df) <- NULL
  geodata_df[, 2:17] <- get_geodata_from_json_nominatim(geodata_json)
  geodata_df_query <- data.frame(search_query = geodata_df[, 1],
                                 stringsAsFactors = FALSE)
  geodata_df_coordinates <- geodata_df[, 2:3]
  geodata_df_address <- geodata_df[, 4:10]
  geodata_df_contacts <- geodata_df[, 11:17]
  # return dataframe with the geodata
  geodata_result <- geodata_df_query
  if("all" %in% fields) {
    geodata_result <- cbind(geodata_result, geodata_df[, 2:17])
  }
  if("coordinates" %in% fields) {
    geodata_result <- cbind(geodata_result, geodata_df_coordinates)
  }
  if("address" %in% fields) {
    geodata_result <- cbind(geodata_result, geodata_df_address)
  }
  if("contacts" %in% fields) {
    geodata_result <- cbind(geodata_result, geodata_df_contacts)
  }

  geodata_result <- cbind(geodata_result, country_match)

  return(geodata_result)
}

