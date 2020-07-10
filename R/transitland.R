base_url <- "https://transit.land/api/v1/"

query_string_from_params<-function(params){
  paste(lapply(names(params),function(p){
    value=params[[p]]
    if (class(value)=="bbox" | is.array(value)) value=paste(value,collapse=",")
    paste0(p,"=",value)
    }) %>% unlist, collapse = "&")
}



#' Get stops data
#' @export
#' @examples
#' get_transit_stops(list(bbox=c(-122.4183,37.7758,-122.4120,37.7858)))
get_transit_stops<-function(params,get_all=FALSE){
  if (length(params)==0) stop("Invalid parameters")
  if (get_all & is.null(params$per_page)) params$total="true"
  meta=list("next"=paste0(base_url,"stops","?",query_string_from_params(params)))
  first=TRUE
  data=NULL
  while ((first|get_all) & "next" %in% names(meta)) {
    json_data <- jsonlite::fromJSON(meta[["next"]])
    meta <- json_data$meta
    data <- json_data$stops %>%
      as_tibble()
    new_data <-  jsonlite::toJSON(data$geometry,
                                  flatten=TRUE,simplifyVector=TRUE,auto_unbox=TRUE) %>%
      geojsonsf::geojson_sf() %>%
      bind_cols(data %>% select(-geometry))

    # feature_collection <- list("type"="FeatureCollection","features"=json_data$stops)
    # temp <- tempfile()
    # jsonlite::write_json(feature_collection,temp, flatten=TRUE,simplifyVector=TRUE,auto_unbox=TRUE)
    # new_data <- geojsonsf::geojson_sf(temp)
    # unlink(temp)
    if (first) {
      data <- new_data
      first=FALSE
    } else {
      data <- dplyr::bind_rows(data,new_data) %>% sf::st_sf(stringsAsFactors=FALSE)
    }
  }
  if (is.na(sf::st_crs(data))) st_crs(data)=4326
  data
}



#' Get schedule stop pairs
#' @export
#' @param parms list of query parameters. Valid parameters are
#' bbox
#' location
#' @param paginate TRUE or FALSE, will try to retrieve all results if TRUE. If per_page is set it will iterate
#' through pages
#' @examples
#' get_transit_schedule_stops(list(bbox=c(-122.4183,37.7758,-122.4120,37.7858)))
get_transit_schedule_stops<-function(params,get_all=FALSE){
  if (length(params)==0) stop("Invalid parameters")
  meta=list("next"=paste0(base_url,"schedule_stop_pairs","?",query_string_from_params(params)))
  count=0
  data=NA
  while ((count==0|get_all) & "next" %in% names(meta)) {
    count <- count+1
    json_data <- jsonlite::fromJSON(meta[["next"]])
    meta <- json_data$meta
    if (count==1) {
      data <- json_data$schedule_stop_pairs %>% as.tibble
    } else {
      cat("\r",paste0("API call number ",count))
      data <- dplyr::bind_rows(data,json_data$schedule_stop_pairs %>% as.tibble)
    }
  }
  data
}


#' Get transit routes
#' @export
#' @param parms list of query parameters. Valid parameters are
#' bbox
#' location
#' @param paginate TRUE or FALSE, will try to retrieve all results if TRUE. If per_page is set it will iterate
#' through pages
#' @examples
#' get_transit_routes(list(bbox=c(-122.4183,37.7758,-122.4120,37.7858)))
get_transit_routes<-function(params,get_all=FALSE){
  if (length(params)==0) stop("Invalid parameters")
  meta=list("next"=paste0(base_url,"routes","?",query_string_from_params(params)))
  first=TRUE
  data=NULL
  while ((first|get_all) & "next" %in% names(meta)) {
    json_data <- jsonlite::fromJSON(meta[["next"]])

    meta <- json_data$meta
    data <- json_data$routes %>%
      as_tibble()
    new_data <-  jsonlite::toJSON(data$geometry,
                                  flatten=TRUE,simplifyVector=TRUE,auto_unbox=TRUE) %>%
      geojsonsf::geojson_sf() %>%
      bind_cols(data %>% select(-geometry))

    if (first) {
      data <- new_data
      first=FALSE
    } else {
      data <- dplyr::bind_rows(data,new_data) %>% sf::st_sf(stringsAsFactors=FALSE)
    }
  }
  if (is.na(sf::st_crs(data))) st_crs(data)=4326
  data
}


#' Get transit route stop patterns
#' @export
#' @param parms list of query parameters. Valid parameters are
#' bbox
#' location
#' @param paginate TRUE or FALSE, will try to retrieve all results if TRUE. If per_page is set it will iterate
#' through pages
#' @examples
#' get_transit_route_stop_patterns(list(bbox=c(-122.4183,37.7758,-122.4120,37.7858)))
get_transit_route_stop_patterns<-function(params,get_all=FALSE){
  if (length(params)==0) stop("Invalid parameters")
  meta=list("next"=paste0(base_url,"route_stop_patterns","?",query_string_from_params(params)))
  first=TRUE
  data=NULL
  while ((first|get_all) & "next" %in% names(meta)) {
    json_data <- jsonlite::fromJSON(meta[["next"]])
    meta <- json_data$meta
    feature_collection <- list("type"="FeatureCollection","features"=json_data$route_stop_patterns)
    temp <- tempfile()
    jsonlite::write_json(feature_collection,temp, flatten=TRUE,simplifyVector=TRUE,auto_unbox=TRUE)
    new_data <- geojsonsf::geojson_sff(temp) #%>% select(-geometry_centroid)
    unlink(temp)
    if (first) {
      data <- new_data
      first=FALSE
    } else {
      data <- dplyr::bind_rows(data,new_data) %>% sf::st_sf(stringsAsFactors=FALSE)
    }
  }
  if (is.na(sf::st_crs(data))) st_crs(data)=4326
  data
}



#' @importFrom dplyr %>%
#' @importFrom rlang .data
NULL
