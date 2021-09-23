#' @rdname utils
#' @title Miscellaneous utilities
#' 
#' @description Functions that convert author metadata to the correct format
#' 
#' @param degree The degrees of latitude/longitude. Always use positive values
#' for \code{degree} and use the \code{dir} parameter to make negative as needed.
#' @param minutes The minutes of latitude/longitude.
#' @param seconds The seconds of latitude/longitude.
#' @param dir North, South, East or West. Uses \code{"N", "S", "E", "W"}.
#' 
#' @return \code{tibble} or a decimal value.
#' @export

dms_deg <- function(degree, minutes = 0, seconds = 0, 
                    dir = c("N", "S", "E", "W")) {
  
  if(dir %in% c("N", "S") && abs(degree) > 90) {
    stop("Latitude degrees cannot be greater than 90!")
  }
  
  if(degree < 0) {
    stop("'degree' must be positive! Use 'dir' to control sign of output.")
  }
  
  if(dir %in% c("E", "W") && abs(degree) > 180) {
    stop("Longitude degrees cannot be greater than 180!")
  }
  
  out <- degree + (minutes / 60) + (seconds / 3600)
  
  if(dir %in% c("S", "W")) out <- out * -1
  
  return(out)
  
}


#' @rdname utils
#' @param species_author The species name used by the author.
#' @importFrom taxize gnr_resolve
#' @export

resolve_pdb_tax <- function(species_author) {
  
  taxize::gnr_resolve(spp, 
                      data_source_ids = 1,
                      highestscore = FALSE)
  
}


