#' Get spatial boundaries for Latin American Countries
#'
#' When creating spatial plots for SALURBAL projects, this function retrieves
#' country `rnaturalearth::ne_countries()`
#'
#' - `get_ne_sf()`: Retrieves the outline all contries.
#' - `la_range_long()`: Retrieves the longitudinal cooridnates cooresponding to Latin American countries.
#' - `la_range_lat()`: Retrieves the latitudinal cooridnates cooresponding to Latin American countries.
#' - `get_l1_sf()`: Retrieves the location of each SALURBAL L1 city.
#'
#' @returns The numeric value of the exposure corresponding to the minimum value of the response.
#'
#' @rdname get_ne_sf
#' @export
#'
#' @examples
#'
#' # Plot Latin American countries
#' ggplot() +
#'   geom_sf(data = get_ne_sf()) +
#'   coord_sf(xlim = la_range_long(), ylim = la_range_lat())
#'

get_ne_sf <- rnaturalearth::ne_countries(scale = "medium")

#' @rdname get_ne_sf
#' @export

la_range_long <- function() c(-118.40136718784544, -34.805468749982765)

#' @rdname get_ne_sf
#' @export

la_range_lat <- function() c(-55.891699218544034, 32.715332031148364)
