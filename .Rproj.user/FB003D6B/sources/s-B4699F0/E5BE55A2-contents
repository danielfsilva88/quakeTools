#' Functions to clean and plot earthquake data
#'
#' Function to clean location column.
#'
#' This function receives an array of char containing locations and its
#' countries joined, returning only the location part. This data is from
#' 'The Significant Database Earthquake' of NOAA.
#'
#' @param loc_full Char array to be cleaned
#'
#' @return A cleaned char array
#'
#' @importFrom stringi stri_split_fixed
#' @importFrom stringr str_to_title
#'
#' @examples
#' \dontrun{readr::read_delim("earthquakes.txt", delim = "\t") %>% eq_clean_data()}
#'
#' @export
eq_location_clean <- function(loc_full){
  # split the string in array by ":"
  listloc <- stringi::stri_split_fixed(loc_full, ":", n = 2)

  location <- trimws(sapply(listloc, "[", 2))

  NAlocation <- trimws(sapply(listloc, "[", 1))

  location[is.na(location)] <- NAlocation[is.na(location)]
  # Title notation
  clean_loc <- stringr::str_to_title(location)

  return(clean_loc)
}

#' Function to clean earthquake data
#'
#' This function receives a data frame from The Significant Database Earthquake
#' from NOAA (https://www.ngdc.noaa.gov/nndc/struts/form?t=101650&s=1&d=1) and
#' return it with new treated columns ABS_DATE, DATE and LOCATION. Also,
#' turn columns of LATITUDE and LONGITUDE to numeric.
#'
#' @param df Data frame to be cleaned.
#'
#' @note Besides requirements, this function turns columns EQ_PRIMARY and
#' DEATHS numeric, simplifying their use to aesthetic plots later on.
#'
#' @return The same data frame with columns ABS_DATE (char column to preserve
#' BC years), DATE and LOCATION added plus LATITUDE, LONGITUDE,
#' EQ_PRIMARY and DEATHS columns treated from char to integer class.
#'
#' @export
eq_clean_data <- function(df){

  ## dates
  # treatment to NA month/day
  month <- ifelse( !is.na(df$MONTH),
                   ifelse(nchar(df$MONTH) < 2,
                          paste0("0", df$MONTH), df$MONTH),
                   ifelse(df$YEAR < 0, "12", "01") )
  day <- ifelse( !is.na(df$DAY),
                 ifelse(nchar(df$DAY) < 2,
                        paste0("0", df$DAY), df$DAY),
                 ifelse(df$YEAR < 0, "31", "01") )

  # getting an absolute date to not lose BC dates
  df$ABS_DATE <- paste(df$YEAR, month, day, sep = "-")
  df$DATE <- as.Date(df$ABS_DATE, format = "%Y-%m-%d")

  ## LAT/LON
  df$LATITUDE  <- as.numeric(df$LATITUDE)
  df$LONGITUDE <- as.numeric(df$LONGITUDE)

  # Cleaning and Title Case location
  df$LOCATION <- eq_location_clean(df$LOCATION_NAME)

  # Treatment to columns EQ_PRIMARY and DEATHS that could be used in plots
  df$EQ_PRIMARY <- as.numeric(df$EQ_PRIMARY)
  df$DEATHS <- as.numeric(df$DEATHS)

  return(df)
}

#' Geom class to plot earthquakes timelines.
#'
#' This object must receive an array of date class, and optionally could
#' receive parameters to y (some factor variable, it was proposed earthquake's
#' country), colour, size and alpha.
#'
#' @param **x** Array of date class
#' @param y Array of factor class. Suggested to be the earthquakes country
#' @param colour Array of class numeric
#' @param size Array of class numeric
#' @param alpha Value (from 0 to 1) to control transparency
#'
#' @return Plot layer containing points and segments grob.
#'
#' @importFrom ggplot2 ggproto Geom aes draw_key_blank
#' @importFrom grid pointsGrob gpar segmentsGrob gList gTree
#'
#' @export
GeomTimeline <- ggplot2::ggproto("GeomTimeline",
  ggplot2::Geom, required_aes = "x",
  default_aes = ggplot2::aes(y = NA, colour = "black", size = 4, alpha = 0.5),
  draw_key = ggplot2::draw_key_blank,
  draw_panel = function(data, panel_scales, coord){

    coords <- coord$transform(data, panel_scales)

    if (is.na(coords$y[1])){ coords$y <- .5 }

    coords$fill <- coords$colour

    pts <- grid::pointsGrob(
      x = coords$x, y <- coords$y,
      size = grid::unit(coords$size * 2, "mm"), pch = 19,
      gp = grid::gpar(alpha = coords$alpha, col = coords$colour,
                      fill = coords$fill)
      )
    grobster <- grid::gList(pts)

    # According to Coursera example, lines must be the same size
    xmin = min(coords$x); xmax = max(coords$x);
    for(i in unique(coords$group)){
      line <- grid::segmentsGrob(
        x0 = xmin,
        x1 = xmax,
        y0 = coords[which(coords$group == i), "y"][1],
        y1 = coords[which(coords$group == i), "y"][1]
        )
      grobster <- grid::gList(grobster, line)
    }
    grid::gTree(children = grobster)
  }
)

#'
#' Function to plot earthquakes timelines
#'
#' This function receives the ggplot2 parameters plus the Geom Timeline to add
#' the earthquakes timeline layer in a ggplot2 object
#'
#' @param mapping Set of aesthetic mappings created by aes() or aes_().
#' @param data The data to be displayed in this layer.
#' @param position Position adjustment. Could be a string or the result of a call to a position adjustment function.
#' @param ... Other arguments passed on layer like color = "red" or size = 3.
#' @param na.rm If FALSE, the default, missing values are mremoved with a warning.  If TRUE, missing values are silently removed.
#' @param show.legend logical.  Should this layer be included in the legends?  NA, the default, incldes if any aesthetics are mapped.  False never includes,
#' and TRUE always includes.
#' @param inherit.aes If FALSE, overrides the default aesthetics, rather than combining with them.  This is most usefule for helper functions that
#' define both data and aesthetics and shouldn't inherit behavior from the default plot specification, e.t. borders.
#'
#' @return The layer containing the elements created with Geom GeomTimeline
#'
#' @importFrom ggplot2 layer
#'
#' @examples
#' \dontrun{
#' readr::read_delim("earthquakes.txt", delim = "\t") %>%
#' eq_clean_data() %>%
#' dplyr::filter(COUNTRY %in% c("CHINA", "USA") & lubridate::year(DATE) >= 2000) %>%
#' ggplot2::ggplot(ggplot2::aes(x = DATE, y = COUNTRY)) +
#' geom_timeline(ggplot2::aes(size = as.numeric(EQ_PRIMARY),
#' colour = as.numeric(DEATHS))) +
#' xlab("Date") + ylab("") +
#' labs(size = "Richter scale", color = "# deaths" ) +
#' theme(axis.line.y=element_blank(), panel.background=element_blank())
#' }
#'
#' @export
geom_timeline <- function(mapping = NULL, data = NULL,
                          stat = "identity",
                          position = "identity",
                          show.legend = NA, na.rm = FALSE, inherit.aes = TRUE, ...) {
  ggplot2::layer(
    data = data, mapping = mapping, stat = stat,
    geom = GeomTimeline, position = position,
    show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

#' Geom class to plot earthquakes timeline.
#'
#' This object must receive an array of date class, and optionally could
#' receive parameters to y (some factor variable, it was proposed earthquake
#' country), colour, size and alpha.
#'
#' @param **x** Array of date class.
#' @param **label** Array of char class. Ideally contains the earthquakes
#' location treated with \code{eq_clean_data} function.
#' @param n_max Value to the maximum number of earthquakes location printed in
#' plot. It must be passed an array to size parameter in GeomTimeline to work.
#'
#' @importFrom ggplot2 ggproto Geom aes draw_key_blank
#' @importFrom grid textGrob unit gpar polylineGrob gList
#'
#' @export
GeomTimelineLabel <-
  ggplot2::ggproto("GeomTimelineLabel", ggplot2::Geom,
    required_aes = c("x", "label"),
    default_aes = ggplot2::aes(y = 0.3, size = 1.0,
                               colour = "grey", fill = "grey", n_max = NA),
    draw_key = ggplot2::draw_key_blank,
    draw_panel = function(data, panel_scales, coord){

      coords <- coord$transform(data,panel_scales)

      # slicing data from any number of countries
      if(!is.na(coords$n_max[1])){
        tmp <- NULL; tmpdf <- NULL
        for(i in unique(coords$group)){
          tmp <- coords[which(coords$group == i),]
          tmp <- tmp[order(tmp$size, decreasing = TRUE)[1:coords$n_max[1]],]
          tmpdf <- rbind(tmpdf, tmp)
          }
        coords <- tmpdf
        }
      locations <- grid::textGrob(
        label = coords$label,
        x = grid::unit(coords$x, "npc"),
        y = grid::unit(coords$y + 0.05, "npc"),
        just = c("left", "bottom"),
        gp = grid::gpar(fontsize = 12, col = 'black'),
        rot = 60
        )
      lines <- grid::polylineGrob(
        x = grid::unit(c(coords$x, coords$x), "npc"),
        y = grid::unit(c(coords$y + .01, coords$y + .04), "npc"),
        id = rep(1:length(coords$x),2),
        gp = grid::gpar(col = "grey")
        )
      grid::gList(locations, lines)
      }
    )

#'
#' Function to plot earthquakes timelines
#'
#' This function receives the ggplot2 parameters plus the Geom Timeline to add
#' the earthquakes timeline layer in a ggplot2 object
#'
#' @param mapping Set of aesthetic mappings created by aes() or aes_().
#' @param data The data to be displayed in this layer.
#' @param position Position adjustment. Could be a string or the result of a call to a position adjustment function.
#' @param ... Other arguments passed on layer like color = "red" or size = 3.
#' @param na.rm If FALSE, the default, missing values are mremoved with a warning.  If TRUE, missing values are silently removed.
#' @param show.legend logical.  Should this layer be included in the legends?  NA, the default, incldes if any aesthetics are mapped.  False never includes,
#' and TRUE always includes.
#' @param inherit.aes If FALSE, overrides the default aesthetics, rather than combining with them.  This is most usefule for helper functions that
#' define both data and aesthetics and shouldn't inherit behavior from the default plot specification, e.t. borders.
#'
#' @return The layer containing the elements created with Geom GeomTimeline
#'
#' @importFrom ggplot2 layer
#'
#' @examples
#' \dontrun{
#' readr::read_delim("earthquakes.txt", delim = "\t") %>%
#' eq_clean_data() %>%
#' dplyr::filter(COUNTRY %in% c("CHINA", "USA") & lubridate::year(DATE) >= 2000) %>%
#' ggplot2::ggplot(ggplot2::aes(x = DATE, y = COUNTRY, size = EQ_PRIMARY, colour = DEATHS)) +
#' geom_timeline() +
#' geom_timeline_label(ggplot2::aes(label = LOCATION, n_max = 6)) +
#' xlab("Date") + ylab("") +
#' labs(size = "Richter scale", color = "# deaths" ) +
#' theme(axis.line.y=element_blank(),panel.background=element_blank())
#' }
#'
#' @export
geom_timeline_label <- function(mapping = NULL, data = NULL, stat = "identity",
                                position = "identity", na.rm = FALSE,
                                show.legend = NA, inherit.aes = TRUE, ...) {
  ggplot2::layer(
    geom = GeomTimelineLabel, mapping = mapping,
    data = data, stat = stat, position = position,
    show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

#' Function to plot earthquakes sites in a map
#'
#' This function receives a data frame and a column with annotation to be
#' plotted inside a popup in each earthquake.
#'
#' @param data Dataframe with earthquake data
#' @param annot_col Char array with html data to be plotted in a popup
#'
#' @return Map with earthquakes sites
#'
#' @importFrom leaflet leaflet addTiles addCircleMarkers
#' @importFrom magrittr `%>%`
#'
#' @examples
#' \dontrun{
#' readr::read_delim("earthquakes.txt", delim = "\t") %>%
#' eq_clean_data() %>%
#' dplyr::filter(COUNTRY %in% c("CHINA", "USA") & lubridate::year(DATE) >= 2000) %>%
#' mutate(popup_text = eq_create_label(.)) %>%
#' eq_map()
#' }
#'
#' @export
eq_map <- function(data, annot_col) {
  leaflet::leaflet() %>%
    leaflet::addTiles() %>%
    leaflet::addCircleMarkers(data = data,
                              lng = ~ LONGITUDE,
                              lat = ~ LATITUDE,
                              radius = ~ EQ_PRIMARY,
                              popup = ~ data[[annot_col]])
}

#' Function to create popup text to leaflet map
#'
#' This function receives a dataframe and return an array of char class
#' containing a string in a HTML format with informations from three
#' columns.
#'
#' @param data Dataframe with earthquake data
#'
#' @return column with HTML formatted data
#'
#' @examples
#' \dontrun{
#' readr::read_delim("earthquakes.txt", delim = "\t") %>%
#' eq_clean_data() %>%
#' dplyr::filter(COUNTRY %in% c("CHINA", "USA") & lubridate::year(DATE) >= 2000) %>%
#' mutate(popup_text = eq_create_label(.)) %>%
#' eq_map()
#' }
#'
#' @export
eq_create_label <- function(data) {
  paste("<b>Location: </b>", data$LOCATION, "<br>",
        "<b>Magnitude: </b>", data$EQ_PRIMARY, "<br>",
        "<b>Total deaths: </b>", data$TOTAL_DEATHS, "<br>")
}
