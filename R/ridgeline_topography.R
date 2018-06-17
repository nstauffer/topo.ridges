#' Convert an elevation raster to a data frame suitable for density plotting
#' @description Creates a long/tall data frame from an elevation raster. Each coordinate pair is repeated the according to its relative elevation within the raster, e.g. if the minimum elevation is 100 and the maximum is 200, coordinates with an elevation of 100 will be repeated once and those with an elevation of 200 will be repeated 100 times.
#' @param raster Raster object. The elevation raster to use. If not provided, then a raster will be read in from \code{raster.path}. Defaults to \code{NULL}.
#' @param raster.path Character string. The full filepath to the elevation raster, including filename and file extension. Passed to \code{raster::raster()}. Only used if \code{raster} is \code{NULL}. Defaults to \code{NULL}.
#' @param frame Optional spatial polygons or spatial polygons data frame. A frame to crop the elevation raster to. If \code{NULL}, then \code{frame.path} will be used. If \code{frame.path} is also \code{NULL} then no cropping will be applied. Defaults to \code{NULL}.
#' @param frame.path Optional character string. The full filepath to the frame shapefile, including filename and file extension (".shp"). Only used if \code{frame} is \code{NULL}. If both are \code{NULL} then the raster will not be cropped to a frame. Defaults to \code{NULL}.
#' @param frame.filename Character string. The filename of the shapefile (without file extension) to be read from \code{frame.path}.
#' @param y.resolution Numeric value. The effective resolution on the y axis, e.g. \code{0.1} will result in keeping 1/10th of the values whereas \code{1} will keep all of them. Smaller values can dramatically increase speed. Defaults to \code{1}.
#' @param n.y Optional numeric value. An integer number of y values to keep; determines the resolution of the plot on the y axis and the number of ridgelines (if plotting with \code{geom_density_ridges()}). This is applied after \code{y.resolution}. Will be rounded to the nearest integer and due to other rounding issues may result in \code{n.y +/- 1} unique y values. Defaults to \code{NULL}.
#' @param x.resolution Numeric value. The effective resolution on the x axis, e.g. \code{0.1} will result in keeping 1/10th of the values whereas \code{1} will keep all of them. Smaller values can dramatically increase speed. Defaults to \code{1}.
#' @param n.x Optional numeric value. An integer number of x values to keep; determines the resolution of the plot on the x axis. This is applied after \code{x.resolution}. Will be rounded to the nearest integer and due to other rounding issues may result in \code{n.x +/- 1} unique x values. Defaults to \code{NULL}.
#' @param elev.scale Optional numeric value. All elevation values are multiplied by \code{elev.scale}, which, when between 0 and 1 can increase speed by reducing the number of rows of in the data frame. This will have a flattening effect on the ridges in the output plot. Defaults to \code{1}.
#' @param na.sub Optional numeric value. An integer to replace any NA values in the elevation data with. Defaults to \code{0}.
#' @return Data frame with variables x and y, each observation corresponding to a cell in the original raster. Coordinates in x and y are relative to raster extent, not CRS, and observations are repeated in proporition to their relative cell value.
#' @export
elev.density.df <- function(raster = NULL,
                            raster.path = NULL,
                            frame = NULL,
                            frame.path = NULL,
                            y.resolution = 1,
                            n.y = NULL,
                            x.resolution = 1,
                            n.x = NULL,
                            elev.scale = 1,
                            na.sub = 0){
  # Sanitize
  if (y.resolution > 1 | y.resolution <= 0) {
    stop("y.resolution cannot be greater than 1 or less than or equal to 0.")
  }
  if (x.resolution > 1 | x.resolution <= 0) {
    stop("x.resolution cannot be greater than 1 or less than or equal to 0.")
  }
  if (!is.null(n.y)) {
    if (!is.numeric(n.y)) {
      stop("n.y must be a numeric value.")
    }
    n.y <- round(n.y)
  }
  if (!is.null(n.x)) {
    if (!is.numeric(n.x)) {
      stop("n.y must be a numeric value.")
    }
    n.x <- round(n.x)
  }

  # Read in the raster
  if (is.null(raster)) {
    if (is.null(raster.path)){
      stop("You must provide a raster or a filepath to a raster.")
    } else {
      if (!file.exists(raster.path)) {
        stop("Invalid raster filepath: file cannot be found.")
      }
      elev.raster <- raster::raster(raster.path)
    }
  } else {
    elev.raster <- raster
  }


  # Read in the extent frame, if there is one
  if (is.null(frame)) {
    if (is.null(frame.path)) {
      message("No frame provided.")
    } else {
      if (file.exists(frame.path)) {
        frame <- rgdal::readOGR(dsn = frame.path,
                                stringsAsFactors = FALSE)

        # Crop the raster to the frame
        elev.raster <- raster::crop(x = elev.raster,
                                    y = sp::spTransform(frame,
                                                        CRSobj = elev.raster@crs))
      } else {
        stop("Invalid frame filepath: file cannot be found.")
      }
    }
  } else {
    if (!grepl(class(frame)[1], pattern = "^SpatialPolygons")) {
      stop("The frame must be a spatial polygons or spatial polygons data frame")
    }
    # Crop the raster to the frame
    elev.raster <- raster::crop(x = elev.raster,
                                y = sp::spTransform(frame,
                                                    CRSobj = elev.raster@crs))
  }



  # Convert to a matrix
  elev.matrix <- raster::as.matrix(elev.raster)

  # Convert to a data frame
  elev.df <- as.data.frame(elev.matrix)

  # Remove the "V" in the variable names so that they can be used as coordinates
  # It's much faster to do this now than once it's tall because the vector is much smaller
  names(elev.df) <- gsub(names(elev.df), pattern = "V", replacement = "")

  # Use the row number as the y coordinate
  elev.df$y <- 1:nrow(elev.df)

  # Gather the frame so that we have an x coordinate variable and an elevation variable
  elev.df.tall <- tidyr::gather(elev.df,
                                key = "x",
                                value = "elev",
                                -y)

  # Convert the x values to numeric (they're still strings because they were variable names)
  elev.df.tall$x <- as.numeric(elev.df.tall$x)

  # Figure out what the minimum elevation was
  min.elev <- min(elev.df.tall$elev, na.rm = TRUE)

  # Use that minimum to convert the elevations to relative elevations
  elev.df.tall$elev[!is.na(elev.df.tall$elev)] <- elev.df.tall$elev[!is.na(elev.df.tall$elev)] - min.elev + 1

  # Replace NAs
  elev.df.tall$elev[is.na(elev.df.tall$elev)] <- na.sub

  # Drop the resolution on both axes
  # By resolution
  y.vector.length <- round(max(elev.df.tall$y)*y.resolution)
  y.vector <- (1:y.vector.length)*round(max(elev.df.tall$y)/y.vector.length)
  x.vector.length <- round(max(elev.df.tall$x)*x.resolution)
  x.vector <- (1:x.vector.length)*round(max(elev.df.tall$x)/x.vector.length)

  # Make the adjustments
  elev.df.tall.reduced <- elev.df.tall[elev.df.tall$y %in% (y.vector) & elev.df.tall$x %in% (x.vector),]

  # By n, if relevant
  if (is.null(n.y)) {
    y.vector <- unique(elev.df.tall.reduced$y)
  } else {
    # What are the unique y values?
    y.vector <- unique(elev.df.tall.reduced$y)

    # How many of the indices to drop
    y.increment <- round(length(y.vector)/n.y)

    # Get only the indices to keep
    y.slice <- (1:n.y)*y.increment

    # Make sure that all of them are actually indices in the vector
    y.slice <- y.slice[y.slice <= length(y.vector)]

    # Slice those unique values down by the indices to keep
    y.vector <- y.vector[y.slice]
  }
  if (is.null(n.x)) {
    x.vector <- unique(elev.df.tall.reduced$x)
  } else {
    # What are the unique x values?
    x.vector <- unique(elev.df.tall.reduced$x)

    # How many of the indices to drop
    x.increment <- round(length(x.vector)/n.x)

    # Get only the indices to keep
    x.slice <- (1:n.x)*x.increment

    # Make sure that all of them are actually indices in the vector
    x.slice <- x.slice[x.slice <= length(x.vector)]

    # Slice those unique values down by the indices to keep
    x.vector <- x.vector[x.slice]
  }

  # Make the adjustments
  elev.df.tall.reduced <- elev.df.tall[elev.df.tall$y %in% (y.vector) & elev.df.tall$x %in% (x.vector),]

  # For a density plot, we need each coordinate repeated a number of time proportional to its elevation
  # Creates a data frame for each observation, repeating the the x and y observation elevation*scaling factor times
  df.list <- lapply(1:nrow(elev.df.tall.reduced), df = elev.df.tall.reduced,
                    FUN = function(X, df){
                      current.df <- data.frame(y = df$y[X],
                                               x = df$x[X],
                                               # Applying scaling factor
                                               cell = 1:round(elev.scale * df$elev[X]))
                      return(current.df[, c("x", "y")])
                    })

  # Combine the list of data frames into a single data frame
  df.long <- dplyr::bind_rows(df.list)

  return(df.long)
}

#' Create a ridgeline plot from a data frame of relative coordinates
#' @description Takes a data frame of relative coordinates, repeated according to relative elevation, and creates a ridgeline plot. There will be one density ridgeline for each unique y axis value.
#' @param data Data frame. Must contain variables corresponding to relative coordinates of elevation data. Each observation of a coordinate pair must appear in the data frame a number of times equal to its relative elevation, e.g. a coordinate pair that is twice as high as another should occur two times as many times in the data frame.
#' @param scale Numeric value. Determines the vertical scaling of ridges; passed to \code{ggridges::geom_density_ridges(ggplot2::aes(scale))}. Defaults to \code{1}.
#' @param size Numeric value. Determines the weight of ridgelines; passed to \code{ggridges::geom_density_ridges(ggplot2::aes(size))}. Defaults to \code{1}.
#' @param fill Character string. The color for the background of the plot. Must be a color that ggplot can recognize (either the color name or hexadecimal); passed to \code{geom_density_ridges(fill)} and \code{theme(panel.background = element_rect(fill))}. Defaults to \code{"light blue"}.
#' @param line.gradient.start Character string. The color for the foreground ridgeline of the plot. Used as the start of an auto-generated color gradient through to the backmost ridgeline. Must be a color that ggplot can recognize (either the color name or hexadecimal); passed to \code{scale_color_continuous(low)}. Defaults to \code{"white"}.
#' @param line.gradient.end Character string. The color for the backmost ridgeline of the plot. Used as the end of an auto-generated color gradient through to the backmost ridgeline. Must be a color that ggplot can recognize (either the color name or hexadecimal); passed to \code{scale_color_continuous(high)}. Defaults to \code{"white"}.
#' @param invert.y Logical. If \code{TRUE} then all y values will by multiplied by -1. Defaults to \code{FALSE}.
#' @return A ggplot object.
#' @export
topo.ridges.plot <- function(data,
                             x.variable = "x",
                             y.variable = "y",
                             scale = 1,
                             size = 1,
                             fill = "light blue",
                             line.gradient.start = "white",
                             line.gradient.end = "white",
                             invert.y = FALSE){
  if (isFALSE(is.data.frame(data))) {
    stop("The data must be a data frame.")
  } else {
    if (!(x.variable %in% names(data)) | !(y.variable %in% names(data))) {
      stop("One or both of the variable names provided for x.variable and y.variable do not exist in the data frame")
    }
    data$x <- data[[x.variable]]
    data$y <- data[[y.variable]]
  }

  attributes <- list(scale = scale,
                     size = size,
                     fill = fill,
                     line.gradient.start = line.gradient.start,
                     line.gradient.end = line.gradient.end)
  if (invert.y) {
    plot <- ggplot2::ggplot(data) +
      ggridges::geom_density_ridges(ggplot2::aes(x = x,
                                                 y = -y,
                                                 group = -y,
                                                 scale = attributes$scale,
                                                 color = -y),
                                    size = attributes$size,
                                    fill = attributes$fill) +
      ggplot2::scale_color_continuous(low = attributes$line.gradient.start, high = attributes$line.gradient.end) +
      ggplot2::theme(panel.background = ggplot2::element_rect(fill = attributes$fill),
                     panel.grid = ggplot2::element_blank(),
                     axis.title = ggplot2::element_blank(),
                     axis.ticks = ggplot2::element_blank(),
                     axis.text = ggplot2::element_blank(),
                     legend.position = "none")
  } else {
    plot <- ggplot2::ggplot(data) +
      ggridges::geom_density_ridges(ggplot2::aes(x = x,
                                                 y = y,
                                                 group = y,
                                                 scale = attributes$scale,
                                                 color = y),
                                    size = attributes$size,
                                    fill = attributes$fill) +
      ggplot2::scale_color_continuous(low = attributes$line.gradient.start, high = attributes$line.gradient.end) +
      ggplot2::theme(panel.background = ggplot2::element_rect(fill = attributes$fill),
                     panel.grid = ggplot2::element_blank(),
                     axis.title = ggplot2::element_blank(),
                     axis.ticks = ggplot2::element_blank(),
                     axis.text = ggplot2::element_blank(),
                     legend.position = "none")
  }

  return(plot)
}

#' Create a ridgeline plot from an elevation raster
#' @description A wrapper for elev.density.df() and topo.ridges.plot(). Converts an elevation raster to a topographic visualization using density ridgelines.
#' @param raster Raster object. The elevation raster to use. If not provided, then a raster will be read in from \code{raster.path}. Defaults to \code{NULL}.
#' @param raster.path Character string. The full filepath to the elevation raster, including filename and file extension. Passed to \code{raster::raster()}. Only used if \code{raster} is \code{NULL}. Defaults to \code{NULL}.
#' @param frame Optional spatial polygons or spatial polygons data frame. A frame to crop the elevation raster to. If \code{NULL}, then \code{frame.path} will be used. If \code{frame.path} is also \code{NULL} then no cropping will be applied. Defaults to \code{NULL}.
#' @param frame.path Optional character string. The full filepath to the frame shapefile, including filename and file extension (".shp"). Only used if \code{frame} is \code{NULL}. If both are \code{NULL} then the raster will not be cropped to a frame. Defaults to \code{NULL}.
#' @param n.ridges Optional numeric value. An integer number of y values to keep; determines the resolution of the plot on the y axis and the number of ridgelines (if plotting with \code{geom_density_ridges()}). Will be rounded to the nearest integer. Defaults to \code{50}.
#' @param x.resolution Numeric value. The effective resolution on the x axis, e.g. \code{0.1} will result in keeping 1/10th of the values whereas \code{1} will keep all of them. Smaller values can dramatically increase speed. Defaults to \code{1}.
#' @param n.x Optional numeric value. An integer number of x values to keep; determines the resolution of the plot on the x axis. This is applied after \code{x.resolution}. Will be rounded to the nearest integer. Defaults to \code{NULL}.
#' @param elev.scale Optional numeric value. All elevation values are multiplied by \code{elev.scale}, which, when between 0 and 1 can increase speed by reducing the number of rows of in the data frame. This will have a flattening effect on the ridges in the output plot. Defaults to \code{0.1}.
#' @param na.sub Optional numeric value. An integer to replace any NA values in the elevation data with. Defaults to \code{0}.
#' @param scale Numeric value. Determines the vertical scaling of ridges; passed to \code{ggridges::geom_density_ridges(ggplot2::aes(scale))}. Defaults to \code{15}.
#' @param size Numeric value. Determines the weight of ridgelines; passed to \code{ggridges::geom_density_ridges(ggplot2::aes(size))}. Defaults to \code{1}.
#' @param fill Character string. The color for the background of the plot. Must be a color that ggplot can recognize (either the color name or hexadecimal); passed to \code{geom_density_ridges(fill)} and \code{theme(panel.background = element_rect(fill))}. Defaults to \code{"light blue"}.
#' @param line.gradient.start Character string. The color for the foreground ridgeline of the plot. Used as the start of an auto-generated color gradient through to the backmost ridgeline. Must be a color that ggplot can recognize (either the color name or hexadecimal); passed to \code{scale_color_continuous(low)}. Defaults to \code{"white"}.
#' @param line.gradient.end Character string. The color for the backmost ridgeline of the plot. Used as the end of an auto-generated color gradient through to the backmost ridgeline. Must be a color that ggplot can recognize (either the color name or hexadecimal); passed to \code{scale_color_continuous(high)}. Defaults to \code{"white"}.
#' @param invert.y Logical. If \code{TRUE} then all y values will by multiplied by -1. Defaults to \code{TRUE}.
#' @return A ggplot object.
#' @export
topo.ridges <- function(raster = NULL,
                        raster.path = NULL,
                        frame = NULL,
                        frame.path = NULL,
                        n.ridges = 50,
                        x.resolution = 1,
                        n.x = NULL,
                        elev.scale = .1,
                        na.sub = 0,
                        scale = 15,
                        size = 1,
                        fill = "light blue",
                        line.gradient.start = "white",
                        line.gradient.end = "white",
                        invert.y = TRUE) {

  # Create the data frame to plot
  df.long <- elev.density.df(raster = raster,
                             raster.path = raster.path,
                             frame = frame,
                             frame.path = frame.path,
                             y.resolution = 1,
                             n.y = n.ridges,
                             x.resolution = x.resolution,
                             n.x = n.x,
                             elev.scale = elev.scale,
                             na.sub = na.sub)

  # PLOT
  plot <- topo.ridges.plot(data = df.long,
                           x.variable = "x",
                           y.variable = "y",
                           scale = scale,
                           size = size,
                           fill = fill,
                           line.gradient.start = line.gradient.start,
                           line.gradient.end = line.gradient.end,
                           invert.y = invert.y)

  return(plot)
}
