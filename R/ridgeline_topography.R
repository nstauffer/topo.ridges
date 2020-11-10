#' Convert a matrix to a data frame
#' @description Convert a matrix of elevation data where columns represent the x component and rows represent the y to a data frame
#' @param matrix The matrix of elevation values to be converted
#' @return A data frame with the variables "x", "y", and "elev"
#' @export
matrix_to_dataframe <- function(matrix){
  df <-as.data.frame(matrix)
  df[["y"]] <- nrow(df):1
  df_tall <- tidyr::pivot_longer(data = df,
                                      cols = tidyr::matches("[^y]"),
                                      names_to = "x",
                                      values_to = "elev")
  df_tall[["x"]] <- as.numeric(gsub(df_tall[["x"]],
                                         pattern = "[A-z]",
                                         replacement = ""))

  # df <- as.data.frame(matrix)
  # names(df) <- paste(1:ncol(df))
  # df[["y"]] <- as.numeric(nrow(df):1)
  # df_tall <- tidyr::pivot_longer(data = df,
  #                                cols = dplyr::matches("[^y]"),
  #                                names_to = "x",
  #                                values_to = "elev"#,
  #                                #-y)
  # )
  # df_tall[["x"]] <- as.numeric(df_tall[["x"]])

  return(df_tall)
}

# Note that the kernel is rectangular
kernel_scan <- function(matrix,
                        search_distance_x = 50,
                        search_distance_y = 20,
                        search_value_min = 10,
                        search_value_max = Inf,
                        return_value = 1.5) {
  n_row <- nrow(matrix)
  n_col <- ncol(matrix)

  min_y_vector <- sapply(X = 1:n_row,
                         search_distance = search_distance_y,
                         FUN = function(X, search_distance){
                           max(1, X - search_distance)
                         })
  max_y_vector <- sapply(X = 1:n_row,
                         max = n_row,
                         search_distance = search_distance_y,
                         FUN = function(X, max, search_distance){
                           min(max, X + search_distance)
                         })

  min_x_vector <- sapply(X = 1:n_col,
                         search_distance = search_distance_x,
                         FUN = function(X, search_distance){
                           max(1, X - search_distance)
                         })
  max_x_vector <- sapply(X = 1:n_col,
                         max = n_col,
                         search_distance = search_distance_x,
                         FUN = function(X, max, search_distance){
                           min(max, X + search_distance)
                         })

  df <- data.frame(x = unlist(lapply(X = 1:n_col, times = n_row, FUN = rep)),
                   y = rep(1:n_row, times = n_col),
                   min_x = unlist(lapply(X = min_x_vector, times = n_row, FUN = rep)),
                   max_x = unlist(lapply(X = max_x_vector, times = n_row, FUN = rep)),
                   min_y = rep(min_y_vector, times = n_col),
                   max_y = rep(max_y_vector, times = n_col))


  check_vector <- sapply(X = 1:nrow(df),
                         df = df,
                         matrix = matrix,
                         search_value_max = search_value_max,
                         search_value_min = search_value_min,
                         FUN = function(X, df, matrix, search_value_max, search_value_min){
                           current_value <- matrix[df[X, "y"], df[X, "x"]]
                           if (current_value >= search_value_min & current_value <= search_value_max) {
                             result <- FALSE
                           } else {
                             x_range <- df[X, "min_x"]:df[X, "max_x"]
                             y_range <- df[X, "min_y"]:df[X, "max_y"]
                             current_matrix <- matrix[y_range, x_range]
                             result <- any(current_matrix >= search_value_min & current_matrix <= search_value_max)
                           }
                           return(result)
                         })

  output_vector <- rep(0, times = length(check_vector))
  output_vector[check_vector] <- return_value

  output_matrix <- matrix(output_vector, ncol = n_col)

  return(output_matrix)
}

ridgemap <- function(elev_df = NULL,
                     elev_matrix = NULL,
                     line_color = "gray20",
                     background_color = "white",
                     line_count = NULL,
                     tilt_factor = 75,
                     scale_factor = 1,
                     size = 0.5,
                     min_height = 0){
  if (is.null(elev_df) & is.null(elev_matrix)) {
    stop("Provide either elev_df or elev_matrix")
  }

  if (!is.null(elev_matrix)) {
    elev_df <- matrix_to_dataframe(elev_matrix)
  }


  if (is.null(line_count)) {
    plotting_data <- elev_df
    line_count <- length(unique(elev_df[["y"]]))
  } else {
    current_lines <- length(unique(elev_df[["y"]]))
    y_values <- round((1:line_count) * (current_lines / line_count))
    plotting_data <- elev_df[elev_df[["y"]] %in% y_values, ]
    plotting_data[["y"]] <- rep(c(length(unique(plotting_data[["y"]])):1), times = length(unique(plotting_data[["x"]])))
  }

  ridge_map <- ggplot2::ggplot(plotting_data) +
    ggridges::geom_ridgeline(ggplot2::aes(x = x,
                                          y = y * tilt_factor,
                                          group = y,
                                          height = elev),
                             min_height = min_height,
                             scale = scale_factor,
                             color = line_color,
                             size = size,
                             fill = background_color) +
    ggplot2::theme(plot.background = ggplot2::element_rect(fill = background_color),
                   panel.background = ggplot2::element_rect(fill = background_color,
                                                            color = background_color),
                   panel.border = ggplot2::element_blank(),
                   panel.grid = ggplot2::element_blank(),
                   panel.spacing = ggplot2::unit(c(0,0,0,0),"mm"),
                   axis.line = ggplot2::element_blank(),
                   axis.title = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank(),
                   axis.text = ggplot2::element_blank(),
                   plot.margin = ggplot2::unit(c(0,0,0,0),"mm"),
                   legend.position = "none") +
    ggplot2::labs(x = NULL,
                  y = NULL)

  return(ridge_map)
}

ridgemap_water <- function(elev_matrix,
                           water_matrix = NULL,
                           land_color = "gray20",
                           water_color = "gray70",
                           background_color = "white",
                           line_count = NULL,
                           tilt_factor = 75,
                           scale_factor = 1,
                           size = 0.5,
                           min_height = 0,
                           search_distance_x = 10,
                           search_distance_y = 3,
                           search_value_min = NULL,
                           search_value_max = NULL,
                           return_value = NULL){

  if (is.null(water_matrix)) {
    if (is.null(search_value_min)) {
      search_value_min <- min_height
    }
    if (is.null(search_value_max)) {
      search_value_max <- Inf
    }
    if (is.null(return_value)) {
      return_value <- min_height + 0.01
    }
    water_matrix <- kernel_scan(matrix = elev_matrix,
                                search_distance_x = search_distance_x,
                                search_distance_y = search_distance_y,
                                search_value_min = search_value_min,
                                search_value_max = search_value_max,
                                return_value = return_value)
  }

  water_df <- matrix_to_dataframe(water_matrix)

  elev_df <- matrix_to_dataframe(elev_matrix)

  if (is.null(line_count)) {
    plotting_data_water <- water_df
    line_count <- length(unique(water_df[["y"]]))
  } else {
    current_lines <- length(unique(water_df[["y"]]))
    y_values <- round((1:line_count) * (current_lines / line_count))
    plotting_data_water <- water_df[water_df[["y"]] %in% y_values, ]
    plotting_data_water[["y"]] <- rep(c(length(unique(plotting_data_water[["y"]])):1), times = length(unique(plotting_data_water[["x"]])))
  }

  ridgemap <- ggplot2::ggplot(plotting_data_water) +
    ggridges::geom_ridgeline(ggplot2::aes(x = x,
                                          y = y * tilt_factor,
                                          group = y,
                                          height = elev),
                             min_height = min_height,
                             scale = scale_factor,
                             color = water_color,
                             size = size,
                             fill = background_color) +
    ggplot2::theme(plot.background = ggplot2::element_rect(fill = background_color),
                   panel.background = ggplot2::element_rect(fill = background_color,
                                                            color = background_color),
                   panel.border = ggplot2::element_blank(),
                   panel.grid = ggplot2::element_blank(),
                   panel.spacing = ggplot2::unit(c(0,0,0,0),"mm"),
                   axis.line = ggplot2::element_blank(),
                   axis.title = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank(),
                   axis.text = ggplot2::element_blank(),
                   plot.margin = ggplot2::unit(c(0,0,0,0),"mm"),
                   legend.position = "none") +
    ggplot2::labs(x = NULL,
                  y = NULL)


  if (is.null(line_count)) {
    plotting_data_land <- elev_df
    line_count <- length(unique(elev_df[["y"]]))
  } else {
    current_lines <- length(unique(elev_df[["y"]]))
    y_values <- round((1:line_count) * (current_lines / line_count))
    plotting_data_land <- elev_df[elev_df[["y"]] %in% y_values, ]
    plotting_data_land[["y"]] <- rep(c(length(unique(plotting_data_land[["y"]])):1), times = length(unique(plotting_data_land[["x"]])))
  }

  ridgemap <- ridgemap + ggridges::geom_ridgeline(data = plotting_data_land,
                                                  ggplot2::aes(x = x,
                                                               y = y * tilt_factor,
                                                               group = y,
                                                               height = elev),
                                                  min_height = min_height,
                                                  scale = scale_factor,
                                                  color = land_color,
                                                  size = size,
                                                  fill = background_color)



  return(ridgemap)
}

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
      message("No frame provided; using full raster extent.")
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

  # Drop the resolution on both axes
  max.y <- nrow(elev.df)
  max.x <- ncol(elev.df)
  # By resolution
  y.vector.length <- round(max.y*y.resolution)
  y.vector <- (1:y.vector.length)*round(max.y/y.vector.length)
  y.vector <- y.vector[y.vector < max.y]
  x.vector.length <- round(max.x*x.resolution)
  x.vector <- (1:x.vector.length)*round(max.x/x.vector.length)
  x.vector <- x.vector[x.vector < max.x]

  # So we can keep the original coordinates and not the new compressed, relative coordinates
  # names(elev.df) <- paste(1:max.x)
  # Make the adjustments
  # elev.df.reduced <- elev.df[, x.vector]
  # So we can keep the original coordinates and not the new compressed, relative coordinates
  # elev.df.reduced$y <- 1:max.y
  # Make the adjustments
  # elev.df.reduced <- elev.df.reduced[y.vector,]
  elev.df.reduced <- elev.df[y.vector, x.vector]

  # By n, if relevant
  if (is.null(n.y)) {
    y.slice <- 1:nrow(elev.df.reduced)
  } else {
    # How many of the indices to drop
    y.increment <- round((nrow(elev.df.reduced))/n.y)

    # Get only the indices to keep
    y.slice <- (1:n.y)*y.increment

    # Make sure that all of them are actually indices in the vector
    y.slice <- y.slice[y.slice <= nrow(elev.df.reduced)]
  }
  if (is.null(n.x)) {
    # x.slice <- 1:(ncol(elev.df.reduced) - 1)
    x.slice <- 1:ncol(elev.df.reduced)
  } else {
    # How many of the indices to drop
    # x.increment <- round((ncol(x.vector) - 1)/n.x)
    x.increment <- round(ncol(elev.df.reduced)/n.x)

    # Get only the indices to keep
    x.slice <- (1:n.x)*x.increment

    # Make sure that all of them are actually indices in the vector
    # x.slice <- x.slice[x.slice <= (ncol(x.vector) - 1)]
    x.slice <- x.slice[x.slice <= ncol(elev.df.reduced)]
  }

  # Make the adjustments
  # ys <- elev.df.reduced[y.slice, "y"]
  # elev.df.reduced <- dplyr::select(elev.df.reduced, -y)[y.slice, x.slice]
  # elev.df.reduced$y <- ys
  elev.df.reduced <- elev.df.reduced[y.slice, x.slice]

  names(elev.df.reduced) <- paste(1:(ncol(elev.df.reduced)))

  elev.df.reduced$y <- nrow(elev.df.reduced):1

  # Gather the frame so that we have an x coordinate variable and an elevation variable
  elev.df.tall <- tidyr::gather(elev.df.reduced,
                                key = "x",
                                value = "elev",
                                -y)

  # Convert the x values to numeric (they're still strings because they were variable names)
  elev.df.tall$x <- as.numeric(elev.df.tall$x)

  # Figure out what the minimum elevation was
  min.elev <- min(elev.df.tall$elev, na.rm = TRUE)

  # Use that minimum to convert the elevations to relative elevations
  elev.df.tall$elev[!is.na(elev.df.tall$elev)] <- elev.df.tall$elev[!is.na(elev.df.tall$elev)] - min.elev

  # Replace NAs
  elev.df.tall$elev[is.na(elev.df.tall$elev)] <- na.sub


  # For a density plot, we need each coordinate repeated a number of time proportional to its elevation
  # Creates a data frame for each observation, repeating the the x and y observation elevation*scaling factor times
  df.list <- lapply(1:nrow(elev.df.tall), df = elev.df.tall,
                    FUN = function(X, df){
                      if (df$elev[X] %in% c(NA, 0)) {
                        return(NULL)
                      }
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
# #' @param line.gradient.mid Character string. The color for the foreground ridgeline of the plot. Used as the midpoint of an auto-generated color gradient through to the backmost ridgeline. Must be a color that ggplot can recognize (either the color name or hexadecimal); passed to \code{scale_color_continuous(low)}. Defaults to \code{"white"}.
#' @param line.gradient.end Character string. The color for the backmost ridgeline of the plot. Used as the end of an auto-generated color gradient through to the backmost ridgeline. Must be a color that ggplot can recognize (either the color name or hexadecimal); passed to \code{scale_color_continuous(high)}. Defaults to \code{"white"}.
# #' @param gradient.midpoint Optional numeric value. The y value that corresponds to the color \code{line.gradient.mid}. If \code{NULL} then the mean of the unique y values will be used. Defaults to \code{NULL}.
#' @param invert.y Logical. If \code{TRUE} then all y values will by multiplied by -1. Defaults to \code{FALSE}.
#' @param invert.x Logical. If \code{TRUE} then all x values will by multiplied by -1. Defaults to \code{FALSE}.
#' @return A ggplot object.
#' @export
topo.ridges.plot <- function(data,
                             x.variable = "x",
                             y.variable = "y",
                             scale = 1,
                             size = 1,
                             fill = "light blue",
                             line.gradient.start = "white",
                             # line.gradient.mid = "white",
                             line.gradient.end = "white",
                             # gradient.midpoint = NULL,
                             invert.y = FALSE,
                             invert.x = FALSE){
  if (!isTRUE(is.data.frame(data))) {
    stop("The data must be a data frame.")
  } else {
    if (!(x.variable %in% names(data)) | !(y.variable %in% names(data))) {
      stop("One or both of the variable names provided for x.variable and y.variable do not exist in the data frame")
    }
    data$x <- data[[x.variable]]
    data$y <- data[[y.variable]]
  }

  if (invert.y) {
    data$y <- data$y * -1
  }
  if (invert.x) {
    data$x <- data$x * -1
  }

  # if (is.null(gradient.midpoint)) {
  #   gradient.midpoint <- mean(data$y)
  # }

  attributes <- list(scale = scale,
                     size = size,
                     fill = fill,
                     # gradient.midpoint,
                     line.gradient.start = line.gradient.start,
                     # line.gradient.mid = line.gradient.mid,
                     line.gradient.end = line.gradient.end)

  plot <- ggplot2::ggplot(data) +
    ggridges::geom_density_ridges(ggplot2::aes(x = x,
                                               y = y,
                                               group = y,
                                               scale = attributes$scale,
                                               color = y),
                                  size = attributes$size,
                                  fill = attributes$fill) +
    ggplot2::scale_color_continuous(low = attributes$line.gradient.start, high = attributes$line.gradient.end) +
    # ggplot2::scale_color_gradient2(low = attributes$line.gradient.start,
    #                                mid = attributes$line.gradient.mid,
    #                                high = attributes$line.gradient.high,
    #                                midpoint = attributes$gradient.midpoint) +
    ggplot2::theme(plot.background = ggplot2::element_rect(fill = attributes$fill),
                   panel.background = ggplot2::element_rect(fill = attributes$fill,
                                                            color = attributes$fill),
                   panel.border = ggplot2::element_blank(),
                   panel.grid = ggplot2::element_blank(),
                   panel.spacing = ggplot2::unit(c(0,0,0,0),"mm"),
                   axis.line = ggplot2::element_blank(),
                   axis.title = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank(),
                   axis.text = ggplot2::element_blank(),
                   plot.margin = ggplot2::unit(c(0,0,0,0),"mm"),
                   legend.position = "none") +
    ggplot2::labs(x = NULL,
                  y = NULL)

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
#' @param invert.y Logical. If \code{FALSE} then all y values will by multiplied by -1. Defaults to \code{FALSE}.
#' @param invert.y Logical. If \code{FALSE} then all y values will by multiplied by -1. Defaults to \code{FALSE}.
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
                        invert.y = FALSE,
                        invert.x = FALSE) {

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
                           invert.y = invert.y,
                           invert.x = invert.x)

  return(plot)
}

#' Convert an elevation raster to a sequence of data frames suitable for density plotting
#' @description Creates a list of long/tall data frames from an elevation raster. Each coordinate pair is repeated the according to its relative elevation within the raster, e.g. if the minimum elevation is 100 and the maximum is 200, coordinates with an elevation of 100 will be repeated once and those with an elevation of 200 will be repeated 100 times. Each of the data frames is a subset of the raster data intended to be used to render a sequence of plots.
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
#' @param n.frames Numeric value. An integer determining the number of data frames to produce.
#' @param perfect.loop Logical. If \code{TRUE} then either the number of frames or the number of ridges will be adjusted (determined by \code{loop.adjustment}) as necessary to create a perfectly-looping set of data frames. Defaults to \code{FALSE}.
#' @param loop.adjustment Character string. Used to adjust the results to create a perfect loop only if \code{perfect.loop} is \code{TRUE}. Accepted values are \code{"start"} (Rows will be dropped from the start of the data frame), \code{"end"} (Rows will be dropped from the end of the data frame), \code{"equal"} (Rows will be dropped from the start and end of the data frame in equal measure, dropping an extra from the end if the adjustment requires an odd number), and \code{"random"} (Rows will be dropped at random indices in the data frame). Defaults to \code{"equal"}.
#' @return List of data frames with variables x and y, each observation corresponding to a cell in the original raster. Coordinates in x and y are relative to raster extent, not CRS, and observations are repeated in proporition to their relative cell value. Each data frame represents one plot's subset in a sequence of plots to use as frames in an animation.
#' @export
elev.anim.df <- function(raster = NULL,
                         raster.path = NULL,
                         frame = NULL,
                         frame.path = NULL,
                         y.resolution = 1,
                         n.y = NULL,
                         x.resolution = 1,
                         n.x = NULL,
                         elev.scale = 1,
                         na.sub = 0,
                         n.frames = 30,
                         perfect.loop = FALSE,
                         loop.adjustment = "equal"){
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
      message("No frame provided; using full raster extent.")
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

  # Drop the resolution on both axes
  max.y <- nrow(elev.df)
  max.x <- ncol(elev.df)
  # By resolution
  y.vector.length <- round(max.y*y.resolution)
  y.vector <- (1:y.vector.length)*round(max.y/y.vector.length)
  x.vector.length <- round(max.x*x.resolution)
  x.vector <- (1:x.vector.length)*round(max.x/x.vector.length)

  # So we can keep the original coordinates and not the new compressed, relative coordinates
  # names(elev.df) <- paste(1:max.x)
  # Make the adjustments
  # elev.df.reduced <- elev.df[, x.vector]
  # So we can keep the original coordinates and not the new compressed, relative coordinates
  # elev.df.reduced$y <- 1:max.y
  # Make the adjustments
  # elev.df.reduced <- elev.df.reduced[y.vector,]
  elev.df.reduced <- elev.df.reduced[y.vector, x.vector]

  # By n, if relevant
  if (is.null(n.y)) {
    y.slice <- 1:nrow(elev.df.reduced)
  } else {
    # How many of the indices to drop
    y.increment <- round((nrow(elev.df.reduced))/(n.y*n.frames))

    # Get only the indices to keep
    y.slice <- (1:(n.y*n.frames))*y.increment

    # Make sure that all of them are actually indices in the vector
    y.slice <- y.slice[y.slice <= nrow(elev.df.reduced)]
  }
  if (is.null(n.x)) {
    # x.slice <- 1:(ncol(elev.df.reduced) - 1)
    x.slice <- 1:ncol(elev.df.reduced)
  } else {
    # How many of the indices to drop
    # x.increment <- round((ncol(x.vector) - 1)/n.x)
    x.increment <- round(ncol(x.vector)/n.x)

    # Get only the indices to keep
    x.slice <- (1:n.x)*x.increment

    # Make sure that all of them are actually indices in the vector
    # x.slice <- x.slice[x.slice <= (ncol(x.vector) - 1)]
    x.slice <- x.slice[x.slice <= ncol(x.vector)]
  }

  # Make the adjustments
  # ys <- elev.df.reduced[y.slice, "y"]
  # elev.df.reduced <- dplyr::select(elev.df.reduced, -y)[y.slice, x.slice]
  # elev.df.reduced$y <- ys
  elev.df.reduced <- elev.df.reduced[y.slice, x.slice]

  # ANIMATION TIME
  y.increment <- round(nrow(elev.df.reduced)/n.frames)

  slice.vectors <- lapply(1:(n.frames), frames = n.frames, increment = y.increment, df.rows = nrow(elev.df.reduced),
                          FUN = function(X, frames, increment, df.rows){
                            # The -100 is overkill to make sure that we get the smallest row number
                            slice <- increment*(-100:frames) + X
                            slice <- slice[slice <= df.rows & slice > 0]
                            return(slice)
                          })

  # If we want a perfect loop
  if (isTRUE(perfect.loop)) {
    # Find the number of rows that aren't accounted for (it should be less than the interval)
    correction <- slice.vectors[[1]][2] - slice.vectors[[length(slice.vectors)]][1]

    switch(loop.adjustment,
           "frames" = {
             if (abs(correction) != 1) {
               # Decide the new number of frames
               n.frames.corrected <- frames + correction

               # Rebuild slice vectors
               slice.vectors <- lapply(1:(n.frames.corrected), frames = n.frames.corrected, increment = y.increment, df.rows = nrow(elev.df.reduced),
                                       FUN = function(X, frames, increment, df.rows){
                                         slice <- increment*(-100:frames) + X
                                         slice <- slice[slice <= df.rows & slice > 0]
                                         return(slice)
                                       })
             }
           },
           "ridges" = {
             # Decide which indices to drop
             drop.vector <- switch(ridge.drop,
                                   "start" = {
                                     # The first n rows
                                     1:correction
                                   },
                                   "end" = {
                                     # The last n rows
                                     (nrow(elev.df.reduced) - correction):nrow(elev.df.reduced)
                                   },
                                   "equal" = {
                                     # The first n/2 (rounded down) and the last n/2 (rounded up) rows
                                     c(1:(1 - floor(correction/2)), (nrow(elev.df.reduced) - ceiling(correction/2)):nrow(elev.df.reduced))
                                   },
                                   "random" = {
                                     # A random n rows. Due to the behavior of runif(), dropping the last row is extremely unlikely
                                     floor(runif(n = correction,
                                                 min = 1,
                                                 max = nrow(elev.df.reduced)))
                                   })

             # Drop those indices
             elev.df.reduced <- elev.df.reduced[-(drop.vector),]

             # Rebuild the slice vectors
             slice.vectors <- lapply(1:(n.frames), frames = n.frames, increment = y.increment, df.rows = nrow(elev.df.reduced),
                                     FUN = function(X, frames, increment, df.rows){
                                       # The -100 is overkill to make sure that we get the smallest frame number
                                       slice <- increment*(-100:frames) + X
                                       slice <- slice[slice <= df.rows & slice > 0]
                                       return(slice)
                                     })
           })
    }

  elev.df.reduced$y <- 1:nrow(elev.df.reduced)

  # Gather the frame so that we have an x coordinate variable and an elevation variable
  elev.df.tall <- tidyr::gather(elev.df.reduced,
                                key = "x",
                                value = "elev",
                                -y)

  # Convert the x values to numeric (they're still strings because they were variable names)
  elev.df.tall$x <- as.numeric(gsub(elev.df.tall$x, pattern = "[A-z]", replacement = ""))

  # Figure out what the minimum elevation was
  min.elev <- min(elev.df.tall$elev, na.rm = TRUE)

  # Use that minimum to convert the elevations to relative elevations
  elev.df.tall$elev[!is.na(elev.df.tall$elev)] <- elev.df.tall$elev[!is.na(elev.df.tall$elev)] - min.elev

  # Replace NAs
  elev.df.tall$elev[is.na(elev.df.tall$elev)] <- na.sub


  dataframes <- lapply(slice.vectors, df = elev.df.tall, elev.scale = elev.scale,
                       FUN = function(X, df, elev.scale) {
                         elev.df.tall.reduced <- df[df$y %in% X,]

                         # For a density plot, we need each coordinate repeated a number of time proportional to its elevation
                         # Creates a data frame for each observation, repeating the the x and y observation elevation*scaling factor times
                         df.list <- lapply(1:nrow(elev.df.tall.reduced), df = elev.df.tall.reduced,
                                           FUN = function(X, df){
                                             current.df <- data.frame(y = df$y[X],
                                                                      x = df$x[X],
                                                                      # alpha = df$alpha[X],
                                                                      # Applying scaling factor
                                                                      cell = 1:round(elev.scale * df$elev[X]))
                                             return(current.df[, c(#"alpha",
                                               "x", "y")])
                                           })

                         # Combine the list of data frames into a single data frame
                         df.long <- dplyr::bind_rows(df.list)

                         return(df.long)
                       })

  return(dataframes)
}
