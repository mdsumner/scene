##

#' Query Sentinel imagery
#'
#' user inputs a longitude,latitude and a width and height
#'
#' Scenes are returned in the decreasing order of file size of the processed local scene.
#' @param x location
#' @param date vector of date(-times)
#' @param wh width height in metres around 'location'
#' @param proj projection for scene
#' @param res resolution desired (a default for a reasonable image size is provided)
#' @param silent emit messages
#' @param dry_run if TRUE only return the stac query
#'
#' @return data frame of available scenes and some processed links
#' @export
#'
#' @examples
#' scene(dry_run = TRUE)
scene <- function(x = cbind(146.614867, -43.298699),
                  date = Sys.Date() + c(-14, -2),
                  wh = c(5000), proj = NULL,
                  res = max(c(10, wh/1024)),
                  silent = FALSE, dry_run = FALSE) {
  x <- x[1L, , drop = FALSE]
    if (is.null(proj)) {
      proj <- sprintf("+proj=laea +lon_0=%f +lat_0=%f", x[1], x[2])
      mp <- cbind(0, 0)
    } else {
      mp <- reproj::reproj_xy(x, proj, source = "EPSG:4326")
    }
  wh <- rep(wh, length.out = 2L)
  ex <- c(-1, 1, -1, 1) * rep(wh, each = 2L)/2 + rep(mp, each = 2L)

  stacex <- reproj::reproj_extent(ex, "EPSG:4326", source = proj)

  qu <- sds::stacit(stacex, date, limit = 10000)

  srcs <- try(hrefs(qu), silent = TRUE)
  if (inherits(srcs, "try-error")) stop("stac query failed, cannot read", qu)
  properties <- props(qu)

  srcs$solarday <- solarday(properties$datetime)

  l <- split(srcs, srcs$solarday)
  ##return(l)
  if (!silent) {
    message(sprintf("processing %i scenes from %i (solar) days", nrow(srcs), length(l)))

    message(sprintf("in longlat region lonmin,lonmax,latmin,latmax %f,%f,%f,%f", stacex[1], stacex[2], stacex[3], stacex[4]))
  }

if (dry_run) return(srcs)
 out <- furrr::future_map(l, function(.x) {
   #vrt <- vapour::buildvrt(sprintf("/vsicurl/%s", c(.x$red, .x$green, .x$blue)))
   dsn0 <- vapour::gdal_raster_dsn(sprintf("/vsicurl/%s", .x$red), target_crs = proj, target_res = res,
                           target_ext = ex)[[1L]]
   dsn1 <- vapour::gdal_raster_dsn(sprintf("/vsicurl/%s", .x$green), target_crs = proj, target_res = res,
                                     target_ext = ex)[[1L]]
   dsn2 <- vapour::gdal_raster_dsn(sprintf("/vsicurl/%s", .x$blue), target_crs = proj, target_res = res,
                                     target_ext = ex)[[1L]]
   vrt <- vapour::buildvrt(c(dsn0, dsn1, dsn1))
   dsn <- vapour::gdal_raster_dsn(vrt)

   tibble::tibble(date = .x$solarday[1L],
                                                         dsn = dsn,
                                                         dsn_sources = list(sprintf("/vsicurl/%s", c(.x$red, .x$green, .x$blue))),
                                                         sources = list(.x),
                                                         spec = list(vapour::vapour_raster_info(dsn))
                                                         )})

dd <- do.call(rbind, out)
dd$filesize <- file.info(dd$dsn)$size
 dplyr::arrange(dd, dplyr::desc(filesize))

}



scene_ex <- function(x,
                  date = Sys.Date() + c(-14, -2),
                   proj = NULL,

                  silent = FALSE, keep = 6L, limit = 1000) {
  if (is.null(proj)) {
    stop("proj must not be NULL")
  }
  ex <- x
  stacex <- reproj::reproj_extent(ex, "EPSG:4326", source = proj)

  qu <- sds::stacit(stacex, date, limit = limit)

  srcs <- try(hrefs(qu), silent = TRUE)
  if (inherits(srcs, "try-error")) stop("stac query failed, cannot read", qu)
  properties <- props(qu)

  srcs$solarday <- solarday(properties$datetime)

  l <- split(srcs, srcs$solarday)
 #return(l)
  keepl <- length(l)
  mk <- min(c(keep, keepl))
  if (!silent) {
    message(sprintf("processing %i (keep) of %i scenes from %i (solar) days", mk, nrow(srcs), length(l)))

    message(sprintf("in longlat region lonmin,lonmax,latmin,latmax %f,%f,%f,%f", stacex[1], stacex[2], stacex[3], stacex[4]))
  }


  out <- furrr::future_map(l, function(.x) tibble::tibble(date = .x$solarday[1L],
                                                          dsn = vapour::gdal_raster_dsn(sprintf("/vsicurl/%s", .x$visual), target_crs = proj,
                                                                                        target_ext = ex)[[1L]],
                                                          sources = list(.x$visual)))

  dd <- do.call(rbind, out)
  dd <- dd[order(file.info(dd$dsn)$size, decreasing = TRUE), ]
  idx <- seq(1, mk)
  dd[idx, ]
}
