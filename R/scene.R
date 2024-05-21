## user inputs a longitude,latitude and a width and height

#' Title
#'
#' @param x
#' @param date
#' @param wh
#' @param proj
#' @param res
#' @param silent
#'
#' @return
#' @export
#'
#' @examples
scene <- function(x = cbind(146.614867, -43.298699),
                  date = Sys.Date() + c(-14, -2),
                  wh = c(5000), proj = NULL,
                  res = max(c(10, wh/1024)),
                  silent = FALSE, keep = 6L) {
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
  keepl <- length(l)
  mk <- min(c(keep, keepl))
  if (!silent) {
    message(sprintf("processing %i (keep) of %i scenes from %i (solar) days", mk, nrow(srcs), length(l)))

    message(sprintf("in longlat region lonmin,lonmax,latmin,latmax %f,%f,%f,%f", stacex[1], stacex[2], stacex[3], stacex[4]))
  }


 out <- furrr::future_map(l, function(.x) tibble::tibble(date = .x$solarday[1L], dsn = vapour::gdal_raster_dsn(sprintf("/vsicurl/%s", .x$visual), target_crs = proj, target_res = res, target_ext = ex)[[1L]],
                                                         sources = list(.x$visual)))

dd <- do.call(rbind, out)
 dd <- dd[order(file.info(dd$dsn)$size, decreasing = TRUE), ]
 idx <- seq(1, mk)
 dd[idx, ]
}
