# internal function to read a stac query and get all its hrefs (every variable)
hrefs0 <- function(x) {
  a <- jsonlite::fromJSON(x)
  l <- lapply(a$features$assets, \(.x) .x$href)
  nms <- names(a$features$assets)

  names(l) <- nms
  hrefs <- tibble::as_tibble(l)
  if(("next" %in% a$links$rel)) {
    idx <- which("next" == a$links$rel)
    hrefs <- rbind(hrefs, Recall(a$links$href[idx]))
  }
  hrefs
}
## vectorized hrefs from a stac query
hrefs <- function(x) {
  out <- NULL
  for (x0 in x) out <- rbind(out, hrefs0(x0))
  out
}

props0 <- function(x) {
  a <- jsonlite::fromJSON(x)
  props <- a$features$properties
  if ("next" %in% a$links$rel) {
    idx <- which("next" == a$links$rel)
    props <- rbind(props, Recall(a$links$href[idx]))
  }
  props
}
props <- function(x) {
  out <- NULL
  out <- NULL
  for (x0 in x) out <- rbind(out, props0(x0))
  out
}
solarday <- function(x, lon =0 )  as.Date(round(as.POSIXct(x, tz = "UTC") + (lon %/% 15 * 3600), "days"))
