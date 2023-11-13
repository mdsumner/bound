# #gdal_footprint vrt:///vsicurl/https://dapds00.nci.org.au/thredds/fileServer/fs38/publications/CMIP6/OMIP/CSIRO-COSIMA/ACCESS-OM2/omip2/r1i1p1f1/Omon/chl/gn/latest/chl_Omon_ACCESS-OM2_omip2_r1i1p1f1_gn_000101-001012.nc?bands=1 fp.parquet -of Parquet
#
# url <- "https://dapds00.nci.org.au/thredds/fileServer/fs38/publications/CMIP6/OMIP/CSIRO-COSIMA/ACCESS-OM2/omip2/r1i1p1f1/Omon/chl/gn/latest/chl_Omon_ACCESS-OM2_omip2_r1i1p1f1_gn_000101-001012.nc"
# sds <- vapour::vapour_sds_names(file.path("/vsicurl", url))
# xy <- bound(sds[4:3])
# plot(reproj::reproj_xy(xy, "+proj=laea +lat_0=90"), asp = 1)
bound <- function(dsn, crs = NULL, source = NULL) {
    info <- vapour::vapour_raster_info(dsn[1])
    dm <- info$dimension
    ex <- info$extent
    cell <-c(vaster::cell_from_row(dm,  1),
         vaster::cell_from_col(dm, dm[1]),
         rev(vaster::cell_from_row(dm, dm[2])),
         rev(vaster::cell_from_col(dm, 1)))

    if (length(dsn) == 2L) {
        ## get them individually
        xy <- terra::extract(terra::rast(dsn), cell)
        if (!is.null(crs)) {
            if (is.null(source)) source <- "OGC:CRS84"
            xy <- terra::project(xy, to = crs, from = crs(r))
        }
    } else {
        ## get a coord for every pixel
        r <- terra::rast(dsn)
        xy < cbind(terra::xFromCell(r, cell), terra::yFromCell(r, cell))
        if (!is.null(crs)) {
            xy <- terra::project(xy, to = crs, from = crs(r))
        }
    }
       xy
}
