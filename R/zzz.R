.onLoad <- function(libname, pkgname) {
  register_filter(
    name   = "rhy",
    label  = "rhapsody (.rhy)",
    ext    = ".rhy",
    import = rhy_import,
    export = rhy_export
  )
  register_filter(
    name   = "desolve",
    label  = "deSolve R script (.R)",
    ext    = ".R",
    export = desolve_export
  )
}
