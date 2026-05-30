cat("SHINY_ACC_NAME:", Sys.getenv("SHINY_ACC_NAME"), "\n")
cat("SHINY_TOKEN length:", nchar(Sys.getenv("SHINY_TOKEN")), "\n")
cat("SHINY_SECRET length:", nchar(Sys.getenv("SHINY_SECRET")), "\n")

rsconnect::setAccountInfo(
  name = Sys.getenv("SHINY_ACC_NAME"),
  token = Sys.getenv("SHINY_TOKEN"),
  secret = Sys.getenv("SHINY_SECRET")
)
rsconnect::deployApp(appDir = ".", appName = "movie-dashboard", forceUpdate = TRUE)
