# deploy.R
# 该脚本会在 GitHub Actions 中执行，自动部署到 shinyapps.io

# 从环境变量读取 GitHub Secrets 中存储的认证信息
rsconnect::setAccountInfo(
  name = Sys.getenv("SHINY_ACC_NAME"),
  token = Sys.getenv("SHINY_TOKEN"),
  secret = Sys.getenv("SHINY_SECRET")
)

# 部署当前目录的应用（appDir = "." 表示根目录）
rsconnect::deployApp(
  appDir = ".",
  appName = "movie-dashboard",   # 可自定义，在 shinyapps.io 上显示的应用名称
  account = Sys.getenv("SHINY_ACC_NAME"),
  launch.browser = FALSE,
  forceUpdate = TRUE
)
