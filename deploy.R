rsconnect::setAccountInfo(
  name = "huangzhaohua25085502",   # 替换为你的正确账户名（无短横线）
  token = "88BCDD4282BA762E65041D55C5990639",
  secret = "PR5mrINEDiAe+o9ofyL8UaFfPcqNC+Ad3j51Fr9x"
)
rsconnect::deployApp(appDir = ".", appName = "movie-dashboard", forceUpdate = TRUE)
