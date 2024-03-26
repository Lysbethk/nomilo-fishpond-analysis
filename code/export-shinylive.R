shinylive::export(
  appdir = "code",
  destdir = "docs"
)

httpuv::runStaticServer(
  dir = "docs",
  port = 8888
)