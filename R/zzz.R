.onLoad <- function(libname, pkgname) {
  # --- Font Loading ---
  font_path <- system.file("fonts", package = pkgname)
  font_files_exist <- all(file.exists(file.path(
    font_path, c("times.ttf", "timesbd.ttf", "timesi.ttf", "timesbi.ttf")
  )))

  if (font_files_exist) {
    sysfonts::font_add(
      family = "Times New Roman",
      regular = file.path(font_path, "times.ttf"),
      bold = file.path(font_path, "timesbd.ttf"),
      italic = file.path(font_path, "timesi.ttf"),
      bolditalic = file.path(font_path, "timesbi.ttf")
    )
    showtext::showtext_auto()
  }

  # --- Startup Message ---
  v <- utils::packageVersion(pkgname)

  msg <- paste0(
    "🍬 Welcome to {", pkgname, "} v", v, " 🍭\n",
    "--------------------------------------------------------------\n",
    "✨ Your toolkit for sweet and simple data visualization.      \n",
    "📊 Type `?` and the package name for an overview of functions.\n",
    "--------------------------------------------------------------\n",
    " >> Custom 'Times New Roman' font ", if (font_files_exist) "loaded successfully." else "not found in package files."
  )

  packageStartupMessage(msg)
}
