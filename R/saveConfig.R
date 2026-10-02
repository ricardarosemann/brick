saveConfig <- function(config, path, fileName = CONFIG_COMPILED, overwrite = FALSE) {
  configFile <- attr(config, "file", exact = TRUE)
  if (!dir.exists(path)) {
    dir.create(path)
  }
  if (file.exists(configFile)) {
    file.copy(configFile, path, overwrite = overwrite)
  }
  yamlConfig <- .asYaml(config)
  yamlAttr <- .asYaml(attributes(config))
  file <- c(yamlConfig, paste("#", yamlAttr))
  writeLines(file, file.path(path, fileName))
}

.asYaml <- function(x) {
  x <- yaml::as.yaml(x)
  x <- strsplit(x, "\n")[[1]]
  x[nzchar(x)] # remove empty lines
}
