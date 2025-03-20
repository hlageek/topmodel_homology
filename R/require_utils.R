lib_require <- function(x) {
    if (!require(x, character.only = TRUE)) {
        install.packages(x, dependencies = TRUE)
    }
    library(x, character.only = TRUE)
}

data_require <- function(x) {
    if (!file.exists(x$path)) {
        download.file(x$source, x$path)
    }
}

