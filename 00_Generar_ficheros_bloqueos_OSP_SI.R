## Crear una tabla integrada de informacion a nivel CTO desde las diferentes fuentes


## Library load

library(readxl)
#library(dataframes2xls)
library(stringr)
library(data.table)
library(zip)

## Environment cleanning

rm(list = ls())


## Utilities

# Copy data out of R
copy.table <- function(obj, size = 4096) {
  clip <- paste('clipboard-', size, sep = '')
  f <- file(description = clip, open = 'w')
  write.table(obj, f, row.names = FALSE, sep = '\t')
  close(f)  
}

# Paste data into R
paste.table <- function() {
  f <- file(description = 'clipboard', open = 'r')
  df <- read.table(f, sep = '\t', header = TRUE)
  close(f)
  return(df)
}


cols.tabla.comillas <- function(tabla.datos){
  cat(paste("c(\"", paste(colnames(tabla.datos), sep=" ", collapse = "\", \"")), "\")", sep="",collapse="")
}

cols.tabla <- function(tabla.datos){
  cat(paste("c(", paste(colnames(tabla.datos), sep="", collapse = ", ")), ")", sep="",collapse="")
}