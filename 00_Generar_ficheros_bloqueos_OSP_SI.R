## Crear una tabla integrada de informacion a nivel CTO desde las diferentes fuentes


## Library load

library(readxl)
library(xlsx)
#library(dataframes2xls)
library(stringr)
library(data.table)
library(zip)
library(lubridate)

## Environment cleanning

rm(list = ls())


## Utilities

# Copy data out of R
copy.table <- function(obj, size = 4096) {
  clip <- paste('clipboard-', size, sep = '')
  f <- file(description = clip, open = 'w')
  write.table(obj, f, row.names = FALSE, sep = '\t', dec = ',')
  close(f)  
}

# Paste data into R
paste.table <- function() {
  f <- file(description = 'clipboard', open = 'r')
  df <- read.table(f, sep = '\t', header = TRUE, dec = ',')
  close(f)
  return(df)
}


cols.tabla.comillas <- function(tabla.datos){
  cat(paste("c(\"", paste(colnames(tabla.datos), sep=" ", collapse = "\", \"")), "\")", sep="",collapse="")
}

cols.tabla <- function(tabla.datos){
  cat(paste("c(", paste(colnames(tabla.datos), sep="", collapse = ", ")), ")", sep="",collapse="")
}

## Path and static variables definition

ocupacion.sistemas.file <- '../../000_DWH_txt_files/03_Extracciones_sistemas/Consulta_Ocupacion.txt'
inventario.sistemas.hogares.file <- '../../000_DWH_txt_files/03_Extracciones_sistemas/Consulta_Cobertura.txt'
customer.file <- '../../000_DWH_txt_files//03_Extracciones_sistemas/ClientesRedPropia_IUA.csv'
hogares.OSP.file <- 'indata/00_Direcciones_mutualizadas_MMB.txt'
reportfile.blockCTO <- './outdata/REPORTS/histCTOblock.csv'
reportfile.totalhogaresbloqueados <- './outdata/REPORTS/hogaresbloqueados.csv'


#### Get CTOs and appartments block/unblock condition

shell('R_exportar_bloqueos_NAE.MAM')
CTOs.block <- as.data.table(read_excel('../../../../compartidos/coberturaFTTH/02_total_CTO_bloqueadas.xlsx',sheet = 1, trim_ws = T))


#### Load CTOs condition from SI report

CTO.report <- as.data.table(read.csv(ocupacion.sistemas.file,
                                     header = F,
                                     sep = ';',
                                     fileEncoding = 'UTF-8',
                                     dec = ',',
                                     strip.white = T,
                                     colClasses = 'character'))
colnames(CTO.report) <- c("CTO","ACTIVOS","LIBRES","RESERVADOS","INACTIVOS", "AVERIADO", "TOTAL","OCUPACION","OLT","ESTADO_CTO_SIS","UUII","TASA_DESPLIEGUE","del1","del2")
CTO.report[, c('del1', 'del2'):=NULL]

registros.iniciales.CTO.report <- nrow(CTO.report)

#### Update condition and reason from block CTOs

CTO.report$ESTADO_CTO_SIS <- 'FREE'
CTO.report[CTO %in% CTOs.block$CTO_ID, ESTADO_CTO_SIS := 'INACTIVE']


#### Load blacklist

blacklist <- as.data.table(read_excel('../../../../compartidos/coberturaFTTH/01_DireccionesListasNegras.xlsx',sheet = 1, trim_ws = T))

#### Load apartments SI

inventario.hogares <- as.data.table(read.csv(inventario.sistemas.hogares.file, header = F, sep = ';', fileEncoding = 'UTF-8', strip.white = T, colClasses = 'character'))
colnames(inventario.hogares) <- c("GESCAL_37","ID_DOMICILIO TO","Codigo Postal",
                                  "Provincia","Poblacion","Tipo via","Nombre via","ID_TECNICO_DE_LA_VIA",
                                  "Numero","BIS","Bloque_finca","Portal_puerta","Letra","Escalera","Planta","Mano1","Mano2",
                                  "Obsservaciones/comentario","Flagdummy","Cod INE Via","Codigo Censal","Codigo Pai","OLT",
                                  "Codigo CTO","TIPO_CTO","DIRECCION_CTO","TIPO_INSTALACION",
                                  "Tipo caja de derivacion","UUII","N viviendas","Fecha_alta","Codigo CD","UBICACION_CD", "del1")

### Replace '?' in city name

inventario.hogares$Poblacion <- str_replace_all(inventario.hogares$Poblacion, '\\?', 'Ñ')
inventario.hogares$del1 <- NULL

#### Init block tag

inventario.hogares$Blacklist <- 'no'


#### Tag block x CTO

inventario.hogares[`Codigo CTO` %in% CTOs.block$CTO_ID, Blacklist := 'si']
inventario.hogares <- merge(inventario.hogares, CTOs.block[,c('CTO_ID', "Motivo bloqueo")], all.x = T, by.x = 'Codigo CTO', by.y = 'CTO_ID')


#### Tag block x apartment blacklist

inventario.hogares[`ID_DOMICILIO TO` %in% blacklist$ID_DOMICILIO, Blacklist := 'si']
inventario.hogares[`ID_DOMICILIO TO` %in% blacklist$ID_DOMICILIO, "Motivo bloqueo" := 'Infraestructura']


#### Load customers

customers.FTTH <- as.data.table(read.csv(customer.file, header = T, sep = ';', fileEncoding = 'UTF-8', strip.white = T, colClasses = 'character'))
## Eliminamos las bajas
customers.FTTH <- customers.FTTH[FECHABAJA == '', ]

#### Untag block if customer

inventario.hogares[`ID_DOMICILIO TO` %in% customers.FTTH$ID_DOMICILIO, Blacklist := 'no']
inventario.hogares[`ID_DOMICILIO TO` %in% customers.FTTH$ID_DOMICILIO, "Motivo bloqueo" := '']
inventario.hogares[Blacklist == 'si' & ("Motivo bloqueo" == '' | is.na(`Motivo bloqueo`)), `Motivo bloqueo`:= 'Infraestructura']
inventario.hogares[, .N, by = c('Blacklist', "Motivo bloqueo")]

#### Load apartments OSP

shell('exportar_domicilios_para_bloqueos_R.MAM')
OSP.hogares <- as.data.table(read.csv(hogares.OSP.file, header = T, sep = ';', fileEncoding = 'UTF-8', strip.white = T, colClasses = 'character'))
OSP.hogares$cruce <- NULL
OSP.hogares$G37 <- NULL
OSP.hogares <- OSP.hogares[, 1:30]


hogares.OSP.bloqueados <- merge(OSP.hogares, inventario.hogares[Blacklist == 'si', c("ID_DOMICILIO TO", "Motivo bloqueo")], by.x = 'ID_DOMICILIO.TO', by.y = "ID_DOMICILIO TO")


#### EXPORTS

AAMMDD <- gsub('-', '', Sys.Date())
AAMMDD <- substr(AAMMDD, 3, nchar(AAMMDD))

#### SI

inventario.hogares[is.na(`Motivo bloqueo`), `Motivo bloqueo` := '']

write.table(CTO.report[,c("OLT", "CTO", "ESTADO_CTO_SIS")], file = str_c('outdata/SI/',  AAMMDD, '_Extraccion_total_CTO_marca_bloqueo.txt', sep = '', collapse = T), sep = ";", col.names = T, fileEncoding = 'UTF-8', quote = F, na = "", row.names = F)
write.table(inventario.hogares[,c("ID_DOMICILIO TO", "GESCAL_37", "Blacklist", "Motivo bloqueo")], file = str_c('outdata/SI/',  AAMMDD, '_Extraccion_total_direcciones_marca_blacklist.txt', sep = '', collapse = T), sep = ";", col.names = T, fileEncoding = 'UTF-8', quote = F, na = "", row.names = F)

#### OSP

NNNNNNNN <- str_pad(nrow(hogares.OSP.bloqueados),width = 8,side = 'left',pad = '0')
VV <- '01'

hogares.OSP.bloqueados$ult_col <- ''

out.bloqueos.name <- str_c('outdata/OSP/BO_904_030_', AAMMDD, "_01_", NNNNNNNN, '.csv', sep = '', collapse = T)

write.table(hogares.OSP.bloqueados, file = out.bloqueos.name, sep = ";", col.names = T, fileEncoding = 'UTF-8', quote = F, row.names = F, na = "")

#### Reports

hogares.OSP.bloqueados[, .N, by = 'Motivo bloqueo']


#### Histogramas tiempos bloqueo

## tiempos de bloqueo x CTO

total.hogares.bloqueados <- inventario.hogares[Blacklist == 'si', ]
total.hogares.bloqueados <- merge(total.hogares.bloqueados, CTOs.block[, c("CTO_ID", "Fecha envio bloqueo SI")], all.x = T, by.x = 'Codigo CTO', by.y = 'CTO_ID')
setnames(total.hogares.bloqueados, "Fecha envio bloqueo SI", "Fecha.bloqueo.CTO")
total.hogares.bloqueados$nivel.bloqueo <- "Blacklist"
total.hogares.bloqueados[!is.na(Fecha.bloqueo.CTO) , nivel.bloqueo := 'bloqueoCTO']
total.hogares.bloqueados[nivel.bloqueo == 'bloqueoCTO', semanas.bloqueado := as.integer((difftime(Sys.Date() , (Fecha.bloqueo.CTO), units = "weeks")))]

total.hogares.bloqueados$Mutualizado <- 'no'
total.hogares.bloqueados[`ID_DOMICILIO TO` %in% hogares.OSP.bloqueados$ID_DOMICILIO.TO, Mutualizado := 'si']

write.table(total.hogares.bloqueados[nivel.bloqueo == 'bloqueoCTO' & `Motivo bloqueo` == "Saturacion", .N, by = c("Provincia", "Poblacion", "Codigo CTO", "semanas.bloqueado", "Mutualizado")],
            file = reportfile.blockCTO, sep = ";", col.names = T, fileEncoding = 'UTF-8', quote = F, row.names = F, na = "")

hist(total.hogares.bloqueados[nivel.bloqueo == 'bloqueoCTO' & `Motivo bloqueo` == "Saturacion", ]$semanas.bloqueado)

## tiempos de bloqueo x Blacklist

total.hogares.bloqueados[, .N, by ="nivel.bloqueo"]
total.hogares.bloqueados <- merge(total.hogares.bloqueados, blacklist[, c("ID_DOMICILIO", "fecha_bloqueo")], all.x = T, by.x = "ID_DOMICILIO TO", by.y = "ID_DOMICILIO")
setnames(total.hogares.bloqueados, "fecha_bloqueo", "Fecha.bloqueo.Blacklist")
total.hogares.bloqueados[nivel.bloqueo == 'Blacklist', semanas.bloqueado := as.integer((difftime(Sys.Date() , Fecha.bloqueo.Blacklist, units = "weeks")))]

write.table(total.hogares.bloqueados,
            file = reportfile.totalhogaresbloqueados, sep = ";", col.names = T, fileEncoding = 'UTF-8', quote = F, row.names = F, na = "")



#### UNEs

length(unique(CTOs.block$CTO_ID))
CTOs.block[duplicated(CTOs.block$CTO_ID), ]

