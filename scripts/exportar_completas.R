# Exporta todas las respuestas que el monitor cuenta como completas (misma
# definición que preparar(): consentimiento + id-final y, desde
# INICIO_FILTRO_CALIDAD, filtro_calidad == "ok"), con ticket, id_user y las
# fechas de entrada y de término en hora de Chile.
#
# Uso (desde la carpeta monitor-jusmer): Rscript scripts/exportar_completas.R

for (f in c("R/config.R", "R/preguntas.R", "R/datos.R", "R/procesar.R")) source(f)
readRenviron(".env")

con <- conectar_base()
crudo <- DBI::dbGetQuery(con, sprintf("SELECT * FROM %s",
                                      DBI::dbQuoteIdentifier(con, Sys.getenv("SD_TABLE", "jusmer_ola1_responses"))))
DBI::dbDisconnect(con)
crudo$ip_address <- NULL
crudo[] <- lapply(crudo, function(x) { x <- as.character(x); x[is.na(x)] <- ""; x })

d <- preparar(crudo)
d <- d[d$completa, ]

# Solo el campo (desde el 30/10): antes son pruebas internas. Dos casos del
# campo no tienen time_start; se usan por su fecha de término.
DESDE <- as.POSIXct("2026-10-30 00:00:00", tz = ZONA_HORARIA)
d <- d[ifelse(is.na(d$inicio), !is.na(d$fin) & d$fin >= DESDE, d$inicio >= DESDE), ]
d <- d[order(d$inicio, d$fin), ]

completas <- data.frame(
  ticket = d$ticket,
  id_user = d$id_user,
  fecha_entrada = format(d$inicio, "%Y-%m-%d %H:%M:%S", tz = ZONA_HORARIA),
  fecha_completado = format(d$fin, "%Y-%m-%d %H:%M:%S", tz = ZONA_HORARIA),
  # ya viene en el último archivo de educación de Netquest (config/educacion_netquest.csv)
  educ_netquest_recibida = ifelse(is.na(d$educ_netquest), "no", "sí")
)

dir.create("output", showWarnings = FALSE)
archivo <- sprintf("output/completas_monitor_%s.xlsx", format(Sys.Date(), "%Y%m%d"))
writexl::write_xlsx(completas, archivo)
cat("Completas:", nrow(completas), "->", archivo, "\n")
cat("Sin ticket:", sum(!nzchar(completas$ticket)), " Sin id_user:", sum(!nzchar(completas$id_user)),
    " Sin fecha de término:", sum(is.na(d$fin)), "\n")
cat("Sin fecha de entrada:", sum(is.na(d$inicio)), "\n")
cat("Entrada desde", min(completas$fecha_entrada, na.rm = TRUE), "hasta", max(completas$fecha_entrada, na.rm = TRUE), "\n")
