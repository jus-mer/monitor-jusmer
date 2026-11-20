# Participaciones de Netquest contra nuestra base -------------------------------
#
# Cruza por `ticket` el reporte de participaciones de Netquest (ARCHIVO, hoja
# "particip") con las sesiones de nuestra base iniciadas el día DIA (hora de
# Chile), y compara el estado que asigna Netquest con el de la encuesta:
# página final (current_page), filtro de calidad guardado (filtro_calidad),
# completa según el monitor y avance en el flujo de preguntas.
#
# Estados de Netquest (hoja "Codifos"):
#   CO_0 completa · FO_0 filtrada (consentimiento) · DR_0 calidad
#   AA_0 ya tenía un acceso · FR_1000 ISO check sexo-edad · FR_6000 recaptcha
#   QC_0 accede con el campo cerrado · vacío: abandona la encuesta
#
# Deja en memoria `participaciones` (un ticket de Netquest por fila),
# `solo_base` (sesiones del día que no están en el reporte) y guarda ambos en
# output/participaciones_<DIA>.xlsx.
#
# Uso, desde la carpeta del proyecto (abrir monitor-jusmer.Rproj):
#   source("scripts/revisar_participaciones.R")

ARCHIVO <- "scripts/participaciones 12-11.xlsx"
DIA     <- as.Date("2026-11-12")
SALIDA  <- sprintf("output/participaciones_%s.xlsx", format(DIA, "%d-%m"))

source("R/config.R")
source("R/datos.R")
source("R/procesar.R")
if (file.exists(".env")) readRenviron(".env")

local({
  tabla <- Sys.getenv("SD_TABLE", "jusmer_ola1_responses")
  con <- conectar_base()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  cols <- setdiff(DBI::dbListFields(con, tabla), "ip_address")
  consulta <- sprintf("SELECT %s FROM %s",
                      paste(DBI::dbQuoteIdentifier(con, cols), collapse = ", "),
                      DBI::dbQuoteIdentifier(con, tabla))
  .base_part <<- DBI::dbGetQuery(con, consulta)
})
base <- preparar(.base_part)
rm(.base_part)
if (!"filtro_calidad" %in% names(base)) base$filtro_calidad <- ""

nq <- as.data.frame(readxl::read_xlsx(ARCHIVO, sheet = "particip"))
names(nq) <- c("ticket", "cod_panelista", "acceso_nq", "estado_nq")
nq$estado_nq[is.na(nq$estado_nq)] <- "(vacío)"
ETIQUETAS_NQ <- c(CO_0 = "Completa", FO_0 = "Filtrada", DR_0 = "Calidad", AA_0 = "Ya tenía acceso",
                  FR_1000 = "ISO check sexo-edad", FR_6000 = "Recaptcha", QC_0 = "Campo cerrado",
                  "(vacío)" = "Abandona")

# Avance: última pregunta respondida dentro del flujo de su forma (1 si completa)
avance <- function(d) {
  a <- vapply(seq_len(nrow(d)), function(i) {
    if (!isTRUE(d$acepta[i])) return(0)
    orden <- ORDEN_FORMA[[d$version_encuesta[i]]]
    if (is.null(orden)) orden <- ORDEN_FORMA$A
    vars <- intersect(unlist(BLOQUES_FLUJO[orden], use.names = FALSE), names(d))
    hechas <- which(vapply(vars, function(v) respondida(d[[v]][i]), TRUE))
    if (length(hechas)) max(hechas) / length(vars) else 0
  }, 0)
  round(ifelse(d$completa, 1, a), 3)
}
base$avance <- avance(base)

hora_chile <- function(x) format(x, "%Y-%m-%d %H:%M", tz = ZONA_HORARIA)
dia_base <- base[!is.na(base$fecha) & base$fecha == DIA, , drop = FALSE]

# Para cada ticket de Netquest, su sesión más avanzada en toda la base
cols_base <- c("session_id", "version_encuesta", "inicio", "consent_understand", "completa",
               "current_page", "filtro_calidad", "ultimo_bloque", "avance", "duracion_min")
sesion <- lapply(nq$ticket, function(t) {
  s <- which(base$ticket == t)
  if (!length(s)) return(NULL)
  i <- s[which.max(base$avance[s] + base$completa[s])]
  cbind(base[i, cols_base, drop = FALSE], sesiones = length(s))
})
vacia <- cbind(base[0, cols_base, drop = FALSE], sesiones = integer(0))[NA_integer_, ]
participaciones <- cbind(nq, do.call(rbind, lapply(sesion, function(s) if (is.null(s)) vacia else s)))
participaciones$estado_nq_etiqueta <- unname(ETIQUETAS_NQ[participaciones$estado_nq])
participaciones$en_base <- !is.na(participaciones$session_id)
participaciones$sesiones[is.na(participaciones$sesiones)] <- 0L
participaciones$inicio <- hora_chile(participaciones$inicio)
participaciones$duracion_min <- round(participaciones$duracion_min, 1)
participaciones$acceso_nq <- format(participaciones$acceso_nq, "%Y-%m-%d %H:%M", tz = "UTC")
participaciones <- participaciones[, c("ticket", "cod_panelista", "estado_nq", "estado_nq_etiqueta",
                                       "acceso_nq", "en_base", "sesiones", setdiff(cols_base, character(0)))]
names(participaciones)[names(participaciones) == "inicio"] <- "inicio_chile"
rownames(participaciones) <- NULL

solo_base <- dia_base[!dia_base$ticket %in% nq$ticket,
                      c("ticket", "id_user", cols_base), drop = FALSE]
solo_base$inicio <- hora_chile(solo_base$inicio)
rownames(solo_base) <- NULL

dir.create(dirname(SALIDA), showWarnings = FALSE)
writexl::write_xlsx(list(participaciones = participaciones, solo_en_base = solo_base), SALIDA)

# Resumen ------------------------------------------------------------------------

p <- participaciones
cat(sprintf("\n== Netquest %s: %d participaciones; nuestra base: %d sesiones iniciadas ese día ==\n",
            format(DIA, "%d/%m"), nrow(p), nrow(dia_base)))
cat(sprintf("Tickets de Netquest en nuestra base: %d; sin registro: %d. Sesiones del día que no están en el reporte: %d\n",
            sum(p$en_base), sum(!p$en_base), nrow(solo_base)))

cat("\nEstado Netquest × página final en nuestra base:\n")
pagina <- ifelse(p$en_base, p$current_page, "(sin registro)")
pagina[p$en_base & !nzchar(pagina)] <- "(vacía)"
print(addmargins(table(estado_nq = paste(p$estado_nq, p$estado_nq_etiqueta), pagina = pagina)))

cat("\nEstado Netquest × filtro_calidad guardado:\n")
filtro <- ifelse(p$en_base, ifelse(nzchar(p$filtro_calidad), p$filtro_calidad, "(vacío)"), "(sin registro)")
print(table(estado_nq = p$estado_nq, filtro_calidad = filtro))

cat("\nEstado Netquest × completa según el monitor:\n")
print(table(estado_nq = p$estado_nq, completa = ifelse(p$en_base, p$completa, NA), useNA = "ifany"))

ab <- p[p$estado_nq == "(vacío)" & p$en_base, ]
if (nrow(ab)) {
  cat("\nAbandonos (sin estado en Netquest): avance en el flujo\n")
  print(table(cut(ab$avance, c(-Inf, 0, 0.25, 0.5, 0.75, 0.999, Inf),
                  labels = c("0%", "1-25%", "26-50%", "51-75%", "76-99%", "100%"))))
  cat("Último bloque con respuestas:\n")
  print(table(ab$ultimo_bloque))
}
if (nrow(solo_base)) {
  cat("\nSesiones del día que no están en el reporte de Netquest, por página final:\n")
  print(table(solo_base$current_page, useNA = "ifany"))
}
message("\nDetalle en `participaciones`, `solo_base` y en ", SALIDA)
