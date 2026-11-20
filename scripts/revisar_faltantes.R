# Faltantes de Netquest contra la base completa ---------------------------------
#
# Cruza los tickets de scripts/faltantes.xlsx con toda la tabla de respuestas en
# Supabase (sin filtro de fecha) y, para cada uno, indica si tenemos algún
# registro y hasta dónde avanzó en la encuesta.
#
#   Match: primero por `ticket`; si el ticket no aparece, por `id_user` contra
#   el código de panelista (sin distinguir mayúsculas).
#   Avance: posición de la última pregunta respondida dentro del flujo de su
#   forma (BLOQUES_FLUJO y ORDEN_FORMA en R/config.R), como % del total de
#   preguntas del flujo. Si el ticket tiene varias sesiones, se usa la de mayor
#   avance.
#
# Deja en memoria `faltantes_revision` (un ticket por fila) y lo guarda en
# output/faltantes_revision.xlsx.
#
# Uso, desde la carpeta del proyecto (abrir monitor-jusmer.Rproj):
#   source("scripts/revisar_faltantes.R")

ARCHIVO <- "scripts/faltantes.xlsx"
SALIDA  <- "output/faltantes_revision.xlsx"

source("R/config.R")
source("R/datos.R")
source("R/procesar.R")
if (file.exists(".env")) readRenviron(".env")

# Base completa, salvo ip_address
local({
  tabla <- Sys.getenv("SD_TABLE", "jusmer_ola1_responses")
  con <- conectar_base()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  cols <- setdiff(DBI::dbListFields(con, tabla), "ip_address")
  consulta <- sprintf("SELECT %s FROM %s",
                      paste(DBI::dbQuoteIdentifier(con, cols), collapse = ", "),
                      DBI::dbQuoteIdentifier(con, tabla))
  .base_falt <<- DBI::dbGetQuery(con, consulta)
})
d <- preparar(.base_falt)
rm(.base_falt)

falt <- as.data.frame(readxl::read_xlsx(ARCHIVO))
names(falt) <- tolower(names(falt))

# Avance de cada sesión: última pregunta respondida dentro del flujo de su forma
avance_sesion <- vapply(seq_len(nrow(d)), function(i) {
  if (!isTRUE(d$acepta[i])) return(0)
  orden <- ORDEN_FORMA[[d$version_encuesta[i]]]
  if (is.null(orden)) orden <- ORDEN_FORMA$A
  vars <- intersect(unlist(BLOQUES_FLUJO[orden], use.names = FALSE), names(d))
  hechas <- which(vapply(vars, function(v) respondida(d[[v]][i]), TRUE))
  if (length(hechas)) max(hechas) / length(vars) else 0
}, 0)
d$avance <- ifelse(d$completa, 1, avance_sesion)

hora_chile <- function(x) format(x, "%Y-%m-%d %H:%M", tz = ZONA_HORARIA)

filas <- lapply(seq_len(nrow(falt)), function(k) {
  t <- falt$ticket[k]
  cod <- falt$codpanelista[k]
  s <- which(d$ticket == t)
  match <- if (length(s)) "ticket" else {
    s <- which(tolower(d$id_user) == tolower(cod))
    if (length(s)) "id_user" else "sin registro"
  }
  if (!length(s)) {
    return(data.frame(ticket = t, codpanelista = cod, match = match, sesiones = 0L,
                      consentimiento = NA, completa = NA, forma = NA, inicio_chile = NA,
                      ultimo_bloque = NA, current_page = NA, avance = NA,
                      ticket_en_base = NA, stringsAsFactors = FALSE))
  }
  i <- s[which.max(d$avance[s])]
  data.frame(
    ticket = t, codpanelista = cod, match = match, sesiones = length(s),
    consentimiento = d$consent_understand[i], completa = any(d$completa[s]),
    forma = d$version_encuesta[i], inicio_chile = hora_chile(d$inicio[i]),
    ultimo_bloque = d$ultimo_bloque[i], current_page = d$current_page[i],
    avance = round(d$avance[i], 3),
    ticket_en_base = d$ticket[i],
    stringsAsFactors = FALSE
  )
})
faltantes_revision <- do.call(rbind, filas)
faltantes_revision <- faltantes_revision[order(faltantes_revision$match, -faltantes_revision$avance), ]
rownames(faltantes_revision) <- NULL

dir.create(dirname(SALIDA), showWarnings = FALSE)
writexl::write_xlsx(faltantes_revision, SALIDA)

# Resumen ------------------------------------------------------------------------

n <- nrow(faltantes_revision)
con_reg <- faltantes_revision[faltantes_revision$match != "sin registro", ]
cat("\n== Faltantes de Netquest: ", n, " tickets ==\n", sep = "")
print(table(match = faltantes_revision$match))
cat(sprintf("\nCon algún registro: %d (%.1f%%). Sin ningún registro: %d (%.1f%%).\n",
            nrow(con_reg), 100 * nrow(con_reg) / n, n - nrow(con_reg), 100 * (n - nrow(con_reg)) / n))
if (nrow(con_reg)) {
  cat("\nDe los que tienen registro:\n")
  cat(sprintf("  aceptaron el consentimiento: %d\n", sum(con_reg$consentimiento %in% "yes")))
  cat(sprintf("  completas según nuestra definición: %d\n", sum(con_reg$completa)))
  cat(sprintf("  avance mediano: %.0f%%; promedio: %.0f%%\n",
              100 * stats::median(con_reg$avance), 100 * mean(con_reg$avance)))
  cat("\nAvance (% del flujo de preguntas):\n")
  print(table(cut(con_reg$avance, c(-Inf, 0, 0.25, 0.5, 0.75, 0.999, Inf),
                  labels = c("0%", "1-25%", "26-50%", "51-75%", "76-99%", "100%"))))
  cat("\nÚltimo bloque con respuestas:\n")
  print(table(con_reg$ultimo_bloque))
}
message("\nDetalle en `faltantes_revision` y en ", SALIDA)
