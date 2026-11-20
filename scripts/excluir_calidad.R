# Casos a excluir por el filtro de calidad -------------------------------------
#
# Aplica a las encuestas completas las mismas reglas del filtro de calidad de la
# encuesta (falla_calidad() en survey-jusmer/surveys/ola1/app.R, redirect
# "Calidad" de Netquest, tp=dr_0):
#
#   manip_tareas    falla el chequeo de manipulación (manip_check distinto de
#                   "reparto") y marca "Prefiero no responder" en 3 o más de
#                   las 6 tareas del conjoint (cbc_q1_nr..cbc_q6_nr)
#   manip_atencion  falla el chequeo de manipulación y el de atención
#                   (responde attention_check, que debía quedar en blanco)
#   tiempo          menos de MIN_MINUTOS minutos entre el inicio y el fin
#
# Completa = misma definición que el monitor (aceptó el consentimiento y
# respondió salud_prov o educ_prov). Un caso puede tener varias razones.
#
# Deja en memoria `excluir` y lo guarda en output/excluir_calidad.xlsx.
#
# Uso, desde la carpeta del proyecto (abrir monitor-jusmer.Rproj):
#   source("scripts/excluir_calidad.R")

DESDE      <- as.POSIXct("2026-10-29 00:00:00", tz = "America/Santiago")  # inicio del campo
MIN_MINUTOS <- 5
SALIDA     <- "output/excluir_calidad.xlsx"

source("R/config.R")
source("R/datos.R")
source("R/procesar.R")
source("R/patrones.R")
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
  .base_cal <<- DBI::dbGetQuery(con, consulta)
})
d <- preparar(.base_cal)
rm(.base_cal)
d <- d[d$completa & !is.na(d$inicio) & d$inicio >= DESDE, , drop = FALSE]

# Reglas -------------------------------------------------------------------------

col_nr <- paste0("cbc_q", 1:6, "_nr")
falta <- setdiff(c(col_nr, "manip_check", "attention_check"), names(d))
if (length(falta)) stop("Faltan columnas en la base: ", paste(falta, collapse = ", "))

falla_manip <- d$manip_check != "reparto"
tareas_nr <- rowSums(vapply(col_nr, function(v) grepl("99", d[[v]], fixed = TRUE), logical(nrow(d))))
falla_atencion <- respondida(d$attention_check)

motivos <- cbind(
  manip_tareas   = falla_manip & tareas_nr >= 3,
  manip_atencion = falla_manip & falla_atencion,
  tiempo         = !is.na(d$duracion_min) & d$duracion_min < MIN_MINUTOS
)
etiquetas <- c(
  manip_tareas   = "No pasa el chequeo de manipulación y no responde 3 o más tareas",
  manip_atencion = "No pasa los chequeos de manipulación y atención",
  tiempo         = sprintf("Menos de %d minutos en responder la encuesta completa", MIN_MINUTOS)
)

# Respuestas descuidadas (careless) ----------------------------------------------
#
# Mismo análisis de patrones de la pestaña Calidad del monitor (R/patrones.R,
# paquete responsePatterns) sobre estas mismas completas, en todas las escalas
# y en la batería de merecimiento. Categorías (un caso puede tener varias):
#   Línea recta         todas las respuestas iguales en algún análisis
#   Casi línea recta    90 % o más de respuestas iguales (sin llegar al 100 %)
#   Autocorrelación     auto-correlación desde el percentil 95
#   Secuencia repetida  secuencia repetida desde el percentil 95
#   Muchos NS/NR        más del 25 % de los ítems de escalas con No sé / NR
# Los percentiles son relativos a la muestra: cerca de un 5 % queda marcado
# por construcción. Vacío si no tiene ninguna marca.

pr <- patrones_respuesta(d)$patrones
pr <- pr[match(d$session_id, pr$session_id), ]
col_de <- function(sufijo) grep(paste0("_", sufijo, "$"), names(pr), value = TRUE)
alguna <- function(cols, f) {
  if (!length(cols)) return(rep(FALSE, nrow(pr)))
  Reduce(`|`, lapply(cols, function(v) { x <- f(pr[[v]]); !is.na(x) & x }))
}
items_escalas <- unlist(lapply(MODULOS_ESCALAS, function(m) names(m$items)), use.names = FALSE)
m_items <- as.matrix(d[, items_escalas, drop = FALSE])
nsnr <- rowMeans(m_items == COD_NS | m_items == COD_NR)

marcas_careless <- cbind(
  "Línea recta"        = alguna(col_de("pct_modal"), function(x) x == 1),
  "Casi línea recta"   = alguna(col_de("pct_modal"), function(x) x >= 0.9 & x < 1),
  "Autocorrelación"    = alguna(col_de("marca_ac"), identity),
  "Secuencia repetida" = alguna(col_de("marca_pat"), identity),
  "Muchos NS/NR"       = nsnr > 0.25
)
marcas_careless[, "Casi línea recta"] <- marcas_careless[, "Casi línea recta"] & !marcas_careless[, "Línea recta"]
careless <- apply(marcas_careless, 1, function(m) paste(colnames(marcas_careless)[m], collapse = "; "))

excluir_fila <- rowSums(motivos) > 0
razon <- apply(motivos[excluir_fila, , drop = FALSE], 1, function(m) paste(etiquetas[m], collapse = "; "))

hora_chile <- function(x) format(x, "%Y-%m-%d %H:%M", tz = ZONA_HORARIA)
e <- d[excluir_fila, , drop = FALSE]
excluir <- data.frame(
  ticket           = e$ticket,
  id_user          = e$id_user,
  session_id       = e$session_id,
  razon_exclusion  = unname(razon),
  careless         = careless[excluir_fila],
  version_encuesta = e$version_encuesta,
  inicio_chile     = hora_chile(e$inicio),
  duracion_min     = round(e$duracion_min, 1),
  manip_check      = e$manip_check,
  tareas_nr        = tareas_nr[excluir_fila],
  attention_check  = e$attention_check,
  current_page     = e$current_page,
  stringsAsFactors = FALSE
)
excluir <- excluir[order(excluir$inicio_chile), ]
rownames(excluir) <- NULL

dir.create(dirname(SALIDA), showWarnings = FALSE)
writexl::write_xlsx(excluir, SALIDA)

# Resumen ------------------------------------------------------------------------

n <- nrow(d)
cat(sprintf("\n== Filtro de calidad sobre %d encuestas completas (desde %s) ==\n", n, format(DESDE, "%d/%m")))
for (k in colnames(motivos)) {
  cat(sprintf("  %-70s %4d (%.1f%%)\n", etiquetas[[k]], sum(motivos[, k]), 100 * mean(motivos[, k])))
}
cat(sprintf("  %-70s %4d (%.1f%%)\n", "A excluir (al menos una razón)", nrow(excluir), 100 * nrow(excluir) / n))
cat(sprintf("  %-70s %4d\n", "  con más de una razón", sum(rowSums(motivos) > 1)))
cat("\n== Careless (responsePatterns) sobre las mismas completas ==\n")
for (k in colnames(marcas_careless)) {
  cat(sprintf("  %-25s %4d (%.1f%%), %d entre los excluidos\n", k, sum(marcas_careless[, k]),
              100 * mean(marcas_careless[, k]), sum(marcas_careless[excluir_fila, k])))
}
con_marca <- nzchar(careless)
cat(sprintf("  %-25s %4d (%.1f%%), %d entre los excluidos y %d fuera de la lista\n", "Alguna marca",
            sum(con_marca), 100 * mean(con_marca), sum(con_marca & excluir_fila), sum(con_marca & !excluir_fila)))
message("\nDetalle en `excluir` y en ", SALIDA)
