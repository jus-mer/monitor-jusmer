# Patrones de respuesta en las escalas Likert -----------------------------------
#
# Busca respuestas descuidadas en las escalas Likert de la encuesta (en línea
# recta, en zigzag, en diagonal o cualquier secuencia que se repite) con el
# paquete responsePatterns:
#
#   rp.acors()     auto-correlaciones de las respuestas de cada persona a lo
#                  largo de los ítems. Una auto-correlación alta en algún
#                  desfase (lag) indica que las respuestas se repiten cada
#                  cierto número de ítems (lag 1 = línea recta).
#   rp.patterns()  busca secuencias que se repiten (p. ej. 1-2-3-1-2-3), de
#                  largo 2 hasta la mitad de los ítems.
#
# Se hacen dos análisis:
#   escalas       todos los ítems Likert de 1 a 5 (acuerdo, justicia y
#                 frecuencia) en el orden en que se presentan. Como las formas
#                 A y B cambian el orden de los módulos, se analiza cada forma
#                 por separado (percentiles dentro de cada forma).
#   por batería   cada módulo de MODULOS_ESCALAS con al menos MIN_ITEMS ítems
#                 (hoy solo Merecimiento). Las baterías cortas no se analizan
#                 solas: con 8 ítems las auto-correlaciones de desfases altos
#                 se calculan con 3 pares y salen ±1 casi siempre.
#
# Los percentiles son relativos a la muestra: siempre habrá alrededor de un
# (100 - PERCENTIL)% de casos arriba del corte. Sirven para priorizar casos a
# revisar (junto con la duración y los chequeos de atención), no para excluir
# en forma automática.
#
# Uso, desde la carpeta del proyecto (abrir monitor-jusmer.Rproj):
#   source("scripts/patrones_respuesta.R")
#   View(patrones)                            # un caso por fila, los más sospechosos arriba
#   ver_caso("<session_id>")                  # respuestas de un caso en todas las escalas
#   ver_caso("<session_id>", "merecimiento")  # solo en una batería
#   rp.hist(rp_resultados$escalas_A$acors)    # distribución del índice
#
# Lee la base directamente desde Supabase con las mismas credenciales SD_* del
# .env que la app (no usa el objeto `respuestas` de descargar_base.R).

# Opciones ----------------------------------------------------------------------

DESDE          <- as.POSIXct("2026-10-29 00:00:00", tz = "America/Santiago")  # NULL = toda la base
SOLO_COMPLETAS <- TRUE   # FALSE: incluye incompletas (deben cumplir MIN_VALIDAS igual)
MIN_RESP_ITEM  <- 0.5    # se omiten ítems que respondió menos de esta proporción de casos
MIN_VALIDAS    <- 0.8    # proporción mínima de ítems con respuesta 1-5 (sin NS/NR) por caso
MIN_ITEMS      <- 12     # largo mínimo de una batería para analizarla por separado
MAX_LAG        <- 10     # desfase máximo de las auto-correlaciones (el paquete usa n ítems - 3)
PERCENTIL      <- 95     # desde qué percentil un caso se marca como sospechoso
GUARDAR        <- TRUE  # TRUE: guarda CSV y PDF con los gráficos de los casos marcados
CARPETA        <- "scripts/patrones"

# Datos -------------------------------------------------------------------------

for (pkg in c("responsePatterns", "DBI", "RPostgres")) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
}
suppressPackageStartupMessages(library(responsePatterns))

source("R/config.R")
source("R/datos.R")
source("R/procesar.R")
source("R/patrones.R")
if (file.exists(".env")) readRenviron(".env")

d <- preparar(leer_base())
d <- d[d$acepta, , drop = FALSE]
if (SOLO_COMPLETAS) d <- d[d$completa, , drop = FALSE]
if (!is.null(DESDE)) d <- d[!is.na(d$inicio) & d$inicio >= DESDE, , drop = FALSE]
rownames(d) <- NULL
message("Casos a analizar: ", nrow(d))

# Análisis -----------------------------------------------------------------------
#
# El núcleo (ítems, orden por forma, rp.acors() y rp.patterns()) está en
# R/patrones.R, que también usa la app en la pestaña Calidad.

opciones <- list(min_resp_item = MIN_RESP_ITEM, min_validas = MIN_VALIDAS, min_items = MIN_ITEMS,
                 max_lag = MAX_LAG, percentil = PERCENTIL)
message("Analizando escalas (puede tardar unos segundos)...")
pr <- patrones_respuesta(d, opciones)
if (length(pr$omitidos)) message("Ítems omitidos por tener pocas respuestas: ", paste(pr$omitidos, collapse = ", "))
patrones <- pr$patrones
rp_resultados <- pr$resultados

# Resumen -----------------------------------------------------------------------

resumen <- do.call(rbind, lapply(names(rp_resultados), function(b) {
  r <- rp_resultados[[b]]
  data.frame(
    analisis       = b,
    items          = length(r$items),
    analizados     = nrow(r$indices),
    excluidos_nsnr = r$n_excluidos,
    ac_max_mediana = stats::median(r$indices$ac_max, na.rm = TRUE),
    linea_recta    = sum(r$indices$pct_modal == 1),
    casi_recta     = sum(r$indices$pct_modal >= 0.9 & r$indices$pct_modal < 1),
    sospechosos    = sum(r$indices$sospechosa)
  )
}))

# ¿Los casos marcados fallan más los otros chequeos de calidad?
grupo <- ifelse(patrones$n_sospechas > 0, "Marcado", "Sin marca")
comparacion <- data.frame(
  grupo          = c("Marcado", "Sin marca"),
  n              = as.integer(table(factor(grupo, c("Marcado", "Sin marca")))),
  falla_atencion = round(tapply(patrones$falla_atencion, factor(grupo, c("Marcado", "Sin marca")), mean), 3),
  falla_manip    = round(tapply(patrones$falla_manip, factor(grupo, c("Marcado", "Sin marca")), mean), 3),
  rapido         = round(tapply(patrones$rapido, factor(grupo, c("Marcado", "Sin marca")), mean), 3)
)

cat("\n== Patrones de respuesta (corte: percentil ", PERCENTIL, ") ==\n", sep = "")
print(resumen, row.names = FALSE)
cat("\n== Casos según en cuántos análisis quedan marcados ==\n")
print(table(n_sospechas = patrones$n_sospechas))
cat("\n== Otros indicadores de calidad (proporción de casos) ==\n")
print(comparacion, row.names = FALSE)
cat("\n== Casos más sospechosos ==\n")
print(utils::head(patrones[patrones$n_sospechas > 0,
  c("session_id", "version_encuesta", "duracion_min", "falla_atencion", "rapido",
    grep("_(pct_modal|ac_max|pat_score)$", names(patrones), value = TRUE), "n_sospechas")], 20),
  row.names = FALSE)

# Gráficos -----------------------------------------------------------------------

# rp.plot() 0.1.1 falla ("objeto 'max.lag' no encontrado") cuando los desfases
# calculados no superan plot.lags, y rp.plots2pdf() lo usa por dentro. Se le
# pasa una copia con un desfase máximo nominal mayor para que entre por la rama
# que funciona; igual muestra todos los desfases calculados.
graficar <- function(r, ...) {
  acors <- r$acors
  lags <- acors@options$max.lag - acors@options$min.lag + 1
  acors@options$max.lag <- acors@options$max.lag + 1
  rp.plot(acors, groups = r$grupos, page.breaks = r$cortes, plot.lags = lags, ...)
}

# Gráfico de un caso: respuestas ítem a ítem (coloreadas por módulo o
# subescala, con una línea entre módulos) y, debajo, sus auto-correlaciones
# por desfase. texto = TRUE además imprime las respuestas en la consola.
ver_caso <- function(session_id, analisis = "escalas", texto = FALSE) {
  candidatos <- rp_resultados[grepl(paste0("^", analisis, "(_[A-Z])?$"), names(rp_resultados))]
  if (!length(candidatos)) stop("Análisis no disponible. Opciones: escalas, ",
                                paste(setdiff(names(rp_resultados), grep("^escalas_", names(rp_resultados), value = TRUE)), collapse = ", "))
  esta <- vapply(candidatos, function(r) session_id %in% r$indices$session_id, TRUE)
  if (!any(esta)) stop("El caso no está en el análisis (incompleto, otra fecha o muchos NS/NR).")
  graficar(candidatos[[which(esta)[1]]], id = session_id, text.output = texto)
}

# Archivos ----------------------------------------------------------------------

if (GUARDAR) {
  dir.create(CARPETA, showWarnings = FALSE, recursive = TRUE)
  utils::write.csv(patrones, file.path(CARPETA, "patrones_respuesta.csv"), row.names = FALSE, na = "")
  # Un PDF por análisis con los casos marcados, una página por caso
  for (b in names(rp_resultados)) {
    r <- rp_resultados[[b]]
    ids <- r$indices$session_id[r$indices$sospechosa]
    if (!length(ids)) next
    grDevices::pdf(file.path(CARPETA, paste0("casos_", b, ".pdf")))
    for (id in ids) graficar(r, id = id)
    grDevices::dev.off()
  }
  message("Resultados guardados en ", CARPETA)
}

message("\nListo: `patrones` (un caso por fila) y `rp_resultados` (objetos de responsePatterns).")
