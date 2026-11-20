# Completas en Netquest que en nuestra base figuran incompletas ------------------
#
# Cruza por `ticket` las completas que reporta Netquest (scripts/completes.xlsx)
# con la tabla de respuestas en Supabase, y deja en memoria:
#
#   incompletas_nq      sesiones de tickets que Netquest da por completos pero
#                       que en nuestra base no tienen ninguna sesión completa.
#                       Primero van columnas de revisión y después todas las
#                       columnas de la base (sin ip_address).
#   no_encontradas_nq   completas de Netquest cuyo ticket no aparece en la base.
#
# "Completa" usa la misma definición que el monitor: aceptó el consentimiento
# y respondió salud_prov o educ_prov (VARS_FIN en R/config.R). Un ticket cuenta
# como completo si alguna de sus sesiones lo está.
#
# Uso, desde la carpeta del proyecto (abrir monitor-jusmer.Rproj):
#   source("scripts/incompletas_netquest.R")
#   View(incompletas_nq)

ARCHIVO_NQ <- "scripts/completes.xlsx"

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
  .base_nq <<- DBI::dbGetQuery(con, consulta)
})

nq <- as.data.frame(readxl::read_xlsx(ARCHIVO_NQ))
d <- preparar(.base_nq)

# Sesiones de tickets completos en Netquest y estado del ticket en nuestra base
en_nq <- d$ticket %in% nq$ticket
ticket_completo <- tapply(d$completa[en_nq], d$ticket[en_nq], any)
incompletos <- names(ticket_completo)[!ticket_completo]
filas <- which(d$ticket %in% incompletos)

hora_chile <- function(x) format(x, "%Y-%m-%d %H:%M:%S", tz = ZONA_HORARIA)
nq_fila <- match(d$ticket[filas], nq$ticket)

revision <- data.frame(
  ticket             = d$ticket[filas],
  id_user            = d$id_user[filas],
  cod_panelista_nq   = nq$CodPanelista[nq_fila],
  acceso_nq          = format(nq$access_date[nq_fila], "%Y-%m-%d %H:%M"),
  session_id         = d$session_id[filas],
  version_encuesta   = d$version_encuesta[filas],
  inicio_chile       = hora_chile(d$inicio[filas]),
  fin_chile          = hora_chile(d$fin[filas]),
  current_page       = d$current_page[filas],
  ultimo_bloque      = d$ultimo_bloque[filas],
  consentimiento     = d$consent_understand[filas],
  # Respondió el final aunque no cuente como completa (p. ej. sin consentimiento)
  respondio_final    = Reduce(`|`, lapply(VARS_FIN, function(v) respondida(d[[v]][filas]))),
  tiene_time_end     = respondida(d$time_end[filas]),
  sesiones_ticket    = as.integer(table(d$ticket[en_nq])[d$ticket[filas]]),
  stringsAsFactors   = FALSE
)
cruda <- .base_nq[filas, setdiff(names(.base_nq), names(revision)), drop = FALSE]
incompletas_nq <- cbind(revision, cruda)
incompletas_nq <- incompletas_nq[order(incompletas_nq$inicio_chile), ]
rownames(incompletas_nq) <- NULL

no_encontradas_nq <- nq[!nq$ticket %in% d$ticket, , drop = FALSE]
rownames(no_encontradas_nq) <- NULL

rm(.base_nq, nq, d, en_nq, ticket_completo, incompletos, filas, nq_fila, revision, cruda, hora_chile)

message(
  "Completas en Netquest: ", nrow(readxl::read_xlsx(ARCHIVO_NQ)), ". ",
  "En la base como incompletas: ", length(unique(incompletas_nq$ticket)), " tickets (",
  nrow(incompletas_nq), " sesiones) en `incompletas_nq`, de ellos ",
  sum(incompletas_nq$respondio_final), " respondieron el final. ",
  "No están en la base: ", nrow(no_encontradas_nq), " en `no_encontradas_nq`."
)
