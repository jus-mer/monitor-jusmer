# Descarga de la base de respuestas ---------------------------------------------
#
# Lee la tabla de respuestas desde Supabase y la deja en memoria como el objeto
# `respuestas`, para revisarla en la misma sesión de R. No escribe archivos.
# Usa las mismas credenciales SD_* del .env que la app.
#
# Uso, desde la carpeta del proyecto (abrir monitor-jusmer.Rproj):
#   source("scripts/descargar_base.R")
#   View(respuestas)
# Para traer las respuestas nuevas, volver a correr el source().
#
# El script está fuera de R/ a propósito: Shiny carga todo lo que hay en R/ al
# iniciar la app. surveydown guarda todas las columnas como texto.

# Columnas que no se descargan. Para traer la tabla completa: character(0)
EXCLUIR <- c("ip_address")

source("R/datos.R")
if (file.exists(".env")) readRenviron(".env")

tabla <- Sys.getenv("SD_TABLE", "jusmer_ola1_responses")
con <- conectar_base()
respuestas <- tryCatch({
  cols <- setdiff(DBI::dbListFields(con, tabla), EXCLUIR)
  consulta <- sprintf(
    "SELECT %s FROM %s",
    paste(DBI::dbQuoteIdentifier(con, cols), collapse = ", "),
    DBI::dbQuoteIdentifier(con, tabla)
  )
  DBI::dbGetQuery(con, consulta)
}, finally = DBI::dbDisconnect(con))

if ("time_start" %in% names(respuestas)) {
  respuestas <- respuestas[order(respuestas$time_start), , drop = FALSE]
  rownames(respuestas) <- NULL
}

message("Cargadas ", nrow(respuestas), " filas y ", ncol(respuestas), " columnas de ", tabla, " en `respuestas`.")

##############################

pacman::p_load(
  dplyr, tidyr, stringr, lubridate, forcats, readr, purrr, tibble
)

dropout <- readxl::read_xlsx("scripts/dropouts.xlsx")
completes <- readxl::read_xlsx("scripts/completes.xlsx")
iniciadas <- respuestas %>% filter(time_start >= "2026-10-29 00:00:00")

completes_ticket <- iniciadas %>% select(ticket, id_user)
test_completas <- merge(completes, iniciadas, by = "ticket")



iniciadas_ticket <- iniciadas %>% select(ticket, id_user)
test_fin <- merge(iniciadas_ticket, dropout, by = "ticket")

# Dropouts de Netquest sin registro en nuestra base (ticket sin match). Se
# busca en toda la base, no solo desde el 29/10, para no dejar fuera un ticket
# que sí tenga alguna sesión.
dropouts_sin_registro <- dropout %>%
  filter(!ticket %in% respuestas$ticket) %>%
  mutate(id_user_en_base = CodPanelista %in% respuestas$id_user |
           tolower(CodPanelista) %in% tolower(respuestas$id_user))

dir.create("output", showWarnings = FALSE)
writexl::write_xlsx(dropouts_sin_registro, "output/dropouts_netquest_sin_registro.xlsx")
message("Dropouts de Netquest: ", nrow(dropout), ". Con match por ticket: ",
        sum(dropout$ticket %in% respuestas$ticket), ". Sin registro: ",
        nrow(dropouts_sin_registro), " (", sum(dropouts_sin_registro$id_user_en_base),
        " de ellos con su id_user en la base), guardados en output/dropouts_netquest_sin_registro.xlsx")

# Todas nuestras completas (ticket e id_user), para que Netquest haga el
# match. Completa = misma definición que el monitor: aceptó el consentimiento
# y respondió salud_prov o educ_prov. Solo sesiones desde el inicio del campo.
respondida <- function(x) !is.na(x) & nzchar(x)
completas_jusmer <- iniciadas %>%
  filter(consent_understand %in% "yes", respondida(salud_prov) | respondida(educ_prov)) %>%
  distinct(ticket, id_user)

writexl::write_xlsx(completas_jusmer, "output/completas_jusmer.xlsx")
message("Completas en nuestra base: ", nrow(completas_jusmer),
        ", guardadas en output/completas_jusmer.xlsx")

completas <- test %>% filter(!is.na(educ_prov))
completas2 <- test %>% filter(!is.na(time_end))
completas3 <- test %>% filter(current_page == "end_quotafull")
sjmisc::frq(test$current_page)

test <- respuestas %>% filter(!is.na(ticket))
test <- select(test, ticket, id_user)
