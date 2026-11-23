# Reporte de salud del campo: Posit Connect Cloud + Supabase -------------------
#
# Revisa, para la última VENTANA_MIN minutos y para el día de hoy:
#   - Posit Cloud: créditos de la cuenta jus-mer, estado de la encuesta, si la
#     URL pública responde y errores/advertencias en el log de la app.
#   - Supabase: conexión y latencia, conexiones abiertas, sesiones nuevas,
#     completas, páginas de salida y la última sesión registrada.
#
# Uso (desde monitor-jusmer, en PowerShell; en Git Bash rsconnect puede fallar):
#   Rscript scripts/reporte_campo.R [minutos]

args <- commandArgs(trailingOnly = TRUE)
VENTANA_MIN <- if (length(args) > 0) as.numeric(args[1]) else 60

for (f in c("R/config.R", "R/preguntas.R", "R/datos.R", "R/procesar.R")) source(f)
readRenviron(".env")

TZ <- ZONA_HORARIA
CUENTA <- "jus-mer"
ENCUESTA <- "01a1136c-df1f-c125-dbbc-30faed92627b"   # Encuesta JUSMER - Ola 1
ahora <- Sys.time()
desde <- ahora - VENTANA_MIN * 60
hoy <- as.POSIXct(format(ahora, "%Y-%m-%d 00:00:00", tz = TZ), tz = TZ)
hm <- function(x) ifelse(is.na(x), "–", format(x, "%d/%m %H:%M", tz = TZ))
seg <- function(t0) round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 2)

cat(sprintf("REPORTE DE CAMPO · %s · ventana: últimos %d min (desde %s)\n\n",
            format(ahora, "%d/%m/%Y %H:%M", tz = TZ), VENTANA_MIN, format(desde, "%H:%M", tz = TZ)))

# Posit Connect Cloud ----------------------------------------------------------
cat("== POSIT CONNECT CLOUD ==\n")
try({
  ns <- asNamespace("rsconnect")
  GET <- get("GET", ns)
  cliente <- rsconnect:::clientForAccount(rsconnect::accountInfo(CUENTA))
  api <- function(p) cliente$withTokenRefreshRetry(GET, p)

  app <- api(paste0("/contents/", ENCUESTA))
  cuenta <- api(paste0("/accounts/", app$account_id))
  lic <- cuenta$license
  cat(sprintf("Cuenta: %s · plan %s · ciclo hasta %s\n", cuenta$state, lic$type, substr(lic$cycle_end_date, 1, 10)))
  cat(sprintf("Créditos: excedidos = %s · aviso de créditos = %s%s\n",
              isTRUE(lic$compute_credits_exceeded), isTRUE(lic$compute_credits_warned),
              if (!is.null(lic$compute_credits_exceeded_until)) paste0(" (hasta ", lic$compute_credits_exceeded_until, ")") else ""))
  rev <- app$current_revision
  cat(sprintf("Encuesta: estado %s · revisión %s (%s, publicada %s) · %s GB RAM, %s CPU\n",
              app$state, rev$status, rev$publish_result, substr(rev$created_time, 1, 10), rev$memory / 1024, rev$cpu))

  t0 <- Sys.time()
  resp <- tryCatch(curl::curl_fetch_memory(rev$url, handle = curl::new_handle(timeout = 30, followlocation = TRUE)),
                   error = function(e) e)
  if (inherits(resp, "error")) {
    cat("URL pública:", rev$url, "· NO RESPONDE:", conditionMessage(resp), "\n")
  } else {
    cat(sprintf("URL pública: %s · HTTP %d en %s s\n", rev$url, resp$status_code, seg(t0)))
  }

  # Log de la revisión activa, desde `desde`
  logs_api <- get("parseHttpUrl", ns)(paste0(get("connectCloudUrls", ns)()$logs, "/v1"))
  canal <- rev$publish_log_channel
  token <- cliente$getAuthorization(canal)
  partes <- list(); cursor <- NULL
  repeat {
    ruta <- paste0("/logs/", canal, "?traversal_direction=backward&limit=1500",
                   if (!is.null(cursor)) paste0("&sort_key__lt=", format(cursor, scientific = FALSE)))
    d <- GET(logs_api, list(accessToken = token), ruta)$data
    if (length(d) == 0) break
    p <- data.frame(
      sort_key = vapply(d, function(x) as.numeric(x$sort_key), 0),
      hora = as.POSIXct(vapply(d, function(x) as.numeric(x$timestamp), 0) / 1e6, origin = "1970-01-01", tz = TZ),
      nivel = vapply(d, function(x) x$level %||% "", ""),
      mensaje = vapply(d, function(x) x$message %||% "", "")
    )
    partes[[length(partes) + 1]] <- p
    cursor <- min(p$sort_key)
    if (min(p$hora) < desde) break
  }
  lg <- do.call(rbind, partes)
  lg <- if (is.null(lg)) data.frame(hora = as.POSIXct(character()), nivel = character(), mensaje = character()) else lg[lg$hora >= desde, ]
  problema <- grepl("error|warn|fail|exception|timeout|killed|out of memory|oom|restart|nested transactions|could not|refused",
                    lg$mensaje, ignore.case = TRUE) | tolower(lg$nivel) %in% c("error", "warn", "warning")
  cat(sprintf("Log (ventana): %d líneas · %d con error/advertencia · última línea %s\n",
              nrow(lg), sum(problema), hm(if (nrow(lg)) max(lg$hora) else NA)))
  if (any(problema)) {
    m <- gsub("[0-9a-f]{8}-[0-9a-f-]{27,}", "<id>", substr(lg$mensaje[problema], 1, 160))
    m <- gsub("\\d+", "#", m)
    t <- sort(table(m), decreasing = TRUE)
    cat("  Mensajes más frecuentes:\n")
    for (i in seq_len(min(8, length(t)))) cat(sprintf("  %4d × %s\n", t[[i]], names(t)[i]))
  }
}, silent = FALSE)

# Supabase ---------------------------------------------------------------------
cat("\n== SUPABASE ==\n")
try({
  t0 <- Sys.time()
  con <- conectar_base()
  t_con <- seg(t0)
  t0 <- Sys.time(); DBI::dbGetQuery(con, "SELECT 1"); t_ping <- seg(t0)
  # Conexiones de apps (vía pooler Supavisor) vs servicios internos de Supabase
  act <- tryCatch(DBI::dbGetQuery(con, "
    SELECT CASE WHEN application_name LIKE 'Supavisor%' OR usename = 'pgbouncer' THEN 'pooler' ELSE 'internas' END
             || ' ' || coalesce(state, 'sin estado') AS state,
           count(*) AS n,
           max(extract(epoch FROM now() - state_change)) FILTER (WHERE state = 'active' AND pid <> pg_backend_pid()) AS activa_max_seg
    FROM pg_stat_activity WHERE datname = current_database() GROUP BY 1 ORDER BY 1"),
                  error = function(e) NULL)
  if (!is.null(act) && any(!is.na(act$activa_max_seg)))
    cat("Consulta activa más larga (sin contar esta):", round(max(act$activa_max_seg, na.rm = TRUE), 1), "s\n")
  tam <- tryCatch(DBI::dbGetQuery(con, "SELECT pg_size_pretty(pg_database_size(current_database())) AS t")$t, error = function(e) "–")
  tabla <- Sys.getenv("SD_TABLE", "jusmer_ola1_responses")
  t0 <- Sys.time()
  crudo <- DBI::dbGetQuery(con, sprintf("SELECT * FROM %s", DBI::dbQuoteIdentifier(con, tabla)))
  t_lect <- seg(t0)
  DBI::dbDisconnect(con)
  cat(sprintf("Conexión %s s · ping %s s · lectura de la tabla (%d filas) %s s · tamaño base %s\n",
              t_con, t_ping, nrow(crudo), t_lect, tam))
  if (!is.null(act)) cat("Conexiones abiertas:", paste(sprintf("%s %d", ifelse(is.na(act$state), "sin estado", act$state), as.integer(act$n)), collapse = " · "), "\n")

  crudo$ip_address <- NULL
  crudo[] <- lapply(crudo, function(x) { x <- as.character(x); x[is.na(x)] <- ""; x })
  d <- preparar(crudo)
  resumen <- function(x, etiqueta) {
    sal <- table(factor(x$current_page[grepl("^end_", x$current_page)],
                        c("end_normal", "end_calidad", "end_consent", "end_quotafull")))
    en_curso <- sum(x$acepta & !grepl("^end_", x$current_page) & !is.na(x$inicio) & x$inicio >= ahora - 60 * 60)
    cat(sprintf("%s: %d sesiones · %d aceptan · %d completas · salidas: %s · en curso (<60 min, sin salida): %d\n",
                etiqueta, nrow(x), sum(x$acepta), sum(x$completa),
                paste(sprintf("%s %d", sub("end_", "", names(sal)), as.integer(sal)), collapse = ", "), en_curso))
  }
  resumen(d[!is.na(d$inicio) & d$inicio >= desde, ], sprintf("Últimos %d min", VENTANA_MIN))
  resumen(d[!is.na(d$inicio) & d$inicio >= hoy, ], "Hoy")
  campo <- d[!is.na(d$inicio) & d$inicio >= as.POSIXct("2026-10-30", tz = TZ), ]
  cat(sprintf("Total campo (desde 30/10): %d completas\n", sum(campo$completa)))
  cat(sprintf("Última sesión iniciada: %s · última completa terminada: %s\n",
              hm(max(d$inicio, na.rm = TRUE)), hm(max(d$fin[d$completa], na.rm = TRUE))))
  h <- d[!is.na(d$inicio) & d$inicio >= hoy, ]
  if (nrow(h)) {
    porh <- table(format(h$inicio, "%H", tz = TZ))
    comp <- table(factor(format(h$inicio[h$completa], "%H", tz = TZ), names(porh)))
    cat("Hoy por hora (sesiones/completas):", paste(sprintf("%sh %d/%d", names(porh), as.integer(porh), as.integer(comp)), collapse = " · "), "\n")
  }
}, silent = FALSE)
