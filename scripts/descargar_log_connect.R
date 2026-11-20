# Descarga del log de la encuesta en Posit Connect Cloud ------------------------
#
# Baja el log completo de la app (el mismo que muestra el panel "Logs" de Connect
# Cloud) desde una fecha, sin tener que subir en el panel ni copiar y pegar.
# Lo guarda en scripts/log_connect.txt y lo deja en memoria como el data frame
# `log_connect`, para buscar en la misma sesión de R.
#
# Uso, desde la carpeta del proyecto (abrir monitor-jusmer.Rproj):
#   source("scripts/descargar_log_connect.R")
#   subset(log_connect, grepl("Nested transactions", mensaje))
#
# Usa la cuenta "jus-mer" que rsconnect ya tiene registrada en este computador
# (la misma con la que se publica la encuesta), así que no necesita el .env.
# Cada deploy crea una revisión con su propio log; el script baja las de todas
# las revisiones que estuvieron activas desde DESDE y las une.

DESDE   <- as.POSIXct("2026-10-30 00:00:00", tz = "America/Santiago")
HASTA   <- Sys.time()
CUENTA  <- "jus-mer"
CONTENT <- "01a1136c-df1f-c125-dbbc-30faed92627b"  # Encuesta JUSMER - Ola 1
ARCHIVO <- "scripts/log_connect.txt"

local({
  ns <- asNamespace("rsconnect")
  GET <- get("GET", ns)
  cliente <- rsconnect:::clientForAccount(rsconnect::accountInfo(CUENTA))
  logs_api <- get("parseHttpUrl", ns)(paste0(get("connectCloudUrls", ns)()$logs, "/v1"))

  hora <- function(x) as.POSIXct(sub("\\.\\d+Z$", "", x), format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")

  # Revisiones activas en algún momento entre DESDE y HASTA: cada una lo está
  # desde que se crea hasta que se crea la siguiente.
  revs <- cliente$withTokenRefreshRetry(GET, paste0("/contents/", CONTENT, "/revisions?limit=500"))$data
  revs <- Filter(function(r) identical(r$publish_result, "success") && !is.null(r$publish_log_channel), revs)
  inicio <- do.call(c, lapply(revs, function(r) hora(r$created_time)))
  revs <- revs[order(inicio)]; inicio <- sort(inicio)
  fin <- c(inicio[-1], as.POSIXct(Inf, origin = "1970-01-01", tz = "UTC"))
  revs <- revs[as.numeric(fin) >= as.numeric(DESDE) & as.numeric(inicio) <= as.numeric(HASTA)]

  bajar_canal <- function(canal) {
    token <- cliente$getAuthorization(canal)
    pedir <- function(cursor) {
      ruta <- paste0("/logs/", canal, "?traversal_direction=backward&limit=1500",
                     if (!is.null(cursor)) paste0("&sort_key__lt=", format(cursor, scientific = FALSE)))
      tryCatch(GET(logs_api, list(accessToken = token), ruta)$data,
        rsconnect_http_401 = function(e) {  # el token del log vence: pedir otro
          token <<- cliente$getAuthorization(canal)
          GET(logs_api, list(accessToken = token), ruta)$data
        })
    }
    partes <- list(); cursor <- NULL
    repeat {
      d <- pedir(cursor)
      if (length(d) == 0) break
      partes[[length(partes) + 1]] <- data.frame(
        sort_key  = vapply(d, function(x) as.numeric(x$sort_key), 0),
        timestamp = vapply(d, function(x) as.numeric(x$timestamp), 0),
        tipo      = vapply(d, function(x) x$type %||% "", ""),
        nivel     = vapply(d, function(x) x$level %||% "", ""),
        mensaje   = vapply(d, function(x) x$message %||% "", "")
      )
      cursor <- min(partes[[length(partes)]]$sort_key)
      mas_antigua <- as.POSIXct(min(partes[[length(partes)]]$timestamp) / 1e6, origin = "1970-01-01")
      message("  ", format(mas_antigua, tz = "America/Santiago", "%d/%m %H:%M"), " (", sum(vapply(partes, nrow, 0)), " líneas)")
      if (mas_antigua < DESDE) break
    }
    do.call(rbind, partes)
  }

  todo <- do.call(rbind, lapply(revs, function(r) {
    message("Revisión del ", format(hora(r$created_time), tz = "America/Santiago", "%d/%m %H:%M"), ":")
    d <- bajar_canal(r$publish_log_channel)
    if (!is.null(d)) d$revision <- r$id
    d
  }))

  todo$hora <- as.POSIXct(todo$timestamp / 1e6, origin = "1970-01-01", tz = "America/Santiago")
  todo <- todo[as.numeric(todo$hora) >= as.numeric(DESDE) & as.numeric(todo$hora) <= as.numeric(HASTA), ]
  todo <- todo[!duplicated(todo[c("revision", "sort_key")]), ]
  todo <- todo[order(todo$timestamp, todo$sort_key), ]
  rownames(todo) <- NULL

  # Mismo formato de hora que el panel de Connect Cloud: 2026-10-31T02:53:36-03:00
  marca <- sub("(\\d{2})(\\d{2})$", "\\1:\\2", format(todo$hora, "%Y-%m-%dT%H:%M:%S%z"))
  writeLines(paste0(marca, "   ", gsub("\n", " ", todo$mensaje)), ARCHIVO, useBytes = TRUE)

  log_connect <<- todo[c("hora", "tipo", "nivel", "mensaje", "revision")]
  message("Guardadas ", nrow(todo), " líneas (", format(min(todo$hora), "%d/%m %H:%M"), " a ",
          format(max(todo$hora), "%d/%m %H:%M"), ") en ", ARCHIVO, " y en `log_connect`.")
})
