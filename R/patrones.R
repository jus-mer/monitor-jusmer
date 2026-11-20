# Patrones de respuesta en las escalas Likert ------------------------------------
#
# Núcleo del análisis de scripts/patrones_respuesta.R, compartido con la app
# (pestaña Calidad). Usa el paquete responsePatterns:
#   rp.acors()     auto-correlaciones de las respuestas de cada persona a lo
#                  largo de los ítems (lag 1 alto = línea recta).
#   rp.patterns()  secuencias que se repiten (p. ej. 1-2-3-1-2-3).
# Se analizan todas las escalas en el orden de presentación (por forma, porque
# A y B cambian el orden de los módulos) y, por separado, las baterías con al
# menos `min_items` ítems (hoy solo Merecimiento).

OPCIONES_PATRONES <- list(
  min_resp_item = 0.5,  # se omiten ítems que respondió menos de esta proporción de casos
  min_validas   = 0.8,  # proporción mínima de ítems con respuesta 1-5 (sin NS/NR) por caso
  min_items     = 12,   # largo mínimo de una batería para analizarla por separado
  max_lag       = 10,   # desfase máximo de las auto-correlaciones
  percentil     = 95    # desde qué percentil un caso se marca como sospechoso
)

# Códigos 1-5 a número; NS (98), NR (99) y sin respuesta quedan como NA
a_likert <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  x[!x %in% 1:5] <- NA
  x
}

clave_de <- function(nombre) gsub("[^a-z]+", "_", tolower(iconv(nombre, to = "ASCII//TRANSLIT")))

# Corre ambos métodos sobre `items` (en ese orden). `grupo` asigna cada ítem a
# un grupo para colorear los gráficos de rp.plot().
analizar_patrones <- function(d, items, grupo, opc = OPCIONES_PATRONES) {
  m <- as.data.frame(lapply(d[items], a_likert))
  validas <- rowMeans(!is.na(m))
  m$session_id <- d$session_id
  m <- m[validas >= opc$min_validas, , drop = FALSE]
  if (nrow(m) < 10) return(NULL)
  n <- length(items)
  # El paquete avisa por cada auto-correlación que no puede calcular (p. ej.
  # en línea recta, sin varianza): se silencian esos avisos.
  acors <- suppressWarnings(responsePatterns::rp.acors(
    m, max.lag = min(opc$max_lag, n - 3), id.var = "session_id", na.rm = TRUE, na.top = TRUE))
  pats <- suppressWarnings(responsePatterns::rp.patterns(
    m, id.var = "session_id", na.rm = TRUE, na.top = TRUE))
  valores <- m[items]
  indices <- data.frame(
    session_id = acors@id,
    # Proporción de respuestas iguales a la más frecuente (1 = línea recta).
    # Complementa a rp.acors(), que no ve a quien responde casi todo igual.
    pct_modal  = round(apply(valores, 1, function(r) max(table(r)) / sum(!is.na(r))), 2),
    ac_max     = round(acors@indices$max.abs.ac, 2),
    ac_lag     = acors@indices$max.ac.lag,
    ac_pct     = acors@indices$percentile,
    pat_score  = round(pats@indices$score, 2),
    pat_pct    = pats@indices$percentile,
    stringsAsFactors = FALSE
  )
  indices$marca_ac  <- !is.na(indices$ac_pct) & indices$ac_pct >= opc$percentil
  indices$marca_pat <- !is.na(indices$pat_pct) & indices$pat_pct >= opc$percentil
  indices$sospechosa <- indices$marca_ac | indices$marca_pat
  tramos <- rle(grupo)
  fin <- cumsum(tramos$lengths)
  list(items = items, n_excluidos = sum(validas < opc$min_validas), acors = acors, patterns = pats,
       indices = indices, grupos = Map(seq, fin - tramos$lengths + 1, fin),
       cortes = utils::head(fin, -1))
}

# Corre todos los análisis sobre `d` (encuestas completas). Devuelve:
#   patrones    un caso por fila, con los chequeos del monitor y los índices
#               de cada análisis (columnas <analisis>_<indice>)
#   resultados  objetos de responsePatterns por análisis
#   omitidos    ítems omitidos por tener pocas respuestas
patrones_respuesta <- function(d, opc = OPCIONES_PATRONES) {
  rownames(d) <- NULL
  mediana_dur <- stats::median(d$duracion_min, na.rm = TRUE)
  patrones <- data.frame(
    session_id       = d$session_id,
    version_encuesta = d$version_encuesta,
    duracion_min     = round(d$duracion_min, 1),
    falla_atencion   = respondida(d$attention_check),
    falla_manip      = !d$manip_check %in% "reparto",
    rapido           = !is.na(d$duracion_min) & d$duracion_min < mediana_dur / 3,
    stringsAsFactors = FALSE
  )
  if (!nrow(d)) return(list(patrones = patrones, resultados = list(), omitidos = character(0)))

  # Ítems Likert por módulo; se omiten los que casi nadie respondió
  items_modulo <- lapply(MODULOS_ESCALAS, function(m) {
    it <- names(m$items)
    resp <- vapply(it, function(v) mean(respondida(d[[v]])), 0)
    it[resp >= opc$min_resp_item]
  })
  omitidos <- setdiff(unlist(lapply(MODULOS_ESCALAS, function(m) names(m$items))), unlist(items_modulo))
  todos_items <- unlist(items_modulo, use.names = FALSE)
  modulo_de <- setNames(rep(names(items_modulo), lengths(items_modulo)), todos_items)

  # Orden de presentación de los ítems Likert en cada forma
  orden_forma <- lapply(ORDEN_FORMA, function(bloques) {
    flujo <- unlist(BLOQUES_FLUJO[bloques], use.names = FALSE)
    todos_items[order(match(todos_items, flujo))]
  })

  agregar <- function(patrones, indices, clave) {
    names(indices)[-1] <- paste0(clave, "_", names(indices)[-1])
    merge(patrones, indices, by = "session_id", all.x = TRUE, sort = FALSE)
  }

  resultados <- list()

  # Todas las escalas, por forma (grupos = módulos)
  indices_escalas <- list()
  for (f in names(orden_forma)) {
    items <- orden_forma[[f]]
    r <- analizar_patrones(d[d$version_encuesta == f, , drop = FALSE], items, unname(modulo_de[items]), opc)
    if (is.null(r)) next
    resultados[[paste0("escalas_", f)]] <- r
    indices_escalas[[f]] <- r$indices
  }
  if (length(indices_escalas)) patrones <- agregar(patrones, do.call(rbind, indices_escalas), "escalas")

  # Baterías largas por separado (grupos = prefijo del ítem)
  for (b in names(items_modulo)[lengths(items_modulo) >= opc$min_items]) {
    items <- items_modulo[[b]]
    clave <- clave_de(b)
    r <- analizar_patrones(d, items, sub("_\\d+$", "", items), opc)
    if (is.null(r)) next
    resultados[[clave]] <- r
    patrones <- agregar(patrones, r$indices, clave)
  }

  col_sosp <- grep("_sospechosa$", names(patrones), value = TRUE)
  patrones$n_sospechas <- if (length(col_sosp)) rowSums(patrones[col_sosp], na.rm = TRUE) else 0
  col_pct <- grep("_(ac|pat)_pct$", names(patrones), value = TRUE)
  patrones$pct_max <- if (length(col_pct)) {
    suppressWarnings(apply(patrones[col_pct], 1, max, na.rm = TRUE))
  } else NA_real_
  patrones$pct_max[!is.finite(patrones$pct_max)] <- NA
  patrones <- patrones[order(-patrones$n_sospechas, -patrones$pct_max), ]
  rownames(patrones) <- NULL
  list(patrones = patrones, resultados = resultados, omitidos = omitidos)
}

# Resumen por análisis: casos analizados y proporción marcada por cada prueba.
# Las escalas completas (una por forma) se juntan en una sola fila.
resumen_patrones <- function(pr) {
  res <- pr$resultados
  if (!length(res)) return(NULL)
  grupo <- ifelse(grepl("^escalas_", names(res)), "escalas", names(res))
  filas <- lapply(unique(grupo), function(g) {
    rr <- res[grupo == g]
    ind <- do.call(rbind, lapply(rr, `[[`, "indices"))
    data.frame(
      analisis    = g,
      items       = max(vapply(rr, function(r) length(r$items), 0L)),
      analizados  = nrow(ind),
      excluidos   = sum(vapply(rr, `[[`, 0L, "n_excluidos")),
      linea_recta = mean(ind$pct_modal == 1),
      casi_recta  = mean(ind$pct_modal >= 0.9 & ind$pct_modal < 1),
      autocorr    = mean(ind$marca_ac),
      secuencias  = mean(ind$marca_pat),
      marcados    = mean(ind$sospechosa),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, filas)
}

# Opciones del chequeo de manipulación (config/questions.yml); solo "reparto"
# es correcta
MANIP_OPCIONES <- c(
  reparto     = "Repartir un monto (correcta)",
  eleccion    = "Elegir a un único postulante",
  universidad = "Evaluar la universidad"
)

# Chequeo de atención y de manipulación: cruce, correlación (phi) y fallas
cruce_chequeos <- function(d) {
  a <- respondida(d$attention_check)       # falla: debía quedar en blanco
  m <- !d$manip_check %in% "reparto"       # falla: no respondió "repartir un monto"
  n <- length(a)
  fisher <- function(tab) tryCatch(
    stats::fisher.test(tab, simulate.p.value = any(dim(tab) > 2), B = 1e4)$p.value,
    error = function(e) NA_real_)
  # Binaria: falla atención (0/1) con falla manipulación (0/1)
  phi <- if (n > 2 && stats::sd(a) > 0 && stats::sd(m) > 0) stats::cor(a, m) else NA_real_
  p <- if (!is.na(phi)) fisher(table(a, m)) else NA_real_

  # Con las tres opciones del chequeo de manipulación (y "sin respuesta", si hay):
  # V de Cramer de la tabla atención × respuesta
  cats <- c(MANIP_OPCIONES, sin_respuesta = "Sin respuesta")
  resp <- ifelse(respondida(d$manip_check), d$manip_check, "sin_respuesta")
  resp <- factor(resp, levels = unique(c(names(cats), resp)))
  tab <- table(factor(a, c(FALSE, TRUE)), resp)
  tab <- tab[, colSums(tab) > 0, drop = FALSE]
  v <- if (n > 2 && ncol(tab) > 1 && all(rowSums(tab) > 0)) {
    x2 <- suppressWarnings(stats::chisq.test(tab, correct = FALSE)$statistic)
    unname(sqrt(x2 / (n * (min(dim(tab)) - 1))))
  } else NA_real_
  p_v <- if (!is.na(v)) fisher(tab) else NA_real_
  por_respuesta <- data.frame(
    codigo = colnames(tab),
    etiqueta = ifelse(colnames(tab) %in% names(cats), cats[colnames(tab)], colnames(tab)),
    n = as.integer(colSums(tab)),
    falla_atencion = as.integer(tab["TRUE", ]),
    stringsAsFactors = FALSE
  )

  list(
    n = n, phi = phi, p = p, cramer_v = v, p_v = p_v, por_respuesta = por_respuesta,
    falla_atencion = sum(a), falla_manip = sum(m),
    ambos = sum(a & m), solo_atencion = sum(a & !m), solo_manip = sum(!a & m),
    alguno = sum(a | m), ninguno = sum(!a & !m)
  )
}
