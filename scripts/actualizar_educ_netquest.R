# Convierte el Excel de Netquest con el nivel educativo del perfil de cada
# panelista (CL_education_level) en config/educacion_netquest.csv, que lee el
# monitor (pestaña Muestra). Para actualizarlo: dejar el Excel nuevo en
# scripts/, ajustar ARCHIVO si cambia el nombre, correr este script desde la
# carpeta monitor-jusmer y volver a publicar la app.

ARCHIVO <- "scripts/educacion-netquest.xlsx"

x <- readxl::read_excel(ARCHIVO, col_types = "text")
stopifnot(all(c("ticket", "id_user", "CL_education_level") %in% names(x)))
salida <- data.frame(
  ticket = x$ticket,
  id_user = x$id_user,
  codigo = sub("\\.0+$", "", x$CL_education_level)
)
salida <- salida[!is.na(salida$codigo), ]
utils::write.csv(salida, "config/educacion_netquest.csv", row.names = FALSE, na = "")
cat("Casos con nivel educativo Netquest:", nrow(salida), "\n")
print(table(salida$codigo))
