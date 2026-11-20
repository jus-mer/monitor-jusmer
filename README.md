# Monitor de respuestas · Encuesta JusMer, Ola 1

Aplicación Shiny para seguir el trabajo de campo de la Ola 1 de la encuesta del proyecto Fondecyt N° 1250518 *Market Justice and Deservingness of Social Welfare* ([jus-mer/survey-jusmer](https://github.com/jus-mer/survey-jusmer), carpeta `surveys/ola1`). Muestra el avance del campo y los descriptivos básicos de las variables principales a medida que entran las respuestas.

La app es independiente de la encuesta: **solo lee** la tabla de respuestas y nunca escribe en ella.

![Resumen del monitor](docs/capturas/resumen.png)


## Qué muestra

| Pestaña | Contenido |
|---|---|
| **Resumen** | Encuestas completas y avance respecto de la meta, sesiones iniciadas, tasa de término, completas del día, duración mediana, sesiones por día, curva acumulada, abandono según el último bloque con respuestas (por forma A/B), formas y dispositivos. |
| **Muestra** | Sexo, edad y nivel educativo contra el marco de cuotas; nivel educativo, ingreso, estatus subjetivo, posición política, situación económica, identidad de género, sistema de salud, tipo de colegio, personas en el hogar y educación de los padres. |
| **Escalas** | Barras apiladas y tabla (N, media, DE, % alto, % No sé, % Prefiero no responder) para Justicia de mercado, Lucro, Merecimiento, Meritocracia e Inseguridad económica. |
| **Conjoint y decisiones** | Reparto del monto en las seis tareas, consistencia de la tarea repetida (práctica y tarea 6 con lados invertidos), decisiones de pago (`dec_1` a `dec_3`) y prioridades de inversión pública. |
| **Calidad** | Chequeo de atención, comprensión de la tarea conjoint, respuestas muy rápidas, respuesta idéntica en la batería de merecimiento, exceso de No sé / No responde, duración e ítems con más No sé y Prefiero no responder; patrones de respuesta en las escalas (línea recta, auto-correlación y secuencias repetidas con `responsePatterns`) y cruce y correlación entre los chequeos de atención y de manipulación. |
| **Explorador** | Frecuencias de cualquier variable del cuestionario, con sus etiquetas. |

La barra lateral permite filtrar por casos (solo completas o todas las que aceptaron), forma de la encuesta y fechas, y ajustar la meta. La app vuelve a leer los datos cada 60 segundos y también con el botón **Actualizar ahora**.

### Definiciones

- **Completa**: aceptó el consentimiento y respondió alguna de las preguntas finales (`salud_prov`, `educ_prov`; `VARS_FIN` en `R/config.R`). Para las sesiones desde el 12/11 (`INICIO_FILTRO_CALIDAD`), además debe haber pasado el filtro de calidad de la encuesta (`filtro_calidad == "ok"`): no cuentan las excluidas por calidad ni quien respondió la última página sin enviarla.
- **Abandono**: para las sesiones incompletas, el bloque más avanzado con alguna respuesta, según el orden de módulos de su forma (A: Meritocracia, Actitudes, Percepción de servicios, Merecimiento; B: Merecimiento, Percepción de servicios, Actitudes, Meritocracia).
- **Sliders con "Prefiero no responder"**: si la casilla `<id>_nr` está marcada, el valor del slider no se considera. En `posicion_politica` esa casilla también ofrece "Independiente" (11) y "Ninguno" (12), que tampoco cuentan como posición.
- **Tarea repetida**: la tarea 6 muestra los perfiles de la práctica con los lados invertidos; una respuesta consistente cumple práctica + tarea 6 ≈ $2.000.000 (tolerancia de $200.000).
- **Respuestas muy rápidas**: duración menor a un tercio de la mediana.
- Los umbrales de los indicadores de calidad (`UMBRALES_CALIDAD`) son referenciales y pueden ajustarse.
- El **marco de cuotas** (`MARCO_CUOTAS`) guarda el n límite de cada cuota de la Ola 1 (meta de 4.500 completas). En la pestaña Muestra la barra es el n observado y la marca negra ese límite.
- El **nivel educativo según Netquest** (pestaña Muestra) sale de `config/educacion_netquest.csv`, que se genera desde el Excel de Netquest con `scripts/actualizar_educ_netquest.R`. Los casos que entren después de ese archivo aparecen sin información hasta que se actualice y se vuelva a publicar.

## Estructura

```
monitor-jusmer/
├── app.R               interfaz y servidor
├── R/
│   ├── config.R        bloques, módulos, etiquetas breves, cuotas, umbrales
│   ├── preguntas.R     lectura de config/questions.yml
│   ├── datos.R         lectura de la tabla de respuestas en Supabase
│   ├── procesar.R      variables derivadas y descriptivos
│   ├── patrones.R      patrones de respuesta (responsePatterns) y cruce de chequeos
│   └── graficos.R      gráficos (ggplot2)
├── scripts/
│   ├── descargar_base.R  carga la tabla de respuestas en la sesión de R
│   └── patrones_respuesta.R  patrones de respuesta caso a caso (CSV y PDF)
├── config/
│   └── questions.yml   copia de surveys/ola1/_survey/questions.yml
├── docs/capturas/      capturas de pantalla
└── .env.example        plantilla de credenciales (el .env real no se sube)
```

Si cambia el cuestionario: copiar de nuevo `surveys/ola1/_survey/questions.yml` a `config/questions.yml` y ajustar las listas de `R/config.R` (bloques, módulos y etiquetas breves).

