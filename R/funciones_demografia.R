# Funciones para evaluar la calidad de la declaración de edad en un censo:
# índices de Whipple, Myers y Naciones Unidas, y prorrateo de la población
# con edad no especificada con suavizamiento 1/16.
#
# Las funciones reciben tablas con columnas estado, ano, edades, total,
# hombres y mujeres, así que sirven para cualquier entidad y año.

library(readxl)
library(dplyr)
library(stringr)
library(tidyr)
library(purrr)


# ---- Lectura ---------------------------------------------------------------

limpiar_numero <- function(x) {
  as.numeric(str_replace_all(as.character(x), ",", ""))
}

normalizar_nombres <- function(nombres) {
  nombres |>
    str_trim() |>
    str_to_lower() |>
    stringi::stri_trans_general("Latin-ASCII") |>
    str_replace_all("[^a-z0-9]+", "_") |>
    str_replace_all("_$", "")
}

# Edad inicial de cada renglón: 25 para "25", 10 para "De 10 a 14 años".
# "Total" y "No especificado" quedan como NA.
extraer_edad_inicial <- function(edades) {
  edades_chr <- str_to_lower(as.character(edades))
  case_when(
    str_detect(edades_chr, "total") ~ NA_real_,
    str_detect(edades_chr, "no especific") ~ NA_real_,
    TRUE ~ as.numeric(str_extract(edades_chr, "\\d+"))
  )
}

# Lee una hoja del Excel aunque el encabezado no esté en la primera fila.
leer_hoja_demografica <- function(ruta_excel, hoja) {
  bruto <- suppressMessages(read_excel(ruta_excel, sheet = hoja, col_names = FALSE))
  bruto <- bruto[, colSums(!is.na(bruto)) > 0, drop = FALSE]
  bruto <- bruto[rowSums(!is.na(bruto)) > 0, , drop = FALSE]

  fila_encabezado <- which(apply(bruto, 1, function(fila) {
    all(c("estado", "ano", "edades", "total", "hombres", "mujeres") %in%
          normalizar_nombres(as.character(fila)))
  }))[1]
  if (is.na(fila_encabezado)) stop("No se encontró el encabezado en la hoja: ", hoja)

  datos <- bruto[(fila_encabezado + 1):nrow(bruto), , drop = FALSE]
  names(datos) <- normalizar_nombres(as.character(unlist(bruto[fila_encabezado, ])))

  datos |>
    select(estado, ano, edades, total, hombres, mujeres) |>
    mutate(
      estado = as.character(estado),
      ano = as.integer(limpiar_numero(ano)),
      edades = str_squish(as.character(edades)),
      across(c(total, hombres, mujeres), limpiar_numero),
      edad_inicial = extraer_edad_inicial(edades)
    ) |>
    filter(!is.na(estado), !is.na(ano), !is.na(edades))
}


# ---- Clasificaciones -------------------------------------------------------

clasificar_whipple <- function(indice) {
  case_when(
    is.na(indice) ~ NA_character_,
    indice < 100 ~ "Menor que 100: posible rechazo del dígito",
    indice < 105 ~ "Datos muy precisos",
    indice < 110 ~ "Datos precisos",
    indice < 125 ~ "Datos aproximados",
    indice < 175 ~ "Datos deficientes",
    TRUE ~ "Datos muy deficientes"
  )
}

clasificar_myers <- function(indice) {
  case_when(
    is.na(indice) ~ NA_character_,
    indice < 5 ~ "Baja concentración en algún dígito",
    indice < 15 ~ "Mediana concentración en algún dígito",
    indice < 30 ~ "Alta concentración en algún dígito",
    TRUE ~ "Muy alta concentración en algún dígito"
  )
}

clasificar_inu <- function(indice) {
  case_when(
    is.na(indice) ~ NA_character_,
    indice < 20 ~ "Información satisfactoria",
    indice < 40 ~ "Información de calidad intermedia",
    TRUE ~ "Información deficiente"
  )
}


# ---- Índice de Whipple -----------------------------------------------------

# Edades de 23 a 62 años. Mide la preferencia por edades terminadas en 0 y 5
# (factor 5) o solo en 0 (factor 10). Sin preferencia, el índice vale 100.
calcular_whipple_columna <- function(datos, columna = "total", tipo = c("digito_5", "digito_0")) {
  tipo <- match.arg(tipo)
  datos_w <- datos |>
    filter(!is.na(edad_inicial), edad_inicial >= 23, edad_inicial <= 62) |>
    arrange(edad_inicial)

  poblacion <- datos_w[[columna]]
  edades <- datos_w$edad_inicial
  numerador <- if (tipo == "digito_5") {
    5 * sum(poblacion[edades %% 5 == 0], na.rm = TRUE)
  } else {
    10 * sum(poblacion[edades %% 10 == 0], na.rm = TRUE)
  }
  100 * numerador / sum(poblacion, na.rm = TRUE)
}

calcular_whipple <- function(datos_desplegados) {
  expand_grid(poblacion = c("total", "hombres", "mujeres"),
              tipo = c("digito_5", "digito_0")) |>
    mutate(
      indice_whipple = map2_dbl(poblacion, tipo, ~ calcular_whipple_columna(datos_desplegados, .x, .y)),
      clasificacion = clasificar_whipple(indice_whipple),
      poblacion = str_to_title(poblacion),
      digito_evaluado = if_else(tipo == "digito_5", "Terminación 0 y 5", "Terminación 0")
    ) |>
    select(poblacion, digito_evaluado, indice_whipple, clasificacion)
}


# ---- Índice de Myers -------------------------------------------------------

# Edades de 10 a 79 años. Para cada dígito final j:
#   Pj  = población de 10 a 69 años con edad terminada en j
#   Pj' = población de 20 a 79 años con edad terminada en j
# Se ponderan con 1..10 y 9..0. Mj es la desviación de cada dígito respecto
# al 10% esperado, y el índice es la suma de |Mj|.
calcular_myers_columna <- function(datos_desplegados, columna = "total") {
  datos_m <- datos_desplegados |>
    filter(!is.na(edad_inicial), edad_inicial >= 10, edad_inicial <= 79) |>
    arrange(edad_inicial)
  if (nrow(datos_m) != 70) stop("Myers necesita las edades simples de 10 a 79 años.")

  tabla <- datos_m |>
    mutate(
      digito = edad_inicial %% 10,
      decena = paste0(floor(edad_inicial / 10) * 10, "-", floor(edad_inicial / 10) * 10 + 9),
      valor = .data[[columna]]
    ) |>
    select(digito, decena, valor) |>
    pivot_wider(names_from = decena, values_from = valor) |>
    arrange(digito) |>
    mutate(
      Pj = rowSums(across(c(`10-19`, `20-29`, `30-39`, `40-49`, `50-59`, `60-69`))),
      Pj_prima = rowSums(across(c(`20-29`, `30-39`, `40-49`, `50-59`, `60-69`, `70-79`))),
      aj = 1:10,
      aj_prima = 9:0,
      suma_ponderada = aj * Pj + aj_prima * Pj_prima,
      Mj = (suma_ponderada / sum(suma_ponderada) - 0.10) * 100,
      atraccion_rechazo = if_else(Mj >= 0, "Atracción", "Rechazo")
    )

  indice <- sum(abs(tabla$Mj))
  list(tabla = tabla, indice_myers = indice, clasificacion = clasificar_myers(indice))
}

calcular_myers <- function(datos_desplegados) {
  columnas <- c("total", "hombres", "mujeres")
  resultados <- map(columnas, ~ calcular_myers_columna(datos_desplegados, .x))
  list(
    resumen = tibble(
      poblacion = str_to_title(columnas),
      indice_myers = map_dbl(resultados, "indice_myers"),
      clasificacion = map_chr(resultados, "clasificacion")
    ),
    tablas = setNames(map(resultados, "tabla"), columnas)
  )
}


# ---- Índice de Naciones Unidas ---------------------------------------------

# Usa 17 grupos quinquenales (0-4 a 80-84). Combina la irregularidad de los
# cocientes de edad de hombres y mujeres con la del índice de masculinidad:
# INU = IEH + IEM + 3 * IRS. Los promedios se dividen entre 17 - 2 = 15.
calcular_inu <- function(datos_quinquenales) {
  datos_q <- datos_quinquenales |>
    filter(!is.na(edad_inicial), edad_inicial <= 80) |>
    arrange(edad_inicial)
  k <- nrow(datos_q)
  if (k != 17) stop("El INU necesita los 17 grupos quinquenales de 0-4 a 80-84.")

  hombres <- datos_q$hombres
  mujeres <- datos_q$mujeres
  indice_masculinidad <- 100 * hombres / mujeres
  dif_im <- c(NA, abs(diff(indice_masculinidad)))

  cociente_h <- cociente_m <- rep(NA_real_, k)
  for (i in 2:(k - 1)) {
    cociente_h[i] <- 100 * 2 * hombres[i] / (hombres[i - 1] + hombres[i + 1])
    cociente_m[i] <- 100 * 2 * mujeres[i] / (mujeres[i - 1] + mujeres[i + 1])
  }
  desv_h <- abs(100 - cociente_h)
  desv_m <- abs(100 - cociente_m)

  # Se excluyen el primer y el último grupo.
  centrales <- 2:(k - 1)
  ieh <- sum(desv_h[centrales]) / (k - 2)
  iem <- sum(desv_m[centrales]) / (k - 2)
  irs <- sum(dif_im[centrales]) / (k - 2)
  inu <- ieh + iem + 3 * irs

  list(
    tabla = datos_q |>
      transmute(grupo_edad = edades, hombres, mujeres, indice_masculinidad, dif_im,
                cociente_edades_h = cociente_h, cociente_edades_m = cociente_m,
                desv_h, desv_m),
    resumen = tibble(
      indice_edades_hombres = ieh,
      indice_edades_mujeres = iem,
      indice_regularidad_sexo = irs,
      indice_naciones_unidas = inu,
      clasificacion = clasificar_inu(inu)
    )
  )
}


# ---- Prorrateo y suavizamiento 1/16 ----------------------------------------

# Reparte la población con edad no especificada en proporción al tamaño de
# cada grupo quinquenal (18 grupos: 0-4 a 85 y más).
prorratear <- function(grupos, no_especificado) {
  factor <- 1 + no_especificado / sum(grupos)
  round(grupos * factor)
}

# Promedio móvil 1/16 sobre los grupos 10-14 a 70-74; los dos primeros y los
# tres últimos grupos no se suavizan. Después se reescala para conservar el
# total prorrateado.
suavizar_1_16 <- function(grupos) {
  suavizado <- grupos
  for (i in 3:15) {
    suavizado[i] <- ceiling((-grupos[i - 2] + 4 * grupos[i - 1] + 10 * grupos[i] +
                               4 * grupos[i + 1] - grupos[i + 2]) / 16)
  }
  suavizado
}

ajustar_total <- function(suavizado, total_objetivo) {
  round(suavizado * total_objetivo / sum(suavizado))
}

# Tabla completa: grupos originales, prorrateados, suavizados y ajustados.
calcular_prorrateo <- function(datos_quinquenales) {
  grupos <- datos_quinquenales |> filter(!is.na(edad_inicial)) |> arrange(edad_inicial)
  no_esp <- datos_quinquenales |> filter(str_detect(str_to_lower(edades), "no especific"))
  if (nrow(grupos) != 18 || nrow(no_esp) != 1) {
    stop("El prorrateo necesita 18 grupos quinquenales y el renglón 'No especificado'.")
  }

  tabla <- tibble(grupo_edad = grupos$edades)
  for (sexo in c("total", "hombres", "mujeres")) {
    prorrateado <- prorratear(grupos[[sexo]], no_esp[[sexo]])
    suavizado <- suavizar_1_16(prorrateado)
    tabla[[paste0(sexo, "_original")]] <- grupos[[sexo]]
    tabla[[paste0(sexo, "_prorrateado")]] <- prorrateado
    tabla[[paste0(sexo, "_suavizado")]] <- suavizado
    tabla[[paste0(sexo, "_ajustado")]] <- ajustar_total(suavizado, sum(prorrateado))
  }
  tabla
}
