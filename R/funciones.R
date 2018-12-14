#' Transform2Dummy
#'
#' Transforma una variable categorica a dummys
#' @param variable variable categorica a transformar
#' @param prefix prefijo a agregarle a los nombres de las dummys (formato character)
#' @keywords categorical transform
#' @export
#' @examples

transform2Dummy <- function(variable, prefix = NULL) {
  # Transforma una variable categorica a una matriz de 1's y 0's
  # Didac, 20160909
  # variable: vector categorico a transformar a dummy
  # prefix: si queremos anyadir un prefijo a los colnames de la variable (formato character)

  # Prevenim per quan hi ha NA's
  variable <- as.character(variable)
  variable[is.na(variable)] <- "NA"

  result <- data.frame(sapply(unique(variable), function(x) {
    ifelse(variable == x, 1, 0)
  }))
  colnames(result) <- unique(as.character(variable))
  if (ncol(result) > 1) {
    result <- result[, order(colnames(result))]
  }
  colnames(result) <- gsub(" ", "", colnames(result))
  if (!is.null(prefix)) {
    colnames(result) <- paste(prefix, colnames(result), sep = "")
  }
  return(result)
}


#' One hot encoding para variables categoricas
#'
#' Transforma variables categoricas de una base a dummys y la retorna. Tiene la opcion de guardar los nombres de variables oneHot, necesarios para evaluar una nueva base
#' @param base data.frame a transformar sus variables
#' @param variables_categoricas vector de nombres de las variables categoricas de la base
#' @param guardar si se guardaran los nombres de las variables oneHot, defaults to FALSE
#' @param directorio directorio donde se guardarian los nombres, defaults to current directory
#' @param sufijo sufijo a anexar al nombre del archivo de nombres de variables, defaults to ""
#' @keywords categorical transform
#' @export
#' @examples
onehot_transform <- function(base, variables_categoricas, guardar = FALSE,  directorio = getwd(), sufijo = ""){
	Sys.setlocale("LC_CTYPE","C")
	oneHotData <- base[, variables_categoricas]
	for (variable in variables_categoricas) {
	  oneHotData <- cbind.data.frame(oneHotData, transform2Dummy(oneHotData[, variable], prefix = variable))
	  oneHotData[, variable] <- NULL; gc()
	}

	#Guardamos el nombre de las variables que han pasado por el one hot encode
	if (guardar) {
		cols_onehot <- colnames(oneHotData)
		file <- paste(directorio, 'Variables_OneHot_', sufijo, ".RData", sep = '')
		save(cols_onehot, file = file); cat('Archivo guardado:', file, '\n')
	}

	#Enganchamos las nuevas variables y borramos las antiguas
	base <- cbind.data.frame(base, oneHotData)
	base_onehot <- base[, !colnames(base) %in% variables_categoricas];

	return(base_onehot)
}

#' One hot encoding para variables categoricas
#'
#' Funcion re penca que nadie deberia usar para guardar las variables del modelo
#' @param base data.frame a transformar sus variables
#' @param variable_dependiente vector de nombres de las variables categoricas de la base
#' @param directorio directorio donde se guardarian los nombres, defaults to current directory
#' @param sufijo sufijo a anexar al nombre del archivo de nombres de variables, defaults to ""
#' @keywords categorical transform
#' @export
#' @examples
guardar_variables_modelo <- function(base, variable_dependiente, directorio="", sufijo=""){
	#Nombres de las columnas
	cols <- colnames(base)

	#Buscar la posicion de la variable dependiente y sacarla
	x <- grep(variable_dependiente, cols)
	cols_independientes <- cols[-x]

	#Guardamos todas las variables que entran en el modelo
	file <- paste(directorio, '01-Variables_Modelo_', sufijo, '.RData', sep = '')
	save(cols_independientes, file = file)
	print(paste("Variables del modelo guardadas en ", file, "\n"))
}

#' tran_test_split
#'
#' Separa aleatoriamento una base en entrenamiento y validacion
#' @param base data.frame a separar en entrenamiento y validacion
#' @param porc_val porcentaje de la muestra que se asignará a validacion. Default 10%
#' @param seed semilla para que sea replicable la division
#' @keywords train test split
#' @export
#' @examples
train_test_split <- function(base, porc_val = 0.1, seed = 2017){
	set.seed(seed)
	ncons <- sample(1:nrow(base), nrow(base)*(1 - porc_val))
	cons <- base[ncons, ]
	test <- base[-ncons, ]

	lista <- list(cons = cons, test = test)
	return(lista)
}

#' cargar_generico
#'
#' Forma de cargar un archivo RData con un solo objeto para poder asignarlo directamente a un objeto nuevo
#' @param archivo path al archivo RData que se quiere cargar
#' @keywords load files
#' @export
#' @examples
cargar_generico <- function(archivo){
	file <- paste(archivo, sep = '')
	aux <- load(file); cat('Archivo cargado:', file, '\n')
	base <- get(aux);

	return(base)
}


#' Completar dummys faltantes
#'
#' Crea las dummys que falten en la base
#' @param base_aevaluar base que se quiere usar para evaluar modelo xgboost
#' @param variables_onehot nombre de todas las dummys que modelo xgboost espera se encuentren en la base
#' @keywords xgboost dummys complete
#' @export
#' @examples
completar_faltantes <- function(base_aevaluar, variables_onehot){
	variables <- variables_onehot[!variables_onehot %in% colnames(base_aevaluar)]
	for (var in variables) {
	  base_aevaluar[, var] <- 0
	}

	return(base_aevaluar)
}


#' iniciar_proyecto
#'
#' Crea las dummys que falten en la base
#' @param PATH path al lugar donde se quiere iniciar un proyecto
#' @param nombre nombre del proyecto, carpeta a crear
#' @param autor nombre de quien trabajara sobre el proyecto
#' @keywords inicio proyecto ais
#' @export
#' @examples
iniciar_proyecto <- function(PATH = NULL, nombre = NULL, autor = NULL){

  PATH <- paste0(PATH, '/', nombre, "/" )
  DATADIR    <- paste(PATH, 'data',   sep = '')
  INPUTDIR   <- paste(PATH, 'input',  sep = '')
  OUTPUTDIR  <- paste(PATH, 'output', sep = '')
  TEMPDIR    <- paste(PATH, 'temp',   sep = '')
  SYNTAXDIR  <- paste(PATH, 'syntax', sep = '')
  DOCDIR     <- paste(PATH, 'doc',    sep = '')
  LOGDIR     <- paste(PATH, 'logs',    sep = '')
  SCRIPTSDIR <- paste(PATH, 'scripts', sep = '')
  FUNSDIR <- paste(PATH, 'funciones', sep = '')
  #DEYDEDIR   <- paste(PREFIX, 'deyde/logs/', sep = '')
  dir.create(PATH) ;    cat('Carpeta creada:', PATH,    '\n')
  if (! file.exists(DATADIR))    { dir.create(DATADIR);    cat('Carpeta creada:', DATADIR,    '\n') }
  if (! file.exists(INPUTDIR))   { dir.create(INPUTDIR);   cat('Carpeta creada:', INPUTDIR,   '\n') }
  if (! file.exists(OUTPUTDIR))  { dir.create(OUTPUTDIR);  cat('Carpeta creada:', OUTPUTDIR,  '\n') }
  if (! file.exists(TEMPDIR))    { dir.create(TEMPDIR);    cat('Carpeta creada:', TEMPDIR,    '\n') }
  if (! file.exists(SYNTAXDIR))  { dir.create(SYNTAXDIR);  cat('Carpeta creada:', SYNTAXDIR,  '\n') }
  if (! file.exists(DOCDIR))     { dir.create(DOCDIR);     cat('Carpeta creada:', DOCDIR,     '\n') }
  if (! file.exists(LOGDIR))     { dir.create(LOGDIR);     cat('Carpeta creada:', LOGDIR,     '\n') }
  if (! file.exists(SCRIPTSDIR)) { dir.create(SCRIPTSDIR); cat('Carpeta creada:', SCRIPTSDIR, '\n') }
  if (! file.exists(FUNSDIR)) { dir.create(FUNSDIR); cat('Carpeta creada:', FUNSDIR, '\n') }

  # Adaptamos rutas
  DATADIR    <- paste(DATADIR,    '/', sep = '')
  INPUTDIR   <- paste(INPUTDIR,   '/', sep = '')
  OUTPUTDIR  <- paste(OUTPUTDIR,  '/', sep = '')
  TEMPDIR    <- paste(TEMPDIR,    '/', sep = '')
  SYNTAXDIR  <- paste(SYNTAXDIR,  '/', sep = '')
  DOCDIR     <- paste(DOCDIR,     '/', sep = '')
  LOGDIR     <- paste(LOGDIR,     '/', sep = '')
  SCRIPTSDIR <- paste(SCRIPTSDIR, '/', sep = '')
  FUNSDIR    <- paste(FUNSDIR,    '/', sep = '')

  template <- template_inicio
  template[3] <- paste0("# Autor(es)   : (c) ", autor, " ", Sys.time())
  template[55] <- paste0("script <- paste('[", nombre, "] ", "01_Inicio Proyecto.R')")
  template[14] <- paste0("PATH <- ", "'", PATH , "'")
  writeLines(template, con = paste0(SYNTAXDIR, "000_inicioProyecto.R"))

  cat("Proyecto ", nombre, "iniciado en ", PATH, "\n")
}
