

## Install package "raster" if necessary and load it
if (!require("pacman")) install.packages("pacman")
pacman::p_load(raster)

##########################################################################################

InitializeHUME <- function(start, end, step = 1, project, year, variants, project.location, weather.source, template.opt = FALSE, template.param = FALSE, template.state = FALSE) {
  
  ## Checks
  if(!file.exists(project.location)) {stop("Error: Project location does not exist!")}
  if(!file.exists(weather.source)) {stop("Error: File with data for weather does not exist!")}
  if(template.opt != FALSE) {if(!file.exists(template.opt)) {stop("Error: File with template for options does not exist!")}}
  if(template.param != FALSE) {if(!file.exists(template.param)) {stop("Error: File with template for parameters does not exist!")}}
  if(template.state != FALSE) {if(!file.exists(template.state)) {stop("Error: File with template for states does not exist!")}}
  
  ## Aendern des Datums von y-m-d in Excel-Nummer
  start.time <- as.numeric(as.Date(start)) + 25569
  end.time <- as.numeric(as.Date(end)) + 25569
  
  ## Aufspannen der Varianten
  variants <- data.frame(expand.grid(variants, stringsAsFactors = FALSE))
  
  ## Ordnerstruktur
  if(file.exists(file.path(project.location, project, fsep = "\\"))) {print("Hinweis: Der Projektpfad existiert bereits")} else {dir.create(file.path(project.location, project, fsep = "\\"))}
  if(file.exists(file.path(project.location, project, "Weather", fsep = "\\"))) {print("Hinweis: Der Wetterpfad zu diesem Projekt existiert bereits")} else {dir.create(file.path(project.location, project, "Weather", fsep = "\\"))}
  if(file.exists(file.path(project.location, project, "Weather", year, fsep = "\\"))) {print("Hinweis: Das Jahr im Wetterpfad zu diesem Projekt existiert bereits")} else {dir.create(file.path(project.location, project, "Weather", year, fsep = "\\"))}
  if(file.exists(file.path(project.location, project, "Measurements", fsep = "\\"))) {print("Hinweis: Der Measurementspfad zu diesem Projekt existiert bereits")} else {dir.create(file.path(project.location, project, "Measurements", fsep = "\\"))}
  if(file.exists(file.path(project.location, project, "Measurements", year, fsep = "\\"))) {print("Hinweis: Das Jahr im Measurementspfad zu diesem Projekt existiert bereits")} else {dir.create(file.path(project.location, project, "Measurements", year, fsep = "\\"))}
  if(file.exists(file.path(project.location, project, "Ini", fsep = "\\"))) {print("Hinweis: Der Inipfad zu diesem Projekt existiert bereits")} else {dir.create(file.path(project.location, project, "Ini", fsep = "\\"))}
  if(file.exists(file.path(project.location, project, "Ini", year, fsep = "\\"))) {print("Hinweis: Das Jahr im Inipfad zu diesem Projekt existiert bereits")} else {dir.create(file.path(project.location, project, "Ini", year, fsep = "\\"))}
  #if(file.exists(file.path(project.location, project, "Ini", year, "IniFn", fsep = "\\"))) {print("Hinweis: Der IniFn-pfad zu diesem Projekt existiert bereits")} else {dir.create(file.path(project.location, project, "Ini", year, "IniFn", fsep = "\\"))}
  if(file.exists(file.path(project.location, project, "Ini", year, "Opt", fsep = "\\"))) {print("Hinweis: Der Opt-pfad zu diesem Projekt existiert bereits")} else {dir.create(file.path(project.location, project, "Ini", year, "Opt", fsep = "\\"))}
  if(file.exists(file.path(project.location, project, "Ini", year, "Param", fsep = "\\"))) {print("Hinweis: Der Param-pfad zu diesem Projekt existiert bereits")} else {dir.create(file.path(project.location, project, "Ini", year, "Param", fsep = "\\"))}
  if(file.exists(file.path(project.location, project, "Ini", year, "State", fsep = "\\"))) {print("Hinweis: Der State-pfad zu diesem Projekt existiert bereits")} else {dir.create(file.path(project.location, project, "Ini", year, "State", fsep = "\\"))}
  
  ## Wetterdaten in Zielordner kopieren
  file.copy(weather.source, file.path(project.location, project, "Weather", year, fsep = "\\"))
  
  ## Verweis-Pfade
  state.reference <- "State.ini"
  param.reference <- "Param.ini"
  option.reference <- "Opt.ini"
  weather.reference <- file.path("Weather", year, list.files(file.path(project.location, project, "Weather", year, fsep = "\\")), fsep = "\\") 
  
  ## Ini-Files fuer Varianten
  
  reference.list <- NULL
  
  for(i in 1:nrow(variants)) {
    
    name <- NULL
    
    for(j in 1:ncol(variants)) {
      
      name <-     if(j == 1) {paste(names(variants)[j], variants[i,j], sep = "")} else
        {paste(name, paste(names(variants)[j], variants[i,j], sep = ""), sep = "_")}
      
    }
    
    state.path <- file.path("Ini", year, "State", paste0("State_", project, "_", name, ".ini"), fsep = "\\")
    param.path <- file.path("Ini", year, "Param", paste0("Param_", project, "_", name, ".ini"), fsep = "\\")
    option.path <- file.path("Ini", year, "Opt", paste0("Opt_", project, "_", name, ".ini"), fsep = "\\")

    path.ini <- file.path(project.location, project, paste0(project, "_", name, ".ini"), fsep = "\\")
    path.fn <- file.path(project.location, project, paste0(project, "_", name, ".fn"), fsep = "\\")
    
    variant.ini <- paste0("[TimeInit]\n",
                          "Startzeit=", start.time, "\n",
                          "Endzeit=", end.time, "\n",
                          "Timestep=", step, "\n\n",
                          "[FileNames]\n",
                          "StateIniFN=", state.path, "\n",
                          "ParamIniFN=", param.path, "\n",
                          "OptionsIniFN=", option.path, "\n",
                          "WeatherFileFN=", weather.reference, "\n\n")
    
    write.table(variant.ini, file = path.ini, quote = FALSE, row.names = FALSE, col.names = FALSE)
    write.table(paste0(project, "_", name, ".ini"), file = path.fn, quote = FALSE, row.names = FALSE, col.names = FALSE)
    
    state.path <- file.path(project.location, project, "Ini", year, "State", paste0("State_", project, "_", name, ".ini"), fsep = "\\")
    opt.path <- file.path(project.location, project, "Ini", year, "Opt", paste0("Opt_", project, "_", name, ".ini"), fsep = "\\") 
    param.path <- file.path(project.location, project, "Ini", year, "Param", paste0("Param_", project, "_", name, ".ini"), fsep = "\\")
    
    if(template.opt == FALSE) {write.table("", file = opt.path, quote = FALSE, row.names = FALSE, col.names = FALSE)} else
    {file.copy(template.opt, opt.path)}
    
    if(template.param == FALSE) {write.table("", file = param.path, quote = FALSE, row.names = FALSE, col.names = FALSE)} else
    {file.copy(template.param, param.path)}
    
    if(template.state == FALSE) {write.table(paste0("[SubPartitioningSimple1]\n", "NcLeafWinter=4.51", "\n", "NcStemWinter=4.01"), file = state.path, quote = FALSE, row.names = FALSE, col.names = FALSE)} else
    {file.copy(template.state, state.path)}
    
    reference.list <- c(reference.list, paste0(project, "_", name, ".ini"))
    
  }
  
  ## Zentrale fn. Datei
  reference.list <- paste(reference.list, collapse = "\n")
  write.table(reference.list, file = paste0(file.path(project.location, project, fsep = "\\"), "\\", project, "_", year, ".fn"), quote = FALSE, row.names = FALSE, col.names = FALSE)
  
}

##########################################################################################

InputToHUME <- function(date, input.value, input.variable, variants, trial, year, inifn.path, override = FALSE) {
  
  date <- as.numeric(as.Date(date)) + 25569
  input <- cbind(date, input.value)
  grid <- expand.grid(date = date, input.variable = input.variable)
  grid <- merge(grid, input)
  names(grid) <- c("section", "name", "value")
  
  ## Ordnerstruktur
  if(file.exists(file.path(inifn.path, trial, fsep = "\\"))) {print("Hinweis: Der Initialisierungspfad zu diesem Projekt existiert bereits")} else {dir.create(file.path(inifn.path, trial, fsep = "\\"))}
  if(file.exists(file.path(inifn.path, trial, year, fsep = "\\"))) {print("Hinweis: Der Initialisierungspfad zu diesem Projekt und year existiert bereits")} else {dir.create(file.path(inifn.path, trial, year, fsep = "\\"))}
  if(file.exists(file.path(inifn.path, trial, year, "State", fsep = "\\"))) {print("Hinweis: Der Initialisierungspfad zu diesem Projekt und Modelljahr und State existiert bereits")} else {dir.create(file.path(inifn.path, trial, year, "State", fsep = "\\"))}
  
  for(i in 1:nrow(variants)) {
    
    name <- NULL
    
    for(j in 1:ncol(variants)) {
      
      name <- if(j == 1) {paste(names(variants)[j], variants[i,j], sep = "")} else {paste(name, paste(names(variants)[j], variants[i,j], sep = ""), sep = "_")}
    
      path <- paste0(file.path(inifn.path, trial, year, fsep = "\\"), "\\", "IniFn\\", trial, "_", name, "_Input.ini")
    
      if(file.exists(path) & !override) { existing.input <- data.frame(readIniFile(filename = path, token = "=", commenttoken = ";", aslist = FALSE), stringsAsFactors = FALSE);
                            updated.input <- rbind(existing.input, grid);
                            writeIniFile(x = updated.input, file = path)}
    
      if(!file.exists(path) | override) {writeIniFile(x = grid, file = path)}
    
    }}

}

##########################################################################################

StateToHUME <- function(date, input.value, input.variable, variants, project, year, project.location) {
  
  ## Checks
  if(length(date) != length(input.value)) {stop("Error: Vectors for date and input.value don't have the same length!")}
  
  ## Aufspannen der Varianten
  variants <- data.frame(expand.grid(variants, stringsAsFactors = FALSE))
  
  date <- strftime(as.Date(date), format = "%d.%m.%Y")
  input <- cbind(date, input.value)
  grid <- expand.grid(input.variable = input.variable, date = date)
  grid <- merge(grid, input)
  names(grid) <- c("name", "section", "value")
  grid <- data.frame(section = grid$section, name = grid$name, value = grid$value)
  date.input <- data.frame(date, input.value)
  
  for(i in 1:nrow(variants)) {
    
    name <- NULL
    
    for(j in 1:ncol(variants)) {
      
      name <- if(j == 1) {paste(names(variants)[j], variants[i,j], sep = "")} else {paste(name, paste(names(variants)[j], variants[i,j], sep = ""), sep = "_")}
      
      path <- paste0(file.path(project.location, project, fsep = "\\"), "\\Ini\\", year, "\\State\\", "State_", project, "_", name, ".ini")
      
      existing.input <- data.frame(readIniFile(filename = path, token = "=", commenttoken = ";", aslist = FALSE), stringsAsFactors = FALSE);
        
      for(date.act in date) {
          
          if(nrow(existing.input[existing.input$section == input.variable & existing.input$name == date.act,]) > 0) {existing.input$value[existing.input$section == input.variable & existing.input$name == date.act] <- date.input$input.value[date.input$date == date.act]}
          if(nrow(existing.input[existing.input$section == input.variable & existing.input$name == date.act,]) == 0) {existing.input <- rbind(existing.input, grid[grid$name == date.act,])}
      }
      
      writeIniFile(x = existing.input, file = path)}
      
    }
}

##########################################################################################


MeasurementToHUME <- function(data, measurement.variable, date.variable, variants, project, year, project.location) {
  
  data <- as.data.frame(data)
  data[is.na(data)] <- 0
  
  start.time <- as.numeric(as.Date(start)) + 25569
  end.time <- as.numeric(as.Date(end)) + 25569
  submodel <- unname(submodel_lookup[names(measurement.variable)])
  
  ## Aufspannen der Varianten
  variants <- data.frame(expand.grid(variants, stringsAsFactors = FALSE))
  
  for(i in 1:nrow(variants)) {
    
    name <- NULL
    
    for(j in 1:ncol(variants)) {
      
      data_ <-    if(j == 1) {with(data, data[get(colnames(variants)[j]) == as.character(variants[i, j]),])} else
        {with(data_, data_[get(colnames(variants)[j]) == as.character(variants[i, j]),])}
      
      name <-     if(j == 1) {paste(names(variants)[j], variants[i,j], sep = "")} else
        {paste(name, paste(names(variants)[j], variants[i,j], sep = ""), sep = "_")}
      
    }
    
    measurement.data <- data.frame(data_[, unname(measurement.variable)])
    names(measurement.data) <- names(measurement.variable)
    time.data <- data.frame(Time = data_[, unname(date.variable)])
    measurement.data <- cbind(time.data, measurement.data)
    
    units <- data.frame(matrix(unname(unit_lookup[names(measurement.data)]), ncol = 2), stringsAsFactors = FALSE)
    names(units) <- names(measurement.data)
    measurement.data <- rbind(units, measurement.data)
    
    measurement.path <-  file.path(project.location, project, "Measurements", year, paste0(project, "_", name, "_", submodel, ".txt"), fsep = "\\") 
    measurement.string <- file.path("Measurements", year, paste0(project, "_", name, "_", submodel, ".txt"), fsep = "\\")
    ini.path  <- file.path(project.location, project, paste0(project, "_", name, ".ini"), fsep = "\\") 
    
    new.line <- data.frame(section = "MeasurementFiles", name = submodel, value = measurement.string) 
    existing.ini <- data.frame(readIniFile(filename = ini.path, token = "=", commenttoken = ";", aslist = FALSE), stringsAsFactors = FALSE)
    
    if(!any(existing.ini$section == "MeasurementFiles" & existing.ini$name == submodel & existing.ini$value == measurement.string)) {
      new.ini <- rbind(existing.ini, new.line);
      writeIniFile(new.ini, ini.path)
    }
    
    
    if(file.exists(measurement.path)) { existing.data <- read.table(measurement.path, header = TRUE, stringsAsFactors = FALSE);
    existing.data <- as.data.frame(existing.data);
    new.data <- merge(existing.data, measurement.data, by = "Time", all = TRUE);
    if(any(names(new.data) == paste0(names(measurement.data)[-1], ".x"))) {new.data[,names(new.data) == paste0(names(measurement.data)[-1], ".x")] <- NULL};
    names(new.data)[names(new.data) == paste0(names(measurement.data)[-1], ".y")] <- names(measurement.data)[-1];
    new.data[is.na(new.data)] <- 0
    write.table(new.data, file = measurement.path, quote = FALSE, row.names = FALSE)
    } else {write.table(measurement.data, file = measurement.path, quote = FALSE, row.names = FALSE)}
    
  }
  
}

##########################################################################################


## All Credits to Wiebke Weymann

writeIniFile <- function(x, file = ""){   #arg: needs matrix!! of Form 
  submodels <- levels(as.factor(x[,1]))
  print(length(submodels))
  for(i in 1:length(submodels)){
    print(submodels[i])
    if(i == 1){
      cat(paste("[",submodels[i],"]", sep = ""),eol = "\n", file = file)
    }else{
      cat("\n", file = file, append = TRUE)
      cat(paste("[",submodels[i],"]", sep = ""),eol = "\n", file = file, append = TRUE)
    }
    write.table(paste(x[x[ ,1] == submodels[i] , 2], 
                      x[x[ ,1] == submodels[i] , 3], sep = "="),
                file = file, sep = "", 
                row.names = FALSE, col.names = FALSE, quote = FALSE, append = TRUE)
  }
} # WriteIniFile

##########################################################################################

