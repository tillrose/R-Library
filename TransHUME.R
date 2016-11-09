

## Install package "raster" if necessary and load it
if (!require("pacman")) install.packages("pacman")
pacman::p_load(raster)


MeasurementToHumeEx <- function(data, start, end, step = 1, variables, variants, trial, year, dat.path, inifn.path, weather.path, write.ini = FALSE) {
  
  data <- as.data.frame(data)
  data[is.na(data)] <- 0
  
  Startzeit <- as.numeric(as.Date(start)) + 25569
  Endzeit <- as.numeric(as.Date(end)) + 25569
  submodel <- unname(submodel_lookup[names(variables)[2]])
  
  ## Ordnerstruktur
  if(file.exists(file.path(dat.path, trial, fsep = "\\"))) {print("Hinweis: Der Datenpfad zu diesem Projekt existiert bereits")} else {dir.create(file.path(dat.path, trial, fsep = "\\"))}
  if(file.exists(file.path(inifn.path, trial, fsep = "\\"))) {print("Hinweis: Der Initialisierungspfad zu diesem Projekt existiert bereits")} else {dir.create(file.path(inifn.path, trial, fsep = "\\"))}
  if(file.exists(file.path(dat.path, trial, year, fsep = "\\"))) {print("Hinweis: Der Datenpfad zu diesem Projekt und Modelljahr existiert bereits")} else {dir.create(file.path(dat.path, trial, year, fsep = "\\"))}
  if(file.exists(file.path(inifn.path, trial, year, fsep = "\\"))) {print("Hinweis: Der Initialisierungspfad zu diesem Projekt und year existiert bereits")} else {dir.create(file.path(inifn.path, trial, year, fsep = "\\"))}
  if(file.exists(file.path(inifn.path, trial, year, "Opt", fsep = "\\"))) {print("Hinweis: Der Initialisierungspfad zu diesem Projekt und Modelljahr und Opt existiert bereits")} else {dir.create(file.path(inifn.path, trial, year, "Opt", fsep = "\\"))}
  if(file.exists(file.path(inifn.path, trial, year, "Param", fsep = "\\"))) {print("Hinweis: Der Initialisierungspfad zu diesem Projekt und Modelljahr und Param existiert bereits")} else {dir.create(file.path(inifn.path, trial, year, "Param", fsep = "\\"))}
  if(file.exists(file.path(inifn.path, trial, year, "State", fsep = "\\"))) {print("Hinweis: Der Initialisierungspfad zu diesem Projekt und Modelljahr und State existiert bereits")} else {dir.create(file.path(inifn.path, trial, year, "State", fsep = "\\"))}
  if(file.exists(file.path(inifn.path, trial, year, "IniFn", fsep = "\\"))) {print("Hinweis: Der Initialisierungspfad zu diesem Projekt und Modelljahr und State existiert bereits")} else {dir.create(file.path(inifn.path, trial, year, "IniFn", fsep = "\\"))}
  
  ## Basis-Ini
  state.path <- paste0(file.path(inifn.path, trial, year, fsep = "\\"), "\\State.ini")
  param.path <- paste0(file.path(inifn.path, trial, year, fsep = "\\"), "\\Param.ini")
  option.path <-paste0(file.path(inifn.path, trial, year, fsep = "\\"), "\\Opt.ini")
  
  text <- paste0("[TimeInit]\n",
                 "Startzeit=", Startzeit, "\n",
                 "Endzeit=", Endzeit, "\n",
                 "Timestep=", step, "\n\n",
                 "[FileNames]\n",
                 "StateIniFN=", state.path, "\n",
                 "ParamIniFN=", param.path, "\n",
                 "OptionsIniFN=", option.path, "\n",
                 "WeatherFileFN=", weather.path
                )
  
  path <- paste0(file.path(inifn.path, trial, year, fsep = "\\"), "\\IniFn\\", trial, "_", year, ".ini")
  write.table(text, file = path, quote = FALSE, row.names = FALSE, col.names = FALSE)
  path.list <- NULL
  
  for(i in 1:nrow(variants)) {
    
    name <- NULL
    
    for(j in 1:ncol(variants)) {
      
      data_ <-    if(j == 1) {with(data, data[get(colnames(variants)[j]) == as.character(variants[i, j]),])}
      else {with(data_, data_[get(colnames(variants)[j]) == as.character(variants[i, j]),])}
      
      name <-     if(j == 1) {paste(names(variants)[j], variants[i,j], sep = "")}
      else {paste(name, paste(names(variants)[j], variants[i,j], sep = ""), sep = "_")}
      
    }
    
    data_ <- data_[, unname(variables)]
    names(data_) <- names(variables)
    units <- data.frame(matrix(unname(unit_lookup[names(data_)]), ncol = 2), stringsAsFactors = FALSE)
    names(units) <- names(variables)
    data_ <- rbind(units, data_)
    
    string <- paste0(file.path(dat.path, trial, year, fsep = "\\"), "\\", trial, "_", name, "_", submodel, ".txt")
    
    if(file.exists(string)) { existing.data <- read.table(string, header = TRUE, stringsAsFactors = FALSE);
                              existing.data <- as.data.frame(existing.data);
                              new.data <- merge(existing.data, data_, by = "Time");
                              new.data[,names(new.data) == paste0(names(data_)[-1], ".x")] <- NULL;
                              names(new.data)[names(new.data) == paste0(names(data_)[-1], ".y")] <- names(data_)[-1];
                              write.table(new.data, file = string, quote = FALSE, row.names = FALSE)
                             } else {write.table(data_, file = string, quote = FALSE, row.names = FALSE)}
    
    state.path <- paste0(file.path(inifn.path, trial, year, fsep = "\\"), "\\", "State", "\\", "State_", trial, "_", name, ".ini")
    param.path <- paste0(file.path(inifn.path, trial, year, fsep = "\\"), "\\", "Param", "\\", "Param_", trial, "_", name, ".ini")
    option.path <-paste0(file.path(inifn.path, trial, year, fsep = "\\"), "\\", "Opt", "\\", "Opt_", trial, "_", name, ".ini")
    path <- paste0(file.path(inifn.path, trial, year, fsep = "\\"), "\\", "IniFn\\", trial, "_", name, ".ini")
    path.fn <- paste0(file.path(inifn.path, trial, year, fsep = "\\"), "\\", "IniFn\\", trial, "_", name, ".fn")
    
    if(file.exists(path))
            { existing.Ini <- data.frame(readIniFile(filename = path, token = "=", commenttoken = ";", aslist = FALSE));
              if(!any(existing.Ini$section == "MeasurementFiles" & existing.Ini$name == submodel))
              { updated.Ini <- as.matrix(rbind(existing.Ini, data.frame(section = "MeasurementFiles", name = submodel, value = string)));
                writeIniFile(x = updated.Ini, file = path)}
            }
           
    if(!file.exists(path)) {
      text <- paste0("[TimeInit]\n",
                     "Startzeit=", Startzeit, "\n",
                     "Endzeit=", Endzeit, "\n",
                     "Timestep=", step, "\n\n",
                     "[FileNames]\n",
                     "StateIniFN=", state.path, "\n",
                     "ParamIniFN=", param.path, "\n",
                     "OptionsIniFN=", option.path, "\n",
                     "WeatherFileFN=", weather.path, "\n\n",
                     "[MeasurementFiles]\n",
                     submodel, "=", string
      );
      write.table(text, file = path, quote = FALSE, row.names = FALSE, col.names = FALSE)
    }
    
    write.table(as.character(path), file = path.fn, quote = FALSE, row.names = FALSE, col.names = FALSE)
    write.table("", file = paste0(file.path(inifn.path, trial, year, fsep = "\\"), "\\", "IniFn\\", "Options.ini"), quote = FALSE, row.names = FALSE, col.names = FALSE)
    write.table("", file = paste0(file.path(inifn.path, trial, year, fsep = "\\"), "\\", "IniFn\\", "Param.ini"), quote = FALSE, row.names = FALSE, col.names = FALSE)
    write.table("", file = paste0(file.path(inifn.path, trial, year, fsep = "\\"), "\\", "IniFn\\", "State.ini"), quote = FALSE, row.names = FALSE, col.names = FALSE)
    
    text.state <- paste0("[SubPartitioningSimple1]\n",
                         "NcLeafWinter=4.51", "\n",
                         "NcStemWinter=4.01"
    )
    
    write.table("", file = paste0(file.path(inifn.path, trial, year, fsep = "\\"), "\\Opt\\", "Opt_", trial, "_", name, ".ini"), quote = FALSE, row.names = FALSE, col.names = FALSE)
    write.table("", file = paste0(file.path(inifn.path, trial, year, fsep = "\\"), "\\Param\\", "Param_", trial, "_", name, ".ini"), quote = FALSE, row.names = FALSE, col.names = FALSE)
    write.table(text.state, file = paste0(file.path(inifn.path, trial, year, fsep = "\\"), "\\State\\", "State_", trial, "_", name, ".ini"), quote = FALSE, row.names = FALSE, col.names = FALSE)
    
    path.list <- c(path.list, path)
    
  }

  path.list <- paste(path.list, collapse = "\n")
  write.table(path.list, file = paste0(file.path(inifn.path, trial, year, fsep = "\\"), "\\IniFN\\", trial, ".fn"), quote = FALSE, row.names = FALSE, col.names = FALSE)
  
}





InputHume <- function(date, input.value, input.variable, variants, trial, year, inifn.path, override = FALSE) {
  
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


#################################################


StateToHume <- function(date, input.value, input.variable, variants, project, year, project.location) {
  
  ## Checks
  if(length(date) != length(input.value)) {stop("Error: Vectors for date and input.value don't have the same length!")}
  
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

########################################################################

InitializeHume <- function(start, end, step = 1, project, year, variants, project.location, weather.source, template.opt = FALSE, template.param = FALSE, template.state = FALSE) {
  
  ## ?ndern des Datums von y-m-d in Excel-Nummer
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
  if(file.exists(file.path(project.location, project, "Ini", year, "IniFn", fsep = "\\"))) {print("Hinweis: Der IniFn-pfad zu diesem Projekt existiert bereits")} else {dir.create(file.path(project.location, project, "Ini", year, "IniFn", fsep = "\\"))}
  if(file.exists(file.path(project.location, project, "Ini", year, "Opt", fsep = "\\"))) {print("Hinweis: Der Opt-pfad zu diesem Projekt existiert bereits")} else {dir.create(file.path(project.location, project, "Ini", year, "Opt", fsep = "\\"))}
  if(file.exists(file.path(project.location, project, "Ini", year, "Param", fsep = "\\"))) {print("Hinweis: Der Param-pfad zu diesem Projekt existiert bereits")} else {dir.create(file.path(project.location, project, "Ini", year, "Param", fsep = "\\"))}
  if(file.exists(file.path(project.location, project, "Ini", year, "State", fsep = "\\"))) {print("Hinweis: Der State-pfad zu diesem Projekt existiert bereits")} else {dir.create(file.path(project.location, project, "Ini", year, "State", fsep = "\\"))}
  
  ## Wetterdaten in Zielordner kopieren
  file.copy(weather.source, file.path(project.location, project, "Weather", year, fsep = "\\"))
  
  ## Basis-Ini
  state.reference <- "State.ini"
  param.reference <- "Param.ini"
  option.reference <- "Opt.ini"
  weather.reference <- paste0("..\\..\\..\\Weather\\", year, "\\", list.files(file.path(project.location, project, "Weather", year, fsep = "\\"))) 
  
  #base.ini <- paste0("[TimeInit]\n",
  #                   "Startzeit=", start.time, "\n",
  #                   "Endzeit=", end.time, "\n",
  #                   "Timestep=", step, "\n\n",
  #                   "[FileNames]\n",
  #                   "StateIniFN=", state.reference, "\n",
  #                   "ParamIniFN=", param.reference, "\n",
  #                   "OptionsIniFN=", option.reference, "\n",
  #                   "WeatherFileFN=", weather.reference)
  
  
  #base.ini.path <- paste0(file.path(project.location, project, "Ini", year, "IniFn", fsep = "\\"), "\\", project, "_", year, ".ini")
  #write.table(base.ini, file = base.ini.path, quote = FALSE, row.names = FALSE, col.names = FALSE)
  
  ## Basis-Opt, -Param und -State
  
  #base.opt.path <- paste0(file.path(project.location, project, "Ini", year, "IniFn", fsep = "\\"), "\\", "Options.ini")
  #base.param.path <- paste0(file.path(project.location, project, "Ini", year, "IniFn", fsep = "\\"), "\\", "Param.ini")
  #base.state.path <- paste0(file.path(project.location, project, "Ini", year, "IniFn", fsep = "\\"), "\\", "State.ini")
  
  #if(template.opt == FALSE) {base.opt <- ""} else
  #{file.copy(template.opt, base.opt.path)}
  
  #if(template.param == FALSE) {base.param <- ""} else
  #{file.copy(template.param, base.param.path)}
  
  #if(template.state == FALSE) {base.state <- paste0("[SubPartitioningSimple1]\n", "NcLeafWinter=4.51", "\n", "NcStemWinter=4.01")} else
  #{file.copy(template.state, base.state.path)}
  
  ## Ini-Files f?r Varianten
  
  reference.list <- NULL
  
  for(i in 1:nrow(variants)) {
    
    name <- NULL
    
    for(j in 1:ncol(variants)) {
      
      name <-     if(j == 1) {paste(names(variants)[j], variants[i,j], sep = "")}
      else {paste(name, paste(names(variants)[j], variants[i,j], sep = ""), sep = "_")}
      
    }
    
    state.path <- paste0("..\\State\\", "State_", project, "_", name, ".ini")
    param.path <- paste0("..\\Param\\", "Param_", project, "_", name, ".ini")
    option.path <-paste0("..\\Opt\\", "Opt_", project, "_", name, ".ini")
    path <- paste0(file.path(project.location, project, "Ini", year, "IniFn", fsep = "\\"), "\\", project, "_", name, ".ini")
    path.fn <- paste0(file.path(project.location, project, "Ini", year, "IniFn", fsep = "\\"), "\\", project, "_", name, ".fn")
    
    variant.ini <- paste0("[TimeInit]\n",
                          "Startzeit=", start.time, "\n",
                          "Endzeit=", end.time, "\n",
                          "Timestep=", step, "\n\n",
                          "[FileNames]\n",
                          "StateIniFN=", state.path, "\n",
                          "ParamIniFN=", param.path, "\n",
                          "OptionsIniFN=", option.path, "\n",
                          "WeatherFileFN=", weather.reference, "\n\n")
    
    write.table(variant.ini, file = path, quote = FALSE, row.names = FALSE, col.names = FALSE)
    write.table(paste0(project, "_", name, ".ini"), file = path.fn, quote = FALSE, row.names = FALSE, col.names = FALSE)
    
    state.path <- paste0(file.path(project.location, project, "Ini", year, "State", fsep = "\\"), "\\", "State_", project, "_", name, ".ini")
    opt.path <- paste0(file.path(project.location, project, "Ini", year, "Opt", fsep = "\\"), "\\", "Opt_", project, "_", name, ".ini")
    param.path <- paste0(file.path(project.location, project, "Ini", year, "Param", fsep = "\\"), "\\", "Param_", project, "_", name, ".ini")
    
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
  write.table(reference.list, file = paste0(file.path(project.location, project, "Ini", year, "IniFn", fsep = "\\"), "\\", project, "_", year, ".fn"), quote = FALSE, row.names = FALSE, col.names = FALSE)
  
}

##########################################################################################

MeasurementToHume <- function(data, measurement.variable, date.variable, variants, project, year, project.location) {
  
  data <- as.data.frame(data)
  data[is.na(data)] <- 0
  
  start.time <- as.numeric(as.Date(start)) + 25569
  end.time <- as.numeric(as.Date(end)) + 25569
  submodel <- unname(submodel_lookup[names(measurement.variable)])
  
  ## Aufspannen der Varianten
  variants <- data.frame(expand.grid(variants, stringsAsFactors = FALSE))
  
  #path.list <- NULL
  
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
    
    measurement.path <-  paste0(file.path(project.location, project, "Measurements", year, fsep = "\\"), "\\", project, "_", name, "_", submodel, ".txt")
    measurement.string <- paste0("..\\..\\..", "\\Measurements\\", year, "\\", project, "_", name, "_", submodel, ".txt")
    ini.path  <- paste0(file.path(project.location, project, "Ini", year, "IniFn", fsep = "\\"), "\\", project, "_", name, ".ini")
    
    new.line <- data.frame(section = "MeasurementFiles", name = submodel, value = measurement.string) 
    existing.ini <- data.frame(readIniFile(filename = ini.path, token = "=", commenttoken = ";", aslist = FALSE), stringsAsFactors = FALSE)
    
    if(!any(existing.ini$section == "MeasurementFiles" & existing.ini$name == submodel & existing.ini$value == measurement.string)) {
      new.ini <- rbind(existing.ini, new.line);
      writeIniFile(new.ini, ini.path)
    }
    
    
    if(file.exists(measurement.path)) { existing.data <- read.table(measurement.path, header = TRUE, stringsAsFactors = FALSE);
    existing.data <- as.data.frame(existing.data);
    new.data <- merge(existing.data, measurement.data, by = "Time");
    if(any(names(new.data) == paste0(names(measurement.data)[-1], ".x"))) {new.data[,names(new.data) == paste0(names(measurement.data)[-1], ".x")] <- NULL};
    names(new.data)[names(new.data) == paste0(names(measurement.data)[-1], ".y")] <- names(measurement.data)[-1];
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

## Lookup-Tables
unit_lookup <- c("EC" = "[]",
                 "Time" = "[d]",
                 "LAI" = "[m2/m2]",
                 "CumCarbo" = "[g/m2]",
                 "avSLA" = "[squarecm/g]",
                 "fStem" = "[-]")

submodel_lookup <- c("EC" = "Development1",
                     "LAI" = "SubLeafAreaSimple1",
                     "CumCarbo" = "SubDryMatterSimple1",
                     "avSLA" = "SubLeafAreaSimple1",
                     "fStem" = "SubPartitioningSimple1")



