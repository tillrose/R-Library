

if (!require("pacman")) install.packages("pacman")
pacman::p_load(raster)

#library(tidyverse)
#library(lubridate)

library(raster)


################################################################################

Basispfad.Daten <- "Q:\\WHEAT\\Data"
Basispfad.Initialisierung <- "Q:\\WHEAT\\INI"
Datei.Wetter <- "Q:\\WEATHER\\HS\\Wetter_HS_14_15.txt"
Standort <- "Hohenschulen"
Versuchsbezeichnung <- 138
Modelljahr <- 2015
start <- "2014-10-02"
end <- "2015-08-14"
step <- 1
Varianten <- list(Sorte = c("Hybery", "Brilliant", "Norin", "Piko", "Toras", "Dekan", "Sur.99820", "Anapolis", "Solehio"))

################################################################################


start <- as.numeric(ymd(Startzeit)) + 25569
end <- as.numeric(ymd(Endzeit)) + 25569
Versuch <- paste0("V", Versuchsbezeichnung)

Varianten <- data.frame(expand.grid(Varianten, stringsAsFactors = FALSE))


template.opt = "D:\\Wheat\\WheatIrri\\Opt_838_1.ini"
template.param = "D:\\Wheat\\WheatIrri\\Param838_1.ini"
template.state ="D:\\Wheat\\WheatIrri\\State838_1.ini"


location <- "Q:\\WHEAT\\"
project <- "V138_HS"
year <- 2015
weather.source <- "Q:\\WEATHER\\HS\\Wetter_HS_14_15.txt"


InitializeHume <- function(start, end, step = 1, project, year, variants, location, weather.source, write.Ini = FALSE, template.opt = FALSE, template.param = FALSE, template.state = FALSE) {
  
  ## ?ndern des Datums von y-m-d in Excel-Nummer
  start.time <- as.numeric(as.Date(start)) + 25569
  end.time <- as.numeric(as.Date(end)) + 25569
  
  ## Aufspannen der Varianten
  variants <- data.frame(expand.grid(variants, stringsAsFactors = FALSE))

  ## Ordnerstruktur
  if(file.exists(file.path(location, project, fsep = "\\"))) {print("Hinweis: Der Projektpfad existiert bereits")} else {dir.create(file.path(location, project, fsep = "\\"))}
  if(file.exists(file.path(location, project, "Weather", fsep = "\\"))) {print("Hinweis: Der Wetterpfad zu diesem Projekt existiert bereits")} else {dir.create(file.path(location, project, "Weather", fsep = "\\"))}
  if(file.exists(file.path(location, project, "Weather", year, fsep = "\\"))) {print("Hinweis: Das Jahr im Wetterpfad zu diesem Projekt existiert bereits")} else {dir.create(file.path(location, project, "Weather", year, fsep = "\\"))}
  if(file.exists(file.path(location, project, "Data", fsep = "\\"))) {print("Hinweis: Der Datenpfad zu diesem Projekt existiert bereits")} else {dir.create(file.path(location, project, "Data", fsep = "\\"))}
  if(file.exists(file.path(location, project, "Data", year, fsep = "\\"))) {print("Hinweis: Das Jahr im Datenpfad zu diesem Projekt existiert bereits")} else {dir.create(file.path(location, project, "Data", year, fsep = "\\"))}
  if(file.exists(file.path(location, project, "Ini", fsep = "\\"))) {print("Hinweis: Der Inipfad zu diesem Projekt existiert bereits")} else {dir.create(file.path(location, project, "Ini", fsep = "\\"))}
  if(file.exists(file.path(location, project, "Ini", year, fsep = "\\"))) {print("Hinweis: Das Jahr im Inipfad zu diesem Projekt existiert bereits")} else {dir.create(file.path(location, project, "Ini", year, fsep = "\\"))}
  if(file.exists(file.path(location, project, "Model", fsep = "\\"))) {print("Hinweis: Der Modelpfad zu diesem Projekt existiert bereits")} else {dir.create(file.path(location, project, "Model", fsep = "\\"))}
  if(file.exists(file.path(location, project, "Ini", year, "IniFn", fsep = "\\"))) {print("Hinweis: Der IniFn-pfad zu diesem Projekt existiert bereits")} else {dir.create(file.path(location, project, "Ini", year, "IniFn", fsep = "\\"))}
  if(file.exists(file.path(location, project, "Ini", year, "Opt", fsep = "\\"))) {print("Hinweis: Der Opt-pfad zu diesem Projekt existiert bereits")} else {dir.create(file.path(location, project, "Ini", year, "Opt", fsep = "\\"))}
  if(file.exists(file.path(location, project, "Ini", year, "Param", fsep = "\\"))) {print("Hinweis: Der Param-pfad zu diesem Projekt existiert bereits")} else {dir.create(file.path(location, project, "Ini", year, "Param", fsep = "\\"))}
  if(file.exists(file.path(location, project, "Ini", year, "State", fsep = "\\"))) {print("Hinweis: Der State-pfad zu diesem Projekt existiert bereits")} else {dir.create(file.path(location, project, "Ini", year, "State", fsep = "\\"))}
  
  ## Wetterdaten in Zielordner kopieren
  file.copy(weather.source, file.path(location, project, "Weather", year, fsep = "\\"))
  
  ## Basis-Ini
  state.reference <- "State.ini"
  param.reference <- "Param.ini"
  option.reference <- "Opt.ini"
  weather.reference <- paste0("..\\..\\..\\Weather\\", year, "\\", list.files(file.path(location, project, "Weather", year, fsep = "\\"))) 
                              
  base.ini <- paste0("[TimeInit]\n",
                 "Startzeit=", start.time, "\n",
                 "Endzeit=", end.time, "\n",
                 "Timestep=", step, "\n\n",
                 "[FileNames]\n",
                 "StateIniFN=", state.reference, "\n",
                 "ParamIniFN=", param.reference, "\n",
                 "OptionsIniFN=", option.reference, "\n",
                 "WeatherFileFN=", weather.reference)
  
  
  base.ini.path <- paste0(file.path(location, project, "Ini", year, "IniFn", fsep = "\\"), "\\", project, "_", year, ".ini")
  write.table(base.ini, file = base.ini.path, quote = FALSE, row.names = FALSE, col.names = FALSE)
  
  ## Basis-Opt, -Param und -State
  
  base.opt.path <- paste0(file.path(location, project, "Ini", year, "IniFn", fsep = "\\"), "\\", "Options.ini")
  base.param.path <- paste0(file.path(location, project, "Ini", year, "IniFn", fsep = "\\"), "\\", "Param.ini")
  base.state.path <- paste0(file.path(location, project, "Ini", year, "IniFn", fsep = "\\"), "\\", "State.ini")
  
  if(template.opt == FALSE) {base.opt <- ""} else
    {file.copy(template.opt, base.opt.path)}
  
  if(template.param == FALSE) {base.param <- ""} else
    {file.copy(template.param, base.param.path)}
  
  if(template.state == FALSE) {base.state <- paste0("[SubPartitioningSimple1]\n", "NcLeafWinter=4.51", "\n", "NcStemWinter=4.01")} else
    {file.copy(template.state, base.state.path)}

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
    path <- paste0(file.path(location, project, "Ini", year, "IniFn", fsep = "\\"), "\\", project, "_", name, ".ini")
    path.fn <- paste0(file.path(location, project, "Ini", year, "IniFn", fsep = "\\"), "\\", project, "_", name, ".fn")
    
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
    write.table(paste0(".\\", project, "_", name, ".ini"), file = path.fn, quote = FALSE, row.names = FALSE, col.names = FALSE)
    write.table("", file = paste0(file.path(location, project, "Ini", year, "State", fsep = "\\"), "\\", "State_", project, "_", name, ".ini"), quote = FALSE, row.names = FALSE, col.names = FALSE)
    write.table("", file = paste0(file.path(location, project, "Ini", year, "Opt", fsep = "\\"), "\\", "Opt_", project, "_", name, ".ini"), quote = FALSE, row.names = FALSE, col.names = FALSE)
    write.table("", file = paste0(file.path(location, project, "Ini", year, "Param", fsep = "\\"), "\\", "Param_", project, "_", name, ".ini"), quote = FALSE, row.names = FALSE, col.names = FALSE)
    
    reference.list <- c(reference.list, paste0(".\\", project, "_", name, ".ini"))
    
  }
  
  ## Zentrale fn. Datei
  reference.list <- paste(reference.list, collapse = "\n")
  write.table(reference.list, file = paste0(file.path(location, project, "Ini", year, "IniFn", fsep = "\\"), "\\", project, "_", year, ".fn"), quote = FALSE, row.names = FALSE, col.names = FALSE)
  
}



























## Ordnerstruktur
if(file.exists(file.path(Basispfad.Daten, Versuch, fsep = "\\"))) {print("Hinweis: Der Datenpfad zu diesem Projekt existiert bereits")} else {dir.create(file.path(Basispfad.Daten, Versuch, fsep = "\\"))}
if(file.exists(file.path(Basispfad.Initialisierung, Versuch, fsep = "\\"))) {print("Hinweis: Der Initialisierungspfad zu diesem Projekt existiert bereits")} else {dir.create(file.path(Basispfad.Initialisierung, Versuch, fsep = "\\"))}
if(file.exists(file.path(Basispfad.Daten, Versuch, Modelljahr, fsep = "\\"))) {print("Hinweis: Der Datenpfad zu diesem Projekt und Modelljahr existiert bereits")} else {dir.create(file.path(Basispfad.Daten, Versuch, Modelljahr, fsep = "\\"))}
if(file.exists(file.path(Basispfad.Initialisierung, Versuch, Modelljahr, fsep = "\\"))) {print("Hinweis: Der Initialisierungspfad zu diesem Projekt und Modelljahr existiert bereits")} else {dir.create(file.path(Basispfad.Initialisierung, Versuch, Modelljahr, fsep = "\\"))}
if(file.exists(file.path(Basispfad.Initialisierung, Versuch, Modelljahr, "Opt", fsep = "\\"))) {print("Hinweis: Der Initialisierungspfad zu diesem Projekt und Modelljahr und Opt existiert bereits")} else {dir.create(file.path(Basispfad.Initialisierung, Versuch, Modelljahr, "Opt", fsep = "\\"))}
if(file.exists(file.path(Basispfad.Initialisierung, Versuch, Modelljahr, "Param", fsep = "\\"))) {print("Hinweis: Der Initialisierungspfad zu diesem Projekt und Modelljahr und Param existiert bereits")} else {dir.create(file.path(Basispfad.Initialisierung, Versuch, Modelljahr, "Param", fsep = "\\"))}
if(file.exists(file.path(Basispfad.Initialisierung, Versuch, Modelljahr, "State", fsep = "\\"))) {print("Hinweis: Der Initialisierungspfad zu diesem Projekt und Modelljahr und State existiert bereits")} else {dir.create(file.path(Basispfad.Initialisierung, Versuch, Modelljahr, "State", fsep = "\\"))}


## Opt Dateien

VariantString <- function(variants, trial) {
  
  string <- NULL
  
  for(i in 1:nrow(variants)) {
  
  name <- NULL
  
  for(j in 1:ncol(variants)) {
    
    name <-     if(j == 1) {paste(names(variants)[j], variants[i,j], sep = "")}
    else {paste(name, paste(names(variants)[j], variants[i,j], sep = ""), sep = "_")}
    
  }
  
  string_ <- paste0(trial, "_", name)
  string <- c(string, string_)
  }
  return(string)
}

VariantString(variants = Varianten, trial = Versuch)


