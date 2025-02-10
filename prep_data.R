# Daten laden und als titanic speichern
titanic <- read.csv("~/Downloads/titanic.csv")

# Anreden
  
  #Anrede extrahieren
  titanic$Title <- sub(".*, (.*?)\\..*", "\\1", titanic$Name)
  
  
  # gleiche Anreden durch ein Wort ersetzten
  titanic$Title <- gsub("Mlle|Ms|Miss", "Miss", titanic$Title)
  titanic$Title <- gsub("Mme", "Mrs", titanic$Title)
  titanic$Title <- gsub("Master", "Master", titanic$Title)

  
table(titanic$Title)
# hat tatsächlich noch komische Einträge, Don: spanischer Ehrentitel, Jonkheer: niederländischer Adelstitel, Rev: irgendwas mit Kirche, Col: Colonel also Militär
  

# Survived, Sex, Embarked und Pclass in Faktoren umwandeln

titanic$Survived <- factor(titanic$Survived, levels = c(0, 1), labels = c("No", "Yes"))
str(titanic$Survived)

titanic$Sex <- factor(titanic$Sex)
str(titanic$Sex)

titanic$Embarked <- factor(titanic$Embarked, levels = c("C", "Q", "S"))
str(titanic$Embarked)

titanic$Pclass <- factor(titanic$Pclass, levels = c(1, 2, 3), labels = c(3, 2, 1), ordered = TRUE)
str(titanic$Pclass)

# Fehlende Altersangaben extrapolieren
missing <- is.na(titanic$Age)

means <- tapply(titanic$Age[!missing], titanic$Title[!missing], mean)

titanic$Age[missing] <- round(unname(means[match(titanic$Title, names(means))][missing]))

# Daten aus Cabin extrahieren
# initalisiere Spalten:
titanic$Bord <- NA
titanic$Deck <- NA

for(i in 1:nrow(titanic)){
  dat = titanic$Cabin[i]
  if(dat != ""){
    dat = strsplit(dat, " ")[[1]]
    titanic$Deck[i] <- paste(unique(gsub("[^A-Z]", "", dat)), collapse = ",")
    number <- na.omit(as.numeric(gsub("[^0-9 ]", "", dat)))
    if(all(number %% 2))
      titanic$Bord[i] <- "S"
    else
      titanic$Bord[i] <- "B"
  }
}

# Entferne die Spalten "PassengerID", "Name", "Ticket" und "Cabin" aus dem Datensatz
# (erst zum Schluss)
titanic <- titanic[, !(names(titanic) %in% c("PassengerID", "Name", "Ticket", "Cabin"))]

# Datensatz speichern

write.csv(titanic, "~/Downloads/titanic_prep.csv", row.names = FALSE)

