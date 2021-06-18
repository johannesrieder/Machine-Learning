#
# Template für die Bearbeitung der Übungsaufgaben 5
#        -- Analyse des Caravan-Datensatz -- 
#

#-----------------
# Bitte ausfüllen:
#-----------------
# Gruppenname: Obi-Wan
# Gruppenteilnehmer: Johannes Rieder, Jannik Steck, Lisa Trovato-Monastra, Marius Bauer


#-------------------------
# notwendinge Bibliotheken
#-------------------------
library(ISLR)   # Datensatz
library(class)  # knn Funktion


#------------------------------------
# Aufgabe 1: Trainings- und Testdaten
#------------------------------------
data <- ISLR::Caravan

set.seed(42)
rows.train <- sample(length(data$Purchase),0.8*length(data$Purchase))

train         <- as.data.frame(data[rows.train,])
test          <- as.data.frame(data[-rows.train,])

#------------------------------------
# Aufgabe 2: Logistische Regression
#------------------------------------
#Logistische Regression mit Output-Variable Purchase und den anderen Variablen als Input-Variablen
glm.fit <- glm(
  data    = train,
  formula = Purchase ~ .,
  family  = "binomial"
)

summary(glm.fit)

# Drei verschiedene Cut-Off Points
t <- c(0.15, 0.35, 0.55)


## Vorhersage der Wahrscheinlichkeiten der Testdaten auf Basis der logistischen Regression
test.glm.predictedProbabilities <- predict( 
  object  = glm.fit, 
  newdata = test, 
  type    = "response"
)

# For-Schleife erzeugt Confusion-Matrizen anhand der drei Cut-Off Points
for (p in t) {
  print(p)
  test.glm.prediction      <- ifelse(test.glm.predictedProbabilities <= p, 0, 1)  
  test.glm.confusionMatrix <- table(test.glm.prediction,test$Purchase)
  print(test.glm.confusionMatrix)
}

#------------------------------------
# Aufgabe 3: KNN
#------------------------------------

# Drei verschiedene K für KNN
train         <- as.data.frame(data[rows.train, seq(1, length(train)-1, 1)])
test          <- as.data.frame(data[-rows.train,])
train.classes <- data[rows.train, seq(1, length(train)-1, 1)]
test.classes  <- data[-rows.train, "Purchase"]
k <- c(1, 3, 5)

knn <- knn(
  train = train,         ## Input-Variablen der Trainingsdaten      
  test  = test,          ## Input-Variablen der Testdaten
  cl    = train.classes, ## Class-Label der Trainingsdaten
  k     = 3,
  prob = TRUE
) 

confusion.table.k3 <- table(knn, train.classes)


#------------------------------------
# Aufgabe 4: Modellauswahl
#------------------------------------

#    ...Platz für Ihren Code, Auswahl und Begründung...