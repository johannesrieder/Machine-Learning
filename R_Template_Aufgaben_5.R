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
summary(Caravan)
data <- ISLR::Caravan

set.seed(42)

size <- sample(length(data$Purchase),0.8*length(data$Purchase))
train <- data[size,]
test <- data[-size,]

train.classes <- data[size,]
test.classes <- data[-size,]

#------------------------------------
# Aufgabe 2: Logistische Regression
#------------------------------------
glm.fit <- glm(
  data    = train,
  formula = Purchase ~ .,
  family  = "binomial"
)

summary(glm.fit)

# cut-off-points
t <- c(0.4, 0.5, 0.6)


## Confusion-Matrix (test)
test.glm.predictedProbabilities <- predict( 
  object  = glm.fit, 
  newdata = test, 
  type    = "response"
)

## Confusion-Matrix (train)
train.glm.predictedProbabilities <- predict( 
  object  = glm.fit, 
  newdata = train, 
  type    = "response"
)

for (p in t) {
  print(p)
  # CM (test)
  test.glm.prediction      <- ifelse(test.glm.predictedProbabilities < p, 0, 1)  
  test.glm.confusionMatrix <- table(test.glm.prediction,test$Purchase)
  print(test.glm.confusionMatrix)
  
  # CM (train)
  # train.glm.prediction <- ifelse(train.glm.predictedProbabilities < p, 0, 1)  
  # train.glm.confusionMatrix <- table(train.glm.prediction,train$Purchase)
  # print(train.glm.confusionMatrix)
}


#------------------------------------
# Aufgabe 3: KNN
#------------------------------------

knn.pred.k3 <- knn(
  train = train,         ## Input-Variablen der Trainingsdaten      
  test  = test,          ## Input-Variablen der Testdaten
  cl    = train.classes, ## Class-Label der Trainingsdaten
  k     = 3,
  prob = FALSE
) 

#------------------------------------
# Aufgabe 4: Modellauswahl
#------------------------------------

#    ...Platz für Ihren Code, Auswahl und Begründung...