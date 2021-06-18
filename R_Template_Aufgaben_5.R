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
train <- as.data.frame(data[size,c("Purchase")])
test <- as.data.frame(data[-size,c("Purchase")])

#------------------------------------
# Aufgabe 2: Logistische Regression
#------------------------------------
glm.fit <- glm(
  data    = train,
  formula = y ~ .,
  family  = "binomial"
)

summary(glm.fit)

# Cut-Off-Point t=0.5:
# Steigung = -a1/a2
# y-Achsenabschnitt = -a0/a2
steigung         <-  - glm.fit$coef["x1"] / glm.fit$coef["x2"]  
yAchsenAbschnitt <-  - glm.fit$coef["(Intercept)"] / glm.fit$coef["x2"]

# Plot 
plot(data1, col="red",ylim=c(-100,150))
points(data2, col="green")
abline(a=yAchsenAbschnitt, b=steigung, col="blue")

# Plot (nur Testdaten)
plot(subset(test,y==0,select=x1:x2), col="red",ylim=c(-100,150))
points(subset(test,y==1,select=x1:x2), col="green")
abline(a=yAchsenAbschnitt, b=steigung, col="blue")


## Confusion-Matrix (Testdaten)
test.glm.predictedProbabilities <- predict( 
  object  = glm.fit, 
  newdata = data.test, 
  type    = "response"
)

test.glm.prediction      <- ifelse(test.glm.predictedProbabilities < 0.5, 0, 1)  
test.glm.confusionMatrix <- table(test.glm.prediction,data.test$y)
test.glm.confusionMatrix

## Confusion-Matrix (Trainingsdaten)
train.glm.predictedProbabilities <- predict( 
  object  = glm.fit, 
  newdata = data.train, 
  type    = "response"
)

train.glm.prediction <- ifelse(train.glm.predictedProbabilities < 0.5, 0, 1)  
train.glm.confusionMatrix <- table(train.glm.prediction,data.train$y)
train.glm.confusionMatrix


#------------------------------------
# Aufgabe 3: KNN
#------------------------------------

#    ...Platz für Ihren Code...

#------------------------------------
# Aufgabe 4: Modellauswahl
#------------------------------------

#    ...Platz für Ihren Code, Auswahl und Begründung...