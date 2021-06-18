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
rows.train <-
  sample(length(data$Purchase), 0.8 * length(data$Purchase))

train         <- as.data.frame(data[rows.train,])
test          <- as.data.frame(data[-rows.train,])

#------------------------------------
# Aufgabe 2: Logistische Regression
#------------------------------------
#Logistische Regression mit Output-Variable Purchase und den anderen Variablen als Input-Variablen
glm.fit <- glm(data    = train,
               formula = Purchase ~ .,
               family  = "binomial")

# Drei verschiedene Cut-Off Points
t <- c(0.15, 0.35, 0.55)

## Vorhersage der Wahrscheinlichkeiten der Testdaten auf Basis der logistischen Regression
glm_predprob <- predict(object  = glm.fit,
                        newdata = test,
                        type    = "response")

# For-Schleife erzeugt Confusion-Matrizen anhand der drei Cut-Off Points
for (p in t) {
  print(p)
  glm_pred      <- ifelse(glm_predprob <= p, 0, 1)
  lr_confusionmatrix <- table(glm_pred, test$Purchase)
  assign(paste("lr_confusionmatrix", p, sep = ""), lr_confusionmatrix)
  print(paste("Confusion Matrix (t =", p, ")"))
  print(lr_confusionmatrix)
}

#------------------------------------
# Aufgabe 3: KNN
#------------------------------------

# Skalierung notwendig, da 85 unterschiedliche Input-Variablen
# Purchase wird von Trainingsdaten entfernt
knn_train <-
  as.data.frame((data[rows.train, seq(0, length(train) - 1, 1)]))
# Purchase wird von Testdaten entfernt
knn_test  <-
  as.data.frame((data[-rows.train, seq(0, length(test) - 1, 1)]))
# Label der Trainingsdaten für Purchase-Klassifikation als Vektor
train_label <- train[['Purchase']]
# Label der Testdaten für Purchase-Klassifikation als Vektor
test_label  <- test[['Purchase']]

# Drei verschiedene K für knn-Funktion
k <- c(1, 3, 5)

# Anwendung knn-Funktion mit drei unterschiedlichen k
for (i in k) {
  knn <- knn(
    train = knn_train,
    test = knn_test,
    cl = train_label,
    k = i,
    prob = TRUE
  )
  # Erzeugung und Ausgabe der Confusion Matrix
  knn_confusionmatrix <- table(knn, test_label)
  assign(paste("knn_confusionmatrix", i, sep = ""),
         knn_confusionmatrix)
  print(paste("Confusion Matrix (k =", i, ")"))
  print(knn_confusionmatrix)
}


#------------------------------------
# Aufgabe 4: Modellauswahl
#------------------------------------

# Summary der Confusion Matrices


#    ...Platz für Ihren Code, Auswahl und Begründung...