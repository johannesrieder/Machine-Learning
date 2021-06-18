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

set.seed(43)
rows.train <-
  sample(length(data$Purchase), 0.8 * length(data$Purchase))

train         <- as.data.frame(data[rows.train,])
test          <- as.data.frame(data[-rows.train,])

matrices <- list()

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
  glm_pred      <- ifelse(glm_predprob <= p, 0, 1)
  lr_confusionmatrix = table(glm_pred, test$Purchase)
  matrices[[length(matrices)+1]] <- list(paste("lr_confusionmatrix_t", p, sep = ""), lr_confusionmatrix)
  print(paste("Confusion Matrix ( t =", p, ")"))
  print(lr_confusionmatrix)
  
}

#------------------------------------
# Aufgabe 3: KNN
#------------------------------------

# Skalierung notwendig, da 85 unterschiedliche Input-Variablen
# Input-Variablen ohne Output-Variable
invar <- seq(0, length(train) - 1, 1)

# Purchase wird von Trainingsdaten entfernt
knn_train <- train[, invar]

# Purchase wird von Testdaten entfernt
knn_test  <- test[, invar]

# Label der Trainingsdaten für Purchase-Klassifikation als Vektor
train_label <- train[['Purchase']]

# Label der Testdaten für Purchase-Klassifikation als Vektor
test_label  <- test[['Purchase']]

# Drei verschiedene K für knn-Funktion
k <- c(1, 3, 5)

# Anwendung knn-Funktion mit drei unterschiedlichen k
for (i in k) {
  knn <- knn(
    train = scale(knn_train),
    test = scale(knn_test),
    cl = train_label,
    k = i,
    prob = TRUE
  )
  # Erzeugung und Ausgabe der Confusion Matrix
  knn_confusionmatrix = table(knn, test_label)
  matrices[[length(matrices)+1]] <- list(paste("knn_confusionmatrix_k", i, sep = ""), lr_confusionmatrix)
  print(paste("Confusion Matrix ( k =", i, ")"))
  print(knn_confusionmatrix)
}


#------------------------------------
# Aufgabe 4: Modellauswahl
#------------------------------------

# Summary der Confusion Matrices

for (matrix in matrices) {
  m = matrix[[2]]
  
  tn = m[1]
  fp = m[2]
  fn = m[3]
  tp = m[4]

  print("==============================================================")
  print(matrix[[1]])
  print("--------------------------------------------------------------")
  print(paste("Positives / Negatives: ", tp+fn, "/", fp+tn))
  prec_t <- (tp+tn)/(tp+fp+fn+tn)
  print(paste("Average Precision: ", prec_t))
  prec_tp <- tp/(tp+fn)
  print(paste("Precision (Positives): ", prec_tp))
  prec_tn <- tn/(fp+tn)
  print(paste("Precision (Negatives): ", prec_tn))
  print("--------------------------------------------------------------")
  print(paste("TN (1,1) / FP (1,2) / FN (2,1) / TP (2,2): ", tn, "/", fp, "/", fn, "/", tp))
  print(m)
}

#    ...Platz für Ihren Code, Auswahl und Begründung...