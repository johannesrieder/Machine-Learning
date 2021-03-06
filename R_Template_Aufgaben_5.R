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
# notwendige Bibliotheken
#-------------------------
library(ISLR)   # Datensatz
library(class)  # knn Funktion

#------------------------------------
# Aufgabe 1: Trainings- und Testdaten
#------------------------------------
data <- ISLR::Caravan

set.seed(50)
# Einteilung des Caravan-Datensatzes in 80% Trainings- und 20% Testdaten
rows.train <- sample(length(data$Purchase), 0.8 * length(data$Purchase))
train         <- as.data.frame(data[rows.train,])
test          <- as.data.frame(data[-rows.train,])

# Initialisierung einer Matrizen-Liste für Aufgabe 4
matrices <- list()

#------------------------------------
# Aufgabe 2: Logistische Regression
#------------------------------------
#Logistische Regression mit Output-Variable Purchase und den anderen Variablen als Input-Variablen
glm.fit <- glm(data    = train,
               formula = Purchase ~ .,
               family  = "binomial")

# Drei verschiedene Cut-Off Points
cutoffpoints <- c(0.05, 0.15, 0.35)

## Vorhersage der Wahrscheinlichkeiten der Testdaten auf Basis der logistischen Regression
glm_predprob <- predict(object  = glm.fit,
                        newdata = test,
                        type    = "response")

# For-Schleife erzeugt Confusion-Matrizen anhand der drei zuvor festgelegten Cut-Off Points
for (p in cutoffpoints) {
  lr      <- ifelse(glm_predprob <= p, 0, 1)
  lr_confusionmatrix = table(lr, test$Purchase)
  matrices[[length(matrices)+1]] <- list(paste("lr_confusionmatrix_t", p, sep = ""), lr_confusionmatrix)
  print(paste("Confusion Matrix ( Cut-Off Point =", p, ")"))
  print(lr_confusionmatrix)
  }

#------------------------------------
# Aufgabe 3: KNN
#------------------------------------

# Input-Variablen ohne Output-Variable Purchase
invar <- seq(0, length(data) - 1, 1)

# Skalierung der Daten
scaled_data <- scale(data[, invar])

# Einteilung des skalierten Caravan-Datensatzes in 80% Trainings- und 20% Testdaten
scaled_train         <- as.data.frame(scaled_data[rows.train,])
scaled_test          <- as.data.frame(scaled_data[-rows.train,])

# Label der Trainingsdaten für Purchase-Klassifikation als Vektor
train_label <- as.data.frame(data[rows.train,])[['Purchase']]

# Label der Testdaten für Purchase-Klassifikation als Vektor
test_label  <- as.data.frame(data[-rows.train,])[['Purchase']]

# Drei verschiedene K für knn-Funktion
k <- c(1, 3, 5)

# Anwendung knn-Funktion mit drei unterschiedlichen k
# Skalierung notwendig, da 85 unterschiedliche Input-Variablen
for (i in k) {
  knn <- knn(
    train = scaled_train,
    test = scaled_test,
    cl = train_label,
    k = i,
    prob = FALSE
  )
  # Erzeugung und Ausgabe der Confusion Matrix
  knn_confusionmatrix = table(knn, test_label)
  matrices[[length(matrices)+1]] <- list(paste("knn_confusionmatrix_k", i, sep = ""), knn_confusionmatrix)
  print(paste("Confusion Matrix ( k =", i, ")"))
  print(knn_confusionmatrix)
}

#------------------------------------
# Aufgabe 4: Modellauswahl
#------------------------------------

# Summary der sechs Confusion Matrizen aus den Aufgaben 2 und 3
for (matrix in matrices) {
  m = matrix[[2]]
  # true negative Element der Matrix
  tn = m[1]
  # false positive Element der Matrix
  fp = m[3]
  # false negative Element der Matrix
  fn = m[2]
  # true positive Element der Matrix
  tp = m[4]

  print("==============================================================")
  print(matrix[[1]])
  print("--------------------------------------------------------------")
  print(paste("Positives / Negatives: ", tp+fn, "/", fp+tn))
  prec_a <- (tp+tn)/(tp+fp+fn+tn)
  print(paste("Average Precision: ", prec_a))
  recall <- tp/(fn+tp)
  print(paste("Precision (Positives) / Recall: ", recall))
  prec_n <- tn/(fp+tn)
  print(paste("Precision (Negatives): ", prec_n))
  prec <- tp/(fp+tp)
  f1 <- 2*((prec*recall)/(prec+recall))
  print(paste("F1-Score: ", f1))
  print("--------------------------------------------------------------")
  print(paste("TN (1,1) / FP (1,2) / FN (2,1) / TP (2,2): ", tn, "/", fp, "/", fn, "/", tp))
  print(m)
}

# Ziel ###

# Für die Versicherung sind nur Kunden interessant, die tatsächlich an einem 
# Vertrag interessiert sind, weswegen ausschließlich die positive-Spalte der
# Confusion Matrix zu betrachten ist. True-positives (korrekt als Interessent 
# klassifiziert) sind möglichst zu maximieren und false-positives (inkorrekt 
# als Interessent klassifiziert) möglichst zu minimieren. Dadurch können nur 
# die Kunden kontaktiert werden, welche tatsächlich an einer Versicherung 
# interessiert sind. 

# Modellauswahl ###

# Das Modell der logistischen Regression mit einem cut-off-point bei 
# 0.05 (lr_confusionmatrix_t0.05) weist 21 false-positives und
# 47 true-positives auf. Das oben beschriebene Ziel, die positives bestmöglich 
# zu prognostizieren, wird damit mit einem Recall von 11.2 % am
# ehesten von den berechneten Modellen erreicht. Das Modell eignet sich jedoch
# nur für diesen Anwendungsfall, da die Negatives-Precision der anderen Modelle
# deutlich besser ist. Hierdurch kann die höhere average Precision der anderen 
# Modelle begründet werden. Auch der F1-Score fällt bei lr_confusionmatrix_t0.05
# am besten aus, obwohl der Score mit 0.19 weit vom Optimum 1.0 entfernt ist.

# Auffälligkeiten ###

# Auffällig ist, dass die Precision der Positives im Fall der logistischen
# Regression zunimmt, wenn sich der cut-off-point sich der 0 annähert. Wir haben
# uns dazu entschieden, den cut-off-point maximal auf 0.05 zu reduzieren, um
# einen akzeptablen trade-off zwischen Precision und Sensitivity zu erreichen.

# Die KNN-Modelle eignen sich für diesen Anwendungsfall eher weniger, da mit 
# 13.0 % (k=1), 10.5 % (k=3), 12.5 % (k=5) eine niedrige Precision der Positives
# zugrunde gelegt wird.
