# Machine-Learning

## Analyse des Caravan-Datensatz

Der Caravan-Datensatz (enthalten im R Paket ISLR) enthält echte Kundendaten aus
dem Versicherungssektor. Fur jeden der in diesem Datensatz enthaltenen Kunden gibt
die Variable Purchase an, ob die jeweilige Person eine Wohnwagenversicherung abgeschlossen hat (Purchase = Yes) oder kein Interesse an einer solchen Versicherungspolice hat (Purchase = No).
Unser Ziel ist es, in R ein Machine Learning Modell zu berechnen, das prognostiziert,
ob ein Kunde an einer solchen Versicherungspolice interessiert ist. Das Modell soll
im Vertrieb unterstutzend zum Einsatz kommen und Mitarbeiter im Vertrieb sollen
denjenigen Kunden ein Angebot unterbreiten, fur die das Modell prognostiziert, dass
derjenige an einer solchen Police Interesse hat.
Das Modell soll einerseits möglichst viele der Personen identifizieren, die tatsächlich
Interesse haben (um möglichst viele neue Policen verkaufen zu können). Andererseits
sollen unter den zu kontaktierenden Personen möglichst wenig Personen sein, die kein
Interesse an einer solchen Police haben (möglichst wenige false positives damit die
Arbeitszeit der Vertriebsmitarbeiter effizient genutzt wird).
Verwenden Sie zur Bearbeitung der Aufgabe das in Moodle zur Verfugung gestellte
R-Skript-Template. Nutzen Sie Kommentare im Skript, um Ihren Code zu gliedern
bzw. zu erläutern sowie um die letzte Frage zu beantworten.

## 1 Trainings- und Testdaten
Teilen Sie den Caravan-Datensatz in Trainings- und Testdaten auf. Verwenden Sie
hierbei ca. 80 % der Daten als Trainingsdaten und die verbleibenden 20 % als Testdaten.

## 2 Logistische Regression
Berechnen Sie ein logistisches Regressionsmodell fur die Output Variable Purchase
und verwenden Sie alle anderen Variablen als Input Variablen. Wählen Sie drei unterschiedliche Cut-Off Points t und bestimmen Sie die Wahrheitsmatrizen (Confusion
Matrices) der drei zugehörigen Klassifikationsmodelle bzgl. der Testdaten.

## 3 KNN
Verwenden Sie fur K = 1, 3, 5 KNN, um den Wert der Output Variable Purchase der
Testdaten zu schätzen. Bestimmen Sie jeweils die Wahrheitsmatrix. Berucksichtigen
Sie hierbei alle 85 Input Variablen und denken Sie an die Skalierung der Daten.
4 Modellauswahl
Sie haben 6 Klassifikationsmodelle berechnet. Welches Modell ist Ihrer Meinung nach
besser in der oben geschilderten Situation geeignet? Begrunden Sie Ihre Antwort.
