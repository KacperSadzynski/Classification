library(Amelia)
library(corrplot)
library(caTools)
library(e1071)
library(class)
library(Hmisc)

imports85 <- read.csv("data/im.csv", stringsAsFactors = T)
summary(imports85)

nrow(imports85)
str(imports85)

imports85$symboling = as.factor(imports85$symboling)
summary(imports85$symboling)
levels(imports85$symboling) <- c("Very Safe", "Safe", "Normal", "Risky" , "Very Risky", "Dangerous")

summary(imports85$make)
#imports85$make = combine.levels(imports85$make) nie robimy tutaj combine levels

summary(imports85$fuel.type)
summary(imports85$aspiration)
summary(imports85$num.of.doors)

imports85$body.style = combine.levels(imports85$body.style)
summary(imports85$body.style)

summary(imports85$drive.wheels)

summary(imports85$engine.location) # zmienna do wyrzucenia
idx = which(colnames(imports85) == "engine.location")
imports85 = imports85[,-idx]
summary(imports85)

imports85$engine.type = combine.levels(imports85$engine.type)
summary(imports85$engine.type)

imports85$num.of.cylinders = as.factor(imports85$num.of.cylinders)
imports85$num.of.cylinders = combine.levels(imports85$num.of.cylinders)
summary(imports85$num.of.cylinders)

imports85$fuel.system = combine.levels(imports85$fuel.system)
summary(imports85$fuel.system)

missmap(imports85, main = "Wartosci brakujace vs dostopne")

summary(imports85$num.of.doors)
#brakujace dane to dodge sedan i mazda sedan
table(imports85$make,imports85$num.of.doors, useNA= "ifany")
table(imports85$body.style,imports85$num.of.doors, useNA= "ifany")
table(imports85$body.style[which(imports85$make == "mazda")],imports85$num.of.doors[which(imports85$make == "mazda")], useNA= "ifany")
#jak widzimy wszystkie pozostale mazdy w sedanie sa 4 drzwiowe

table(imports85$body.style[which(imports85$make == "dodge")],imports85$num.of.doors[which(imports85$make == "dodge")], useNA= "ifany")
#jak widzimy w pozostalych autach marki dodge typu sedan,auta sa 4 drzwiowe
#wiec uzupelniamy te braki danych wartoscia "four"
imports85$num.of.doors[is.na(imports85$num.of.doors)] = "four"

imports85$make = combine.levels(imports85$make)

#uzupelnianie normalized losses
summary(imports85$normalized.losses)
table(imports85$make,imports85$normalized.losses, useNA = "ifany")
mean(imports85$normalized.losses, na.rm = TRUE)
aggregate(data = imports85, normalized.losses ~ make, mean, na.rm = TRUE)
ave_losses <- ave(imports85$normalized.losses, imports85$make,
               FUN = function(x) mean(x, na.rm = TRUE))

library(dplyr)#biblioteka dplyr pozwala nam jakos strumieniowac dane i grupowac je jak w SQL
imports85 %>% group_by(make) %>%
  summarise(
    srednia=mean(normalized.losses, na.rm = T),
    mediana=median(normalized.losses, na.rm = T),
    ile_osob=n(),
    ile_brakow=sum(is.na(normalized.losses))
  )

table(ave_losses,imports85$make)
imports85$normalized.losses <- ifelse(is.na(imports85$normalized.losses), ave_losses, imports85$normalized.losses)
summary(imports85$normalized.losses)
imports85$normalized.losses = as.integer(imports85$normalized.losses)

#uzupelnianie price
summary(imports85$price)
table(imports85$make,imports85$price, useNA= "ifany") # mozna zauwazyc ze braki danych sa tylko w kategorii other
table(imports85$make[which(imports85$make == "OTHER")],imports85$price[which(imports85$make == "OTHER")], useNA= "ifany")
#robimy corplot
imports85.corr <- sapply(imports85,function(x) as.numeric(x))
cor_matrix=cor(imports85.corr, use ="complete.obs") #ignorujemy wiersze z brakujacymi wartosciami NA
corrplot(cor_matrix)
#widzimy ze zmienna price jest najmocniej zalezna od zmiennej engine.size

options(max.print=999999)

table(imports85$engine.size[which(imports85$make == "OTHER")],imports85$price[which(imports85$make == "OTHER")], useNA= "ifany")
mean(imports85$price, na.rm = TRUE)
aggregate(data = imports85, price ~ engine.size, mean, na.rm = TRUE)
ave_price <- ave(imports85$price, imports85$engine.size,
                  FUN = function(x) mean(x, na.rm = TRUE))

table(ave_price,imports85$engine.size,useNA= "ifany")
imports85$price <- ifelse(is.na(imports85$price), ave_price, imports85$price)
imports85$price = as.integer(imports85$price)
summary(imports85$price)
#zostalo 1 NA, poniewaz zawiera unikalny rozmiar silnika 203, wiec podmienimy je wartoscia sredniej dla silnika o rozmiaze 209 czyli 36318
imports85$price[is.na(imports85$price)] = 36318

summary(imports85$horsepower)
table(imports85$make,imports85$horsepower, useNA= "ifany")
a = mean(imports85$horsepower[which(imports85$make == "OTHER")], na.rm = T)
imports85$horsepower[is.na(imports85$horsepower)] = mean(imports85$horsepower[which(imports85$make == "OTHER")], na.rm = T)
imports85$horsepower = as.integer(imports85$horsepower)

summary(imports85$stroke)
#srednia i mediana sa zblizone i zmienna stroke nie jest mocno skorelowana z zadna inna, wiec uzupelnimy braki danych srednia
imports85$stroke[is.na(imports85$stroke)] = mean(imports85$stroke, na.rm = T)

summary(imports85$bore)
#srednia i mediana sa zblizone i zmienna "bore" wiec uzupelniam srednia
imports85$bore[is.na(imports85$bore)] = mean(imports85$bore, na.rm = T)

summary(imports85$peak.rpm)
table(imports85$peak.rpm,imports85$fuel.type, useNA= "ifany")
imports85$peak.rpm[is.na(imports85$peak.rpm)] = mean(imports85$peak.rpm[which(imports85$fuel.type == "gas")], na.rm = T)
imports85$peak.rpm = as.integer(imports85$peak.rpm)

#podzial
split <- sample.split(imports85$symboling, Split = 3/7)
train <- subset(imports85, split == T)
test <- subset(imports85, split == F)
nrow(train)
nrow(test)

prop.table(table(train$symboling))
prop.table(table(test$symboling))
prop.table(table(imports85$symboling))
#proporcje sa zachowane

#KNN
train_knn= as.data.frame(sapply(train,function(x) as.numeric(x)))
test_knn= as.data.frame(sapply(test,function(x) as.numeric(x)))

X_train=train_knn[, -c(1)]
y_train=train_knn$symboling
X_test=test_knn[, -c(1)]
y_test=test_knn$symboling

accuracy = function(actual, predicted) {
  mean(actual == predicted)
}

k_to_try = 1:60 # tworzymy vector
acc_k = rep(x = 0, times = length(k_to_try)) # uzupelniamy go zerami

sapply(X_train, class) #sprawdzenie czy na pewno zmienne sa numeryczne

for(i in seq_along(k_to_try)) {
  pred = knn(train = X_train,
             test = X_test,
             cl = y_train,
             k = k_to_try[i])
  acc_k[i] = accuracy(y_test, pred)
}

plot(acc_k, type = "b", col = "blue",
     cex = 1, pch = 20,
     xlab = "k, number of neighbors",
     ylab = "classification accuracy",
     main = "Accuracy vs Neighbors") # graficzne wyswietlenie ilosci k w zaleznosci od acuuracy modelu
abline(v = which(acc_k == max(acc_k)), col = "green", lwd = 1.5) #zaznaczenie najwiekszej wartosci k na wykresie(x)
abline(h = max(acc_k), col = "grey", lty = 2) #zaznaczenie najwiekszej wartosci k na wykresie (y)
which(acc_k == max(acc_k)) # wypisanie najlepszego k do konsoli

actual = y_test # przypisanie do actual zmiennej survived z danych testowych
predicted = knn(train = X_train,
                test = X_test,
                cl = y_train, k = 36)# ustalilismy najlepsze k i budujemy model

accuracy(actual, predicted)
wynik1 = accuracy(actual, predicted) * 100
table(actual,predicted) # tabela ktora wyswietla jak dobrze nasz model sobie poradzil z predykcja (macierz pomylek)

#bardziej skorelowane zmienne
id=which(abs(cor_matrix[1, ]) > 0.3)
imports_03 = imports85
imports_03 = imports_03[, c(id)]
summary(imports_03)

id=which(abs(cor_matrix[1, ]) > 0.3)
test_03 = test
train_03 = train
test_03 = test_03[, c(id)]
train_03 =train_03[, c(id)]
summary(test_03)

nrow(train_03)
nrow(test_03)
prop.table(table(train_03$symboling))
prop.table(table(test_03$symboling))
prop.table(table(imports_03$symboling))

train_knn_03= as.data.frame(sapply(train_03,function(x) as.numeric(x)))
test_knn_03= as.data.frame(sapply(test_03,function(x) as.numeric(x)))
summary(train_knn_03)
X_train_03=train_knn_03[, -c(1)]
y_train_03=train_knn_03$symboling

X_test_03=test_knn_03[, -c(1)]
y_test_03=test_knn_03$symboling

accuracy = function(actual, predicted) {
  mean(actual == predicted)
}

k_to_try = 1:60
acc_k = rep(x = 0, times = length(k_to_try))
for(i in seq_along(k_to_try)) {
  pred = knn(train = X_train_03,
             test = X_test_03,
             cl = y_train_03,
             k = k_to_try[i])
  acc_k[i] = accuracy(y_test_03, pred)
}
plot(acc_k, type = "b", col = "blue",
     cex = 1, pch = 20,
     xlab = "k, number of neighbors",
     ylab = "classification accuracy",
     main = "Accuracy vs Neighbors") # graficzne wyswietlenie ilosci k w zaleznosci od acuuracy modelu
abline(v = which(acc_k == max(acc_k)), col = "green", lwd = 1.5) #zaznaczenie najwiekszej wartosci k na wykresie(x)
abline(h = max(acc_k), col = "grey", lty = 2) #zaznaczenie najwiekszej wartosci k na wykresie (y)
which(acc_k == max(acc_k)) # wypisanie najlepszego k do konsoli

actual = y_test_03 # przypisanie do actual zmiennej survived z danych testowych
predicted = knn(train = X_train_03,
                test = X_test_03,
                cl = y_train_03, k = 1)
accuracy(actual, predicted)
wynik1_03 = accuracy(actual, predicted) * 100
table(actual,predicted) # tabela ktora wyswietla jak dobrze nasz model sobie poradzil z predykcja (macierz pomylek)
#bardziej skorelowane zmienne pomogly dla k = 1

#metoda naiwengo bayesa
symboling_classifier <- naiveBayes(X_train, y_train)
symboling_pred <- predict(symboling_classifier, X_test)
library(gmodels)
CrossTable(symboling_pred, y_test, prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE, dnn = c('predicted', 'actual'))
table(symboling_pred, y_test)
accuracy(y_test,symboling_pred)
wynik2 = accuracy(y_test, symboling_pred) * 100

# na zbioze mocniej skorelowanym
symboling_classifier_03 <- naiveBayes(X_train_03, y_train_03)
symboling_pred_03 <- predict(symboling_classifier_03, X_test_03)
CrossTable(symboling_pred_03, y_test_03, prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE, dnn = c('predicted', 'actual'))
table(symboling_pred_03, y_test_03)
accuracy(y_test_03,symboling_pred_03)
wynik2_03 = accuracy(y_test_03, symboling_pred_03) * 100

#metoda regresji logistycznej
#skorzystanie z kodu na stronie http://www.sthda.com/english/articles/36-classification-methods-essentials/147-multinomial-logistic-regression-essentials-in-r/
library(tidyverse)
library(caret)
library(nnet)

model <- nnet::multinom(symboling ~., data = train)
summary(model)
predicted.classes <- model %>% predict(test)
head(predicted.classes)
# Model accuracy
mean(predicted.classes == test$symboling)
table(predicted.classes, test$symboling)
wynik3 = accuracy(predicted.classes, test$symboling) * 100

# na bardziej skorelowanych zmiennych
model <- nnet::multinom(symboling ~., data = train_03)
summary(model)
predicted.classes <- model %>% predict(test_03)
head(predicted.classes)
# Model accuracy
mean(predicted.classes == test_03$symboling)
table(predicted.classes, test_03$symboling)
wynik3_03 = accuracy(predicted.classes, test_03$symboling) * 100

b <- c(wynik1, wynik2,wynik3)
b_03 <- c(wynik1_03,wynik2_03,wynik3_03)
h <- c("KNN","Naive Bayes","Regresja Logistyczna")
# Plot the bar chart
xx <- barplot(b, names.arg=h, xlab="Algorithm", ylab="Accuracy (%)",col=c("blue","red","yellow"),
        main="Comparison", ylim=c(0,100))
text(x = xx, y = round(b, digits = 2), label = round(b, digits = 2), pos = 3, cex = 0.8)
xx <- barplot(b_03, names.arg=h, xlab="Algorithm", ylab="Accuracy(%)",col=c("blue","red","yellow"),
        main="Comparison",ylim=c(0,100))
text(x = xx, y = round(b_03, digits = 2), label = round(b_03, digits = 2), pos = 3, cex = 0.8)
