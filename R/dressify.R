library(Amelia)
library(corrplot)
library(caTools)
library(e1071)
library(class)
library(Hmisc)

dressify <- read.csv("data/train_dress.csv", stringsAsFactors = T)
summary(dressify)
nrow(dressify)
str(dressify)

missmap(dressify, main = "Wartosci brakujace vs dostepne")

#wyrzucamy unikalna zmienna ID
dressify$ID = NULL

summary(dressify$Style)
dressify$Style = combine.levels(dressify$Style)

summary(dressify$Price)
dressify$Price[which(dressify$Price == "low")] = "Low"
dressify$Price = combine.levels(dressify$Price)

summary(dressify$Rating)

summary(dressify$Size)
dressify$Size[which(dressify$Size == "small")] = "S"
dressify$Size <- droplevels(dressify$Size)

summary(dressify$Season)
dressify$Season = tolower(dressify$Season)
dressify$Season = as.factor(dressify$Season)
dressify$Season[which(dressify$Season == "automn")] = "autumn"
dressify$Season[which(dressify$Season == "")] = "summer"
dressify$Season <- droplevels(dressify$Season)

summary(dressify$NeckLine)
#dressify$NeckLine[which(dressify$NeckLine == "sweetheart")] = "Sweetheart"
dressify$NeckLine = combine.levels(dressify$NeckLine)

summary(dressify$SleeveLength)
dressify$SleeveLength[which(dressify$SleeveLength == "half")] = "halfsleeve"
dressify$SleeveLength[which(dressify$SleeveLength == "sleeevless")] = "sleeveless"
dressify$SleeveLength[which(dressify$SleeveLength == "sleevless")] = "sleeveless"
dressify$SleeveLength[which(dressify$SleeveLength == "sleveless" )] = "sleeveless"
dressify$SleeveLength = combine.levels(dressify$SleeveLength)

summary(dressify$waiseline)
dressify$waiseline = combine.levels(dressify$waiseline)
dressify$waiseline[which(dressify$waiseline == "null" )] = "OTHER"
dressify$waiseline <- droplevels(dressify$waiseline)

summary(dressify$Material)
dressify$Material = combine.levels(dressify$Material)
dressify$Material[which(dressify$Material == "null" )] = "OTHER"
dressify$Material <- droplevels(dressify$Material)

#za duzo brak?w danych
summary(dressify$FabricType)
dressify$FabricType = NULL

summary(dressify$Decoration)
dressify$Decoration = NULL

summary(dressify$Pattern.Type)
dressify$Pattern.Type = combine.levels(dressify$Pattern.Type)
dressify$Pattern.Type[which(dressify$Pattern.Type == "null" )] = "OTHER"
dressify$Pattern.Type <- droplevels(dressify$Pattern.Type)

summary(dressify$Area)

summary(dressify$Recommended)
dressify$Recommended = as.factor(dressify$Recommended)
levels(dressify$Recommended) <- c("No", "Yes")

#robimy corplot
dressify.corr <- sapply(dressify,function(x) as.numeric(x))
cor_matrix=cor(dressify.corr) #ignorujemy wiersze z brakujacymi wartosciami NA
corrplot(cor_matrix)

#split
split <- sample.split(dressify$Recommended,
                       SplitRatio = 1/3)

badania_cls_train <- subset(dressify, split == T)
badania_cls_test <- subset(dressify, split == F)

#metoda regresji logistycznej
cls_m = glm(Recommended~., data = badania_cls_train,
            family = binomial(link = "logit"))

pred_log <- predict(cls_m, newdata=badania_cls_test,type='response')
c = table(badania_cls_test$Recommended, pred_log > 0.5)
c
sum(diag(c))/sum(c) #accuracy
wynik1 = sum(diag(c))/sum(c) * 100
wynik1_specificity = c[1, "FALSE"] / (c[1, "FALSE"] + c[1, "TRUE"]) * 100
#SVM
model_svm <- svm(Recommended ~ . , badania_cls_train)
pred_svm <- predict(model_svm,newdata=badania_cls_test)
c = table(badania_cls_test$Recommended, pred_svm)
c
sum(diag(c))/sum(c) #accuracy
wynik2 = sum(diag(c))/sum(c) * 100
wynik2_specificity = c[1, "No"] / (c[1, "No"] + c[1, "Yes"]) * 100
#tuned
svm_tune <- tune(svm, Recommended~.,data = badania_cls_train,ranges = list(gamma = seq(0,0.5,0.1), cost = seq(1,100,10)))
print(svm_tune)

best_mod <- svm_tune$best.model #zapisuje najlepszy model z tuningu
best_mod_pred <- predict(best_mod, badania_cls_test)  #zapisuje najlepsz? predykcj? z tuningu
c = table(badania_cls_test$Recommended, best_mod_pred)
c
sum(diag(c))/sum(c)
wynik3 = sum(diag(c))/sum(c) * 100
wynik3_specificity = c[1, 1] / (c[1, 1] + c[1, 2]) * 100
#KNN
train_knn= as.data.frame(sapply(badania_cls_train,function(x) as.numeric(x)))
test_knn= as.data.frame(sapply(badania_cls_test,function(x) as.numeric(x)))

X_train=train_knn[, -c(12)]
y_train=train_knn$Recommended
summary(dressify)
summary(X_train)
X_test=test_knn[, -c(12)]
y_test=test_knn$Recommended

accuracy = function(actual, predicted) {
  mean(actual == predicted)
}

k_to_try = 1:100 # tworzymy vector
acc_k = rep(x = 0, times = length(k_to_try)) # uzupelniamy go zerami

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
     main = "Accuracy vs Neighbors") # graficzne wyswietlenie
abline(v = which(acc_k == max(acc_k)), col = "green", lwd = 1.5) #zaznaczenie najwiekszej wartosci k
abline(h = max(acc_k), col = "grey", lty = 2) #zaznaczenie najwiekszej wartosci k
which(acc_k == max(acc_k)) # wypisanie najlepszego k

actual = y_test
predicted = knn(train = X_train,
                test = X_test,
                cl = y_train, k = 28)

accuracy(actual, predicted)
table(actual,predicted)
t = table(actual,predicted) #macierz pomylek
wynik4 = accuracy(actual, predicted) * 100
wynik4_specificity = t[1, 1] / (t[1, 1] + t[1, 2]) * 100
#metoda naiwengo bayesa
recommended_classifier <- naiveBayes(X_train, y_train)
recommended_pred <- predict(recommended_classifier, X_test)
library(gmodels)
CrossTable(recommended_pred, y_test, prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE, dnn = c('predicted', 'actual'))
table(recommended_pred, y_test)
t = table(recommended_pred, y_test)
accuracy(y_test,recommended_pred)
wynik5 = accuracy(y_test, recommended_pred) * 100
wynik5_specificity = t[1, 1] / (t[1, 1] + t[1, 2]) * 100

b <- c(wynik1, wynik2,wynik3, wynik4, wynik5)
b_specificity <- c(wynik1_specificity, wynik2_specificity,wynik3_specificity, wynik4_specificity, wynik5_specificity)
h <- c("Regresja Logistyczna","SVM","Tuned SVM","KNN","Naive Bayes")
# Plot the bar chart
xx <- barplot(b, names.arg=h, xlab="Algorithm", ylab="Accuracy (%)",col=c("blue","red","yellow","green","orange"),
              main="Comparison", ylim=c(0,100))
text(x = xx, y = round(b, digits = 2), label = round(b, digits = 2), pos = 3, cex = 0.8)
xx <- barplot(b_specificity, names.arg=h, xlab="Algorithm", ylab="Specificity (%)",col=c("blue","red","yellow","green","orange"),
              main="Comparison", ylim=c(0,110))
text(x = xx, y = round(b_specificity, digits = 2), label = round(b_specificity, digits = 2), pos = 3, cex = 0.8)
