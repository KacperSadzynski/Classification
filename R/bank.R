library(Amelia)
library(corrplot)
library(caTools)
library(e1071)
library(class)
library(Hmisc)
library(devtools)
bank <- read.table("data/bank.csv", header=TRUE, sep = ";", stringsAsFactors = T)
summary(bank)

nrow(bank)
str(bank)

missmap(bank, main = "Warto?ci brakuj?ce vs dost?pne")

summary(bank$job)
summary(bank$marital)
summary(bank$month)
summary(bank$previous)

#robimy corplot
bank.corr <- sapply(bank,function(x) as.numeric(x))
cor_matrix=cor(bank.corr)
corrplot(cor_matrix)

#split
split <- sample.split(bank$y,
                      SplitRatio = 1/3)

badania_cls_train <- subset(bank, split == T)
badania_cls_test <- subset(bank, split == F)

prop.table(table(badania_cls_train$y))
prop.table(table(badania_cls_test$y))
prop.table(table(bank$y))

#metoda regresji logistycznej
cls_m = glm(y~., data = badania_cls_train,
            family = binomial(link = "logit"))

pred_log <- predict(cls_m, newdata=badania_cls_test,type='response')
c = table(badania_cls_test$y, pred_log > 0.5)
c
sum(diag(c))/sum(c) #accuracy
wynik1 = sum(diag(c))/sum(c) * 100
wynik1_specificity = c[1, "FALSE"] / (c[1, "FALSE"] + c[1, "TRUE"]) * 100

#SVM
model_svm <- svm(y ~ . , badania_cls_train)
pred_svm <- predict(model_svm,newdata=badania_cls_test)
c = table(badania_cls_test$y, pred_svm)
c
sum(diag(c))/sum(c) #accuracy
wynik2 = sum(diag(c))/sum(c) * 100
wynik2_specificity = c[1, "no"] / (c[1, "no"] + c[1, "yes"]) * 100
#tuned
svm_tune <- tune(svm, y~.,data = badania_cls_train,ranges = list(gamma = seq(0,0.5,0.1), cost = seq(1,100,10)))

print(svm_tune)

best_mod <- svm_tune$best.model #zapisuje najlepszy model z tuningu
best_mod_pred <- predict(best_mod, badania_cls_test)  #zapisuje najlepsz? predykcj? z tuningu
c = table(badania_cls_test$y, best_mod_pred)
c
sum(diag(c))/sum(c)
wynik3 = sum(diag(c))/sum(c) * 100
wynik3_specificity = c[1, 1] / (c[1, 1] + c[1, 2]) * 100
#KNN

train_knn= as.data.frame(sapply(badania_cls_train,function(x) as.numeric(x)))
test_knn= as.data.frame(sapply(badania_cls_test,function(x) as.numeric(x)))

X_train=train_knn[, -c(17)]
y_train=train_knn$y
summary(bank)
summary(X_train)
X_test=test_knn[, -c(17)]
y_test=test_knn$y

accuracy = function(actual, predicted) {
  mean(actual == predicted)
}

k_to_try = 1:200 # tworzymy vector
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
     main = "Accuracy vs Neighbors") # graficzne wyswietlenie ilosci k w zaleznosci od acuuracy modelu
abline(v = which(acc_k == max(acc_k)), col = "green", lwd = 1.5)
abline(h = max(acc_k), col = "grey", lty = 2)
which(acc_k == max(acc_k)) # wypisanie najlepszego k do konsoli

actual = y_test
predicted = knn(train = X_train,
                test = X_test,
                cl = y_train, k = 23)

accuracy(actual, predicted)

table(actual,predicted) #macierz pomylek
t = table(actual,predicted)
wynik4 = accuracy(actual, predicted) * 100
wynik4_specificity = t[1, 1] / (t[1, 1] + t[1, 2]) * 100

#metoda naiwengo bayesa
y_classifier <- naiveBayes(X_train, y_train)
y_pred <- predict(y_classifier, X_test)
library(gmodels)
CrossTable(y_pred, y_test, prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE, dnn = c('predicted', 'actual'))
table(y_pred, y_test)
t = table(actual,predicted)
accuracy(y_test,y_pred)
wynik5 = accuracy(y_test, y_pred) * 100
wynik5_specificity = t[1, 1] / (t[1, 1] + t[1, 2]) * 100

b <- c(wynik1, wynik2,wynik3, wynik4, wynik5)
b_specificity <- c(wynik1_specificity, wynik2_specificity,wynik3_specificity, wynik4_specificity, wynik5_specificity)
h <- c("Logistic regression","SVM","Tuned SVM","KNN","Naive Bayes")
# Plot the bar chart
xx <- barplot(b, names.arg=h, xlab="Algorithm", ylab="Accuracy (%)",col=c("blue","red","yellow","green","orange"),
              main="Comparison", ylim=c(0,100))
text(x = xx, y = round(b, digits = 2), label = round(b, digits = 2), pos = 3, cex = 0.8)
xx <- barplot(b_specificity, names.arg=h, xlab="Algorithm", ylab="Specificity (%)",col=c("blue","red","yellow","green","orange"),
              main="Comparison", ylim=c(0,110))
text(x = xx, y = round(b_specificity, digits = 2), label = round(b_specificity, digits = 2), pos = 3, cex = 0.8)
