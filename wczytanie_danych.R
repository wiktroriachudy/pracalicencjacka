library(readxl)
library(olsrr)  ###cp mallows
library(GGally)
library(readxl)
library(regclass)
library(psych)
library(ggplot2)
library(deltaPlotR)
library(e1071)
library("gridExtra")
dane<-read_excel("DANE5.xlsx")
View(dane)
attach(dane)
dane<-as.data.frame(dane)
ilosciowe<-which(sapply(dane,is.numeric))
ilosciowe
############################################################################################################
################################################  D A N E ##################################################
plot(1:262,sort(dane$Cena[`stan wykończenia`=="do zamieszkania"]))

head(dane)
#STAN WYKOŃCZENIA
`stan` <- factor(`stan`,levels=c("do zamieszkania","do remontu","do wykończenia"))
levels(`stan`)
plot(`stan wykończenia`)
plot(`powierzchnia [m^2]`,`liczba pokoi`)
cor(log(`powierzchnia [m^2]`),`liczba pokoi`)
log(`powierzchnia [m^2]`)
summary(stan)

ramka <- data.frame(
  stan = c("do zamieszkania", "do remontu", "do wykończenia"),
  Wartosc = c(262,28,10)
)

# Tworzenie wykresu kołowego w ggplot dla zmiennej Dzielnica
ggplot(ramka, aes(x = "", y = Wartosc, fill = stan)) +
  geom_bar(stat = "identity") +
  coord_polar("y", start = 0) +
  labs(x = NULL, y = NULL, fill = "Stan wykończenia",title="Wykres kołowy") +
  geom_text(aes(label = paste0(round((Wartosc/sum(Wartosc))*100,2), "%")), position = position_stack(vjust = 0.5))





#DZIELNICA
Dzielnica<-factor(Dzielnica,levels=c("Fabryczna","Psie Pole","Stare Miasto","Śródmieście","Krzyki"))
levels(Dzielnica)
summary(Dzielnica)
Dzielnica1<-data.frame(Dzielnica)
plot(Dzielnica)

ramka <- data.frame(
  Dzielnica = c("Fabryczna", "Psie Pole", "Stare Miasto", "Śródmieście", "Krzyki"),
  Wartosc = c(84, 61, 24,46, 85)
)

# Tworzenie wykresu kołowego w ggplot dla zmiennej Dzielnica
ggplot(ramka, aes(x = "", y = Wartosc, fill = Dzielnica)) +
  geom_bar(stat = "identity") +
  coord_polar("y", start = 0) +
  labs(x = NULL, y = NULL, fill = "Dzielnica",title="Wykres kołowy") +
  geom_text(aes(label = paste0(round((Wartosc/sum(Wartosc))*100,2), "%")), position = position_stack(vjust = 0.5))


#POWIERZCHNIA
boxplot(`powierzchnia [m^2]`)
summary(`powierzchnia [m^2]`)
hist(`powierzchnia [m^2]`)
sd(`powierzchnia [m^2]`)
skewness(`powierzchnia [m^2]`)
kurtosis(`powierzchnia [m^2]`)
pow<-data.frame(dane$`powierzchnia [m^2]`)

ggplot(pow, aes(dane$`powierzchnia [m^2]`)) +
  geom_histogram(bins= 14,color = "#000000", fill = "#0099F8")+
  labs(
    x="Powierzchnia mieszkania",
    y="Ilość",
    title="Histogram")

ggplot(pow, aes(x = "", y = `powierzchnia [m^2]`)) +
  geom_boxplot(color = "#000000", fill = "#0099F8") +
  labs(x = "", y = "Cena mieszkania", title="Boxplot") 







#RODZAJ ZABUDOWY
`rodzaj zabudowy`<-factor(`rodzaj zabudowy`,levels=c("blok", "apartamentowiec","kamienica", "szeregowiec"))
levels(`rodzaj zabudowy`)
plot(`rodzaj zabudowy`)
is.factor(`rodzaj zabudowy`)

ramka <- data.frame(
  rodzaj = c("apartamentowiec", "blok", "kamienica", "szeregowiec"),
  Wartosc = c(56, 189, 48,7)
)

# Tworzenie wykresu kołowego w ggplot dla zmiennej Dzielnica
ggplot(ramka, aes(x = "", y = Wartosc, fill = rodzaj)) +
  geom_bar(stat = "identity") +
  coord_polar("y", start = 0) +
  labs(x = NULL, y = NULL, fill = "Rodzaj zabudowy",title = "Wykres kołowy") +
  geom_text(aes(label = paste0(round((Wartosc/sum(Wartosc))*100,2), "%")), position = position_stack(vjust = 0.5))

summary(`rodzaj zabudowy`)

#TYP OGŁOSZENIODAWCY
`typ ogłoszeniodawcy`<-factor(`typ ogłoszeniodawcy`, levels=c("prywatny", "biuro nieruchomości"))
plot(`typ ogłoszeniodawcy`)
summary(`typ ogłoszeniodawcy`)

ramka <- data.frame(
  typ = c("prywatny", "biuro nieruchomości"),
  Wartosc = c(105,195)
)

# Tworzenie wykresu kołowego w ggplot dla zmiennej Dzielnica
ggplot(ramka, aes(x = "", y = Wartosc, fill = typ)) +
  geom_bar(stat = "identity") +
  coord_polar("y", start = 0) +
  labs(x = NULL, y = NULL, fill = "Typ ogłoszeniodawcy",title="Wykres kołowy") +
  geom_text(aes(label = paste0(round((Wartosc/sum(Wartosc))*100,2), "%")), position = position_stack(vjust = 0.5))


#OGRZEWANIE
ogrzewanie<-factor(ogrzewanie, levels=c("elektryczne", "miejskie","kotłownia", "inne", "gazowe"))
levels(ogrzewanie)
plot(ogrzewanie)
summary(ogrzewanie)


ramka <- data.frame(
  ogrzewanie = c("elektryczne", "miejskie","kotłownia", "inne", "gazowe"),
  Wartosc = c(12,183,29,23,53)
)

# Tworzenie wykresu kołowego w ggplot dla zmiennej Dzielnica
ggplot(ramka, aes(x = "", y = Wartosc, fill = ogrzewanie)) +
  geom_bar(stat = "identity") +
  coord_polar("y", start = 0) +
  labs(x = NULL, y = NULL, fill = "Ogrzewanie",title = "Wykres kołowy") +
  geom_text(aes(label = paste0(round((Wartosc/sum(Wartosc))*100,2), "%")), position = position_stack(vjust = 0.5))




#LICZBA POKOI
summary(`liczba pokoi`)
plot(`liczba pokoi`)
hist(`liczba pokoi`)
liczba_pokoi<-table(`liczba pokoi`)
liczba_pokoi
barplot(liczba_pokoi)

ramka <- data.frame(
  Kategoria = c("1", "2", "3", "4"),
  Wartosc = c(12, 117, 111, 35)
)

ggplot(ramka, aes(x = Kategoria, y = Wartosc)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(x = "Liczba pokojów", y = "Liczba mieszkań ", title="Wykres słupkowy") +
  theme_minimal()




#PIĘTRO
summary(piętro)
plot(piętro)
hist(piętro)
Piętro<-table(piętro)

ramka <- data.frame(
  Kategoria = c(" 0"," 1" , " 2", " 3", " 4"," 5"," 6"," 7"," 8"," 9","10"),
  Wartosc = c(47, 53, 61, 52,32,12,6,2,5,1,4)
)

ggplot(ramka, aes(x = Kategoria, y = Wartosc)) +
  geom_bar(stat = "identity",color = "#000000", fill = "blue") +
  labs(x = "Piętro mieszkania", y = "Liczba mieszkań ", title="Wykres słupkowy") 









#ILOŚĆ PIĘTER
summary(`ilość pięter w budynku`)
plot(`ilość pięter w budynku`)
hist(`ilość pięter w budynku`)
budynek<-table(`ilość pięter w budynku`)
ramka <- data.frame(
  Kategoria = c(" 1" , " 2", " 3", " 4"," 5"," 6"," 7"," 8"," 9","10","11"),
  Wartosc = c(5, 42, 64, 85,23,12,7,9,2,16,10)
)

ggplot(ramka, aes(x = Kategoria, y = Wartosc)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(x = "Liczba pięter w budynku", y = "Liczba mieszkań ", title="Wykres słupkowy") +
  theme_minimal()



#BALKON

Balkon<-factor(Balkon,levels=c("tak","nie"))
levels(Balkon)
plot(Balkon)
summary(Balkon)
#TARAS
taras<-factor(taras, levels=c("tak","nie"))
levels(taras)
plot(taras)
summary(taras)
#OGRÓD

ogród<-factor(ogród, levels=c("tak","nie"))
levels(ogród)
plot(ogród)
summary(ogród)

ramka <- data.frame(
  ogród = c("tak", "nie"),
  Wartosc = c(33,267)
)

# Tworzenie wykresu kołowego w ggplot dla zmiennej Dzielnica
ggplot(ramka, aes(x = "", y = Wartosc, fill = ogród)) +
  geom_bar(stat = "identity") +
  coord_polar("y", start = 0) +
  labs(x = NULL, y = NULL, fill = "Ogród") +
  geom_text(aes(label = paste0(round((Wartosc/sum(Wartosc))*100,2), "%")), position = position_stack(vjust = 0.5))

boxplot(Cena~stan)



#WINDA
winda<-factor(winda,levels=c("tak","nie"))
levels(winda)
plot(winda)
summary(winda)

#OKNA ANTYWŁAMANIOWE
`okna antywłamaniowe`<- factor(`okna antywłamaniowe`,levels=c("tak","nie"))
levels(`okna antywłamaniowe`)
plot(`okna antywłamaniowe`)
summary(`okna antywłamaniowe`)
summary(`rok budowy`)
rok<-data.frame(`rok budowy`)

ggplot(rok, aes(`rok budowy`)) +
  geom_histogram(color = "#000000", fill = "blue")+
  labs(
    x="Rok budowy",
    y="Liczba mieszkań ",
    title="Histogram")+
  theme_minimal()




#PIWNICA
`piwnica/pomieszczenie użytkowe`<-factor(`piwnica/pomieszczenie użytkowe`,levels=c("tak","nie"))
levels(`piwnica/pomieszczenie użytkowe`)
plot(`piwnica/pomieszczenie użytkowe`)
summary(`piwnica/pomieszczenie użytkowe`)
#TEREN ZAMKNIĘTY 
`teren zamknięty`<-factor(`teren zamknięty`,levels=c("tak","nie"))
levels(`teren zamknięty`)
plot(`teren zamknięty`)
summary(`teren zamknięty`)
#CENA
Cena1<-data.frame(dane$Cena)
summary(Cena)
mean(Cena)
skewness(Cena)
kurtosis(Cena)
plot(Cena)
IQR(Cena)
range(Cena)
sd(Cena)
boxplot(Cena)


hist(Cena1, main="Rozkład ceny mieszkań")

ggplot(Cena1, aes(dane$Cena)) +
  geom_histogram(bins=14,color = "#000000", fill = "#0099F8")+
  labs(
       x="Cena mieszkania",
       y="Liczba",
       title="Histogram")

Cena2<-data.frame(dane4$Cena)
ggplot(Cena2, aes(dane4$Cena)) +
  geom_histogram(bins=14,color = "#000000", fill = "#0099F8")+
  labs(
    x="Cena mieszkania",
    y="Liczba",
    title="Histogram")



ggplot(Cena2, aes(x = "", y = Cena)) +
  geom_boxplot(color = "#000000", fill = "#0099F8") +
  labs(x = "", y = "Cena mieszkania",title="Boxplot") 

summary(dane4$`powierzchnia [m^2]`)


#Korelacja 
ggcorr(dane)
cor(Cena,`powierzchnia [m^2]`)  #0.7489
cor(`liczba pokoi`,`rok budowy`) #0.73925
cor(`ilość pięter w budynku`,`liczba pokoi`)   #0.5689
cor(`liczba pokoi`,Cena)  #0.6278
cor

boxplot(dane4$Cena)
boxplot(dane$Cena/dane$`powierzchnia [m^2]`~dane$stan, xlab="Stan wykończenia", ylab="Cena", main="Boxplot")
boxplot(dane$Cena~dane$`rodzaj zabudowy`, xlab="Rodzaj zabudowy", ylab="Cena", main="Boxplot")
boxplot(dane$Cena~dane$ogrzewanie, xlab="Ogrzewanie", ylab="Cena", main="Boxplot")
boxplot(dane$Cena~dane$`typ ogłoszeniodawcy`, xlab="Typ ogłoszeniodawcy", ylab="Cena", main="Boxplot")
boxplot(dane$Cena~dane$Dzielnica, xlab="Dzielnica", ylab="Cena", main="Boxplot")
boxplot(dane$Cena~dane$ogród, xlab="Ogród", ylab="Cena", main="Boxplot")
boxplot(dane$Cena~dane$taras, xlab="Taras", ylab="Cena", main="Boxplot")
boxplot(dane$Cena~dane$Balkon, xlab="Balkon", ylab="Cena", main="Boxplot")
boxplot(dane$Cena~dane$`teren zamknięty`, xlab="Teren zamknięty", ylab="Cena", main="Boxplot")
boxplot(dane$Cena~dane$`okna antywłamaniowe`, xlab="Okna antywłamaniowe", ylab="Cena", main="Boxplot")
boxplot(dane$Cena~dane$`piwnica/pomieszczenie użytkowe`, xlab="Piwnica", ylab="Cena", main="Boxplot")
boxplot(dane$Cena~dane$winda, xlab="Winda", ylab="Cena", main="Boxplot")


#######################################################################################################################
#############################################  M O D E L E ###########################################################

###### Podział na zbiór uczący i testowy ###########

sed.seed(123)
n <- nrow(dane)
n
`rodzaj zabudowy`
as.numeric(dane$`rodzaj zabudowy`)
learning.set.index <- sample(1:n,0.8*n)
learning.set <- dane[learning.set.index,]
test   <- dane[-learning.set.index,]

dim(learning.set)
dim(test.set)




Model<-lm(Cena~., learning.set)
sum<-summary(Model)

y_pred_test<- predict(Model,test)
R1<-sum$r.squared
R1[i]<-sum$adj.r.squared
MAE1<-(sum(abs(y_pred_test -  test$Cena)) / nrow(test))
MSE1<-sum((y_pred_test -  test$Cena)^2) / nrow(test)
RMSE1 <- sqrt (sum((y_pred_test -  test$Cena)^2) / nrow(test))
MAPE1<- 1/nrow(test)*sum(abs((test$Cena-y_pred_test)/test$Cena))


######### M O D E L    P E Ł N Y   dla pełnego zbioru #####################

model<-lm(Cena~., data=dane)
summary(model)   #R^2=0.8008
plot(dffits(model), xlab="Numer indeksu mieszkania", ylab="DFFITS", main= "DFFITS")
R<-c()
for (i in 1:1000){
  index <- sample(1:nrow(dane), 0.8*nrow(dane)) 
  train <- dane[index,]
  test  <- dane[-index,]
  lmfit<-lm(Cena~.,data=train)
  sum<-summary(lmfit)
  R[i]<-sum$adj.r.squared
}

R<-mean(R)
R
View(dane)

VIF(model)  #nigdzie nie ma powyżej 10 
d<-cooks.distance(model)
d
plot(d,main= "Odległość Cooke'a", xlab="Numer indeksu mieszkania", ylab="Odległość Cooke'a")
which(d>4/(300-5))  # 16  23  54  57  68  72  74  75  91 107 132 169 172 229 233 236 240 252 263 273 293 300 
which(d>0.025)
r<-rstudent(model)
plot(r,main= "Studentyzowane rezydua", xlab="Numer indeksu mieszkania", ylab="Studentyzowane rezedua")
which(r>3) #16  23  34  57  68  75  88  91 169 219 229 252 300 
which(r<(-2))   #107 132 
dffits(model)


# 16 23 54 57 68 72 75 88 91 107 132 169 172 219 229 233 252 263 273 293 300
 
Dane()
D<-dffits(model)
plot(dffits(model))
which(D>2*sqrt(18/300))
which(D>1)


4/(300-18)
ols_plot_dffits(model, print_plot = TRUE)

plot(rstudent(model))
library(car)
outlierTest(model)
plot(`rok budowy`)
qqPlot(model)
boxplot(learning.set3$Cena~learning.set3$`rok budowy`)
par(mfrow=c(1,2))
hist(learning.set3$Cena[`typ ogłoszeniodawcy`=="prywatny"],breaks=10)
hist(learning.set3$Cena[`typ ogłoszeniodawcy`=="biuro nieruchomości"],breaks=10)
boxplot(learning.set3$Cena~learning.set3$`stan wykończenia`)
boxplot(learning.set3$Cena~learning.set3$ogród)
Cena[`typ ogłoszeniodawcy`=="prywatny"]
###################################################################################

#USUWAM TYLKO 65 OBSERWACJĘ !!!!!!!!!!!!!!!!!


set.seed(1)    # ustalamy ziarno generatora aby otrzymać powtarzalne wyniki
n2 <- nrow(dane2)
n2

learning.set.index <- sample(1:n,0.8*n)
learning.set <- dane[learning.set.index,]
test.set    <- dane[-learning.set.index,]

dim(learning.set2)
dim(test.set2)

model2<-lm(Cena~., data=learning.set2)
summary(model2) #Znacznie słabiej niż w przypadku z 65 obserwacją R^2=0.7351

VIF(model2)
d2<-cooks.distance(model2)
which(d>0.07)

plot(rstudent(model2))

ols_plot_dffits(model2, print_plot = TRUE)

library(car)
outlierTest(model)

########################################################################################
ols_plot_dffits(model, print_plot = TRUE)
dane4<-dane[-c(16, 23, 54, 57, 68, 72, 75, 88 ,91, 107, 132, 169, 172, 219, 229, 233, 252, 263, 273, 293, 300,34,123,140,143,162,240,255,261,51,81,152,236),]  #patrząc na wykres ols_plot_dffits(model, print_plot = TRUE)
set.seed(1)    # ustalamy ziarno generatora aby otrzymać powtarzalne wyniki
n4<- nrow(dane4)
n4
attach(dane4)
summary(dane4$Cena)
sd(dane4$Cena)
skewness(dane4$Cena)
kurtosis(dane4$Cena)
model2<-lm(Cena~., data=dane4)
plot(cooks.distance(model2), ylab="Odległość Cooke'a", main="Odległość Cooke'a")
which(cooks.distance(model2)>4/(271-18))
plot(rstudent(model2), main="Studentyzowane rezydua", ylab="Studentyzowane rezydua")
plot(dffits(model2), main="DFFITS", ylab="DFFITS")
which(rstudent(model2)>2)
which(rstudent(model2)<(-2))
learning.set.index4 <- sample(1:n4,0.8*n4)
learning.set4 <- dane4[learning.set.index4,]
test   <- dane4[-learning.set.index4,]
Model<-lm(Cena~., learning.set4)
summary(Model)
y_pred_test<- predict(Model,test)
R1.[i]<-sum$r.squared
R1[i]<-sum$adj.r.squared
MAE1<-(sum(abs(y_pred_test -  test$Cena)) / nrow(test))
MSE1<-sum((y_pred_test -  test$Cena)^2) / nrow(test)
RMSE <- sqrt (sum((y_pred_test -  test$Cena)^2) / nrow(test))
MAPE1<- 1/nrow(test)*sum(abs((test$Cena-y_pred_test)/test$Cena))

dim(learning.set4)
dim(test.set4)

model4<-lm(Cena~., data=learning.set4)
summary(model4)  #tu mamy R^2=0.7387
plot(cooks.distance(model4))
VIF(model4)


plot(rstudent(model4))

ols_plot_dffits(model4, print_plot = TRUE)

library(car)
outlierTest(model4)

######################################################################################################################
###################### MODEL PEŁNY PO USUNIĘCIU ###################################################################
dane3<-dane[-c(57,75,107,16,132,172),]

attach(dane3)
set.seed(1)    # ustalamy ziarno generatora aby otrzymać powtarzalne wyniki
n3<- nrow(dane3)
n3
n9<-nrow(dane9)
learning.set.index3 <- sample(1:n3,0.8*n3)
learning.set3 <- dane3[learning.set.index3,]
test.set3   <- dane3[-learning.set.index3,]

dim(learning.set3)
dim(test.set3)

model3<-lm(Cena~., data=learning.set3)
sum<-summary(model3)
sum
n9

r1<-rstudent(model3)
plot(rstudent(model3))
which(r1>2)
which(r1<(-2))


learning.set.index9<- sample(1:n9,0.8*n9)
learning.set9 <- dane9[learning.set.index9,]
test.set9  <- dane9[-learning.set.index9,]

dim(learning.set3)
dim(test.set3)

model9<-lm(Cena~., data=learning.set9)
sum<-summary(model9)
sum






dim(sum$coefficients)
sum$coefficients[1,]

###########################################################################################################################

VIF(model3)
plot(cooks.distance(model3))

plot(rstudent(model3))

ols_plot_dffits(model3, print_plot = TRUE)

library(car)
outlierTest(model3)
plot(learning.set3$Cena,learning.set3$`stan wykończenia`)
learning.set$Cena[ogród=="tak"]
#############################################################
################ TEST homoskedastczności ##########
library(lmtest)
gq<-gqtest(model1)
print(gq)

x<-Cena
x2 <- seq(min(x), max(x), length = 40)

# Normal curve
fun <- dnorm(x2, mean = mean(x), sd = sd(x))

# Histogram
hist(x, prob = TRUE, col = "white",
     ylim = c(0, max(fun)),
     main = "Histogram with normal curve")
lines(x2, fun, col = 2, lwd = 2)
lines(density(x),col=4,lwd=2)

qqnorm(Cena)
x<-rnorm(300,mean(Cena), sd(Cena))
sw<-shapiro.test(Cena)
print(sw)
qqnorm(x)
qqnorm(model$residuals)
qqnorm(model1$residuals)


#################################################

e_std <- rstandard(model)
head(e_std)
eq<-quantile(e_std, seq(0.01,0.99, 0.01))
tq<-qnorm(seq(0.01,0.99, 0.01))
plot(tq, eq, xlab="Theoretical quantiles", ylab="Standardized residuals quantiles")
 
#####################################################################################

y_pred_test<- predict(model, test.set)
y_pred_test1<-predict(model1,test.set1)
plot(y_pred_test, test.set$Cena, xlab="Actual", ylab="Predicted")
abline(0,1)
plot(y_pred_test1, test.set1$Cena, xlab="Actual", ylab="Predicted")
abline(0,1)




attach(dane4)



########################################################################################
############################ LASSO REGRESSION###########################################


library(glmnet)                         
X=data.matrix(learning.set3[,-1])
Y=learning.set3$Cena
X_1=data.matrix(test.set3[,-1])
Y_1<-test.set3$Cena
lasso <- glmnet(X,Y, alpha=1)
summary(lasso)

cv_lasso<-cv.glmnet(X,Y,alpha=1)
best_lambda<-cv_lasso$lambda.min
best_lambda
plot(cv_lasso)
best_model <- glmnet(X, Y, alpha = 1, lambda = best_lambda)
coef(best_model)
y_predicted <- predict(best_model, s = best_lambda, newx = X_1)

#find SST and SSE
sst <- sum((Y_1 - mean(Y_1))^2)
sse <- sum((y_predicted - Y_1)^2)

#find R-Squared
r<-1-sse/sst
r
rsq <- 1 - ((sse/(n3-17))/(sst/(n3-1)))
rsq                         

#######################################  3 GRUPA  ######################################

MAE <- (sum(abs(y_predicted-  test.set3$Cena)) / nrow(test.set3))
MAE

MSE<-sum((y_predicted -  test.set3$Cena)^2) / nrow(test.set3)
MSE

RMSE <- sqrt (sum((y_predicted -  test.set3$Cena)^2) / nrow(test.set2))
RMSE

MAPE<- 1/nrow(test.set3)*sum(abs((test.set3$Cena-y_predicted)/test.set3$Cena))
MAPE




##################################### RIDGE REGRESSION ########################################

library(glmnet)                         
X=data.matrix(learning.set3[,-1])
Y=learning.set3$Cena
ridge <- glmnet(X,Y, alpha=0)
summary(ridge)
#Aby wybrać najlepsze lambda używamy k-fold cross validation i wybieramy lambdę, która minimalizuje MSE
cv_ridge<-cv.glmnet(X,Y,alpha=0)
best_lambda<-cv_ridge$lambda.min
best_lambda
plot(cv_ridge)
best_model <- glmnet(X, Y, alpha = 0, lambda = best_lambda)
coef(best_model)

y_predicted <- predict(best_model, s = best_lambda, newx = X_1)


sst <- sum((Y_1 - mean(Y_1))^2)
sse <- sum((y_predicted - Y_1)^2)

r<-1-sse/sst
r
rsq <- 1 - ((sse/(n3-17))/(sst/(n3-1)))
rsq  

MAE <- (sum(abs(y_predicted-  test.set3$Cena)) / nrow(test.set3))
MAE

MSE<-sum((y_predicted -  test.set3$Cena)^2) / nrow(test.set3)
MSE

RMSE <- sqrt (sum((y_predicted -  test.set3$Cena)^2) / nrow(test.set3))
RMSE

MAPE<- 1/nrow(test.set3)*sum(abs((test.set3$Cena-y_predicted)/test.set3$Cena))
MAPE



############################################ zamiana na liczbowe ############################
dane10<-read_excel("dane7.xlsx")
attach(dane10)
View(dane10)
n <- nrow(dane10)
n

learning.set.index10 <- sample(1:n,0.8*n)
learning.set10 <- dane10[learning.set.index,]
test.set10    <- dane10[-learning.set.index,]

dim(learning.set)
dim(test.set)

model<-lm(Cena~.,data=learning.set10)
summary(model)

modelMax = lm(Cena~., learning.set10)
modelMin = lm(Cena~Dzielnica, learning.set10)
mBack = step(modelMax, scope = list(upper = modelMax,lower = modelMin),
             direction = "backward")
mForw= step(modelMin, scope = list(upper = modelMax, lower = modelMin),
            direction = "forward")
mBoth= step(modelMax, scope = list(upper = modelMax, lower = modelMin),
            direction = "both")
summary(mBack)
summary(mForw)
summary(mBoth)

boxplot(dane$Cena~dane$stan)
boxplot(dane4$Cena~dane4$stan)
boxplot(dane4$Cena~dane4$Dzielnica)
boxplot(dane$Cena~dane$`typ ogłoszeniodawcy`)

boxplot(learning.set3$Cena~learning.set3$`Dzielnica`)
boxplot(learning.set3$Cena~learning.set3$Balkon)
boxplot(learning.set3$Cena~learning.set3$taras)
boxplot(learning.set3$Cena~learning.set3$`okna antywłamaniowe`)
boxplot(learning.set3$Cena~learning.set3$`teren zamknięty`)
boxplot(learning.set3$Cena~learning.set3$`rodzaj zabudowy`)
boxplot(learning.set3$Cena~learning.set3$`stan wykończenia`)
boxplot(learning.set3$Cena~learning.set3$`typ ogłoszeniodawcy`)
boxplot(learning.set3$Cena~learning.set3$`piwnica/pomieszczenie użytkowe`)


Call:
  lm(formula = Cena ~ Dzielnica + `powierzchnia [m^2]` + `liczba pokoi` + 
       piętro + `ilość pięter w budynku` + `stan wykończenia` + 
       `typ ogłoszeniodawcy` + `rok budowy` + `rodzaj zabudowy` + 
       winda + `okna antywłamaniowe` + Balkon, data = train)

Call:
  lm(formula = Cena ~ Dzielnica + `powierzchnia [m^2]` + `rok budowy` + 
       `stan wykończenia` + `rodzaj zabudowy` + winda + `typ ogłoszeniodawcy` + 
       Balkon + `liczba pokoi` + `piwnica/pomieszczenie użytkowe` + 
       `okna antywłamaniowe`, data = train)


################################### MODEL PEŁNY ########################################
attach(dane)
dim(dane_D)
R1.<-c()
R1<-c()
MAE1<-c()
MSE1<-c()
RMSE1<-c()
MAPE1<-c()
przedział1<-matrix(0,nrow=27,ncol=100)
przedział2<-matrix(0,nrow=27,ncol=100)
View(dane4)
for (i in 1:100){
  index <- sample(1:nrow(dane4), 0.8*nrow(dane4)) 
  train <- dane4[index,]
  test  <- dane4[-index,]
  Model<-lm(Cena~.,data=train)
  sum<-summary(Model)
  przedział1[,i]<-confint(Model)[,1]
  przedział2[,i]<-confint(Model)[,2]
  y_pred_test<- predict(Model,test)
  R1.[i]<-sum$r.squared
  R1[i]<-sum$adj.r.squared
  MAE1[i]<-(sum(abs(y_pred_test -  test$Cena)) / nrow(test))
  MSE1[i]<-sum((y_pred_test -  test$Cena)^2) / nrow(test)
  RMSE1[i] <- sqrt (sum((y_pred_test -  test$Cena)^2) / nrow(test))
  MAPE1[i]<- 1/nrow(test)*sum(abs((test$Cena-y_pred_test)/test$Cena))
}
rowMeans(przedział1)
przedział1
rowMeans(przedział2)
mean(R1.)  #0.8331859
mean(R1)  #0.810238
mean(MAE1) #51557.58
mean(MSE1)  #4035974836
mean(RMSE1) #63340.86
mean(MAPE1) #0.08913154
przedział1
index <- sample(1:nrow(dane4), 0.8*nrow(dane4)) 
train <- dane4[index,]
test  <- dane4[-index,]
Model<-lm(Cena~.,data=train)
summary(Model)
confint(Model)
dim(dane4)
##############################################################################################
########################## Regresja krokowa Backward #########################################
attach(dane4)
View(dane4)
R2.<-c()
R2<-c()
MAE2<-c()
MSE2<-c()
RMSE2<-c()
MAPE2<-c()
A<-c()
B<-c()
M<-c()
for (i in 1:1000){
  index <- sample(1:nrow(dane4), 0.8*nrow(dane4)) 
  train <- dane4[index,]
  test  <- dane4[-index,]
  modelMax = lm(Cena~., train)
  modelMin = lm(Cena~Dzielnica, train)
  mBack = step(modelMax, scope = list(upper = modelMax,lower = modelMin),
               direction = "backward")
  sum1<-summary(mBack)
  y_pred_test1<- predict(mBack,test)
  R2.[i]<-sum1$r.squared
  R2[i]<-sum1$adj.r.squared
  MAE2[i]<-(sum(abs(y_pred_test1 -  test$Cena)) / nrow(test))
  MSE2[i]<-sum((y_pred_test1 -  test$Cena)^2) / nrow(test)
  RMSE2[i] <- sqrt (sum((y_pred_test1 -  test$Cena)^2) / nrow(test))
  MAPE2[i]<- 1/nrow(test)*sum(abs((test$Cena-y_pred_test1)/test$Cena))
  A[i]<-AIC(mBack)
  B[i]<-BIC(mBack)
  M[i]<-ols_mallows_cp(mBack,modelMax) 
}
dim(dane3)
fa
dane_D1$Dzielnica
mean(R3.) #0.8288098
mean(R3)  #0.8110661
mean(MAE3) # 52302.88
mean(MSE3) #4166004339
mean(RMSE3) #64345.83
mean(MAPE3) #0.09017852
mean(A2) #5383.369
mean(B2) #5458.382
mean(M) #3.5628
############################################################################################################33
###################### Regresja Forward #############################################################


attach(dane4)
R3.<-c()
R3<-c()
MAE3<-c()
MSE3<-c()
RMSE3<-c()
MAPE3<-c()
A2<-c()
B2<-c()
M2<-c()
for (i in 1:1000){
  index <- sample(1:nrow(dane4), 0.8*nrow(dane4)) 
  train <- dane4[index,]
  test  <- dane4[-index,]
  modelMax = lm(Cena~., train)
  modelMin = lm(Cena~Dzielnica, train)
  mForw= step(modelMin, scope = list(upper = modelMax, lower = modelMin),
              direction = "forward")
  sum2<-summary(mForw)
  y_pred_test2<- predict(mForw,test)
  R3.[i]<-sum2$r.squared
  R3[i]<-sum2$adj.r.squared
  MAE3[i]<-(sum(abs(y_pred_test2 -  test$Cena)) / nrow(test))
  MSE3[i]<-sum((y_pred_test2 -  test$Cena)^2) / nrow(test)
  RMSE3[i] <- sqrt (sum((y_pred_test2 -  test$Cena)^2) / nrow(test))
  MAPE3[i]<- 1/nrow(test)*sum(abs((test$Cena-y_pred_test2)/test$Cena))
  A2[i]<-AIC(mForw)
  B2[i]<-BIC(mForw)
  M2[i]<-ols_mallows_cp(mForw,modelMax) 
}

mean(R3.) #0.8396
mean(R3)  #0.8221
mean(MAE3) # 50841.88
mean(MSE3) #4166004339
mean(RMSE3) #62396.93
mean(MAPE3) #0.0880
mean(A2) #5296.51
mean(B2) #5373.108
mean(M2) #5.336


##########################################################################################################
############################## REGRESJA BOTH #############################################################

set.seed(123)
R4.<-c()
R4<-c()
MAE4<-c()
MSE4<-c()
RMSE4<-c()
MAPE4<-c()
A3<-c()
B3<-c()
M3<-c()
przedział1<-matrix(0,nrow=22,ncol=1000)
przedział2<-matrix(0,nrow=22,ncol=1000)
for (i in 1:1000){
  index <- sample(1:nrow(dane4), 0.8*nrow(dane4)) 
  train <- dane4[index,]
  test  <- dane4[-index,]
  modelMax = lm(Cena~., train)
  modelMin = lm(Cena~Dzielnica, train)
  mBoth= step(modelMax, scope = list(upper = modelMax, lower = modelMin),
              direction = "both")
  sum3<-summary(mBoth)
  y_pred_test3<- predict(mBoth,test)
  R4.[i]<-sum3$r.squared
  R4[i]<-sum3$adj.r.squared
  MAE4[i]<-(sum(abs(y_pred_test3 -  test$Cena)) / nrow(test))
  MSE4[i]<-sum((y_pred_test3 -  test$Cena)^2) / nrow(test)
  RMSE4[i] <- sqrt (sum((y_pred_test3 -  test$Cena)^2) / nrow(test))
  MAPE4[i]<- 1/nrow(test)*sum(abs((test$Cena-y_pred_test3)/test$Cena))
  A3[i]<-AIC(mBoth)
  B3[i]<-BIC(mBoth)
  M3[i]<-ols_mallows_cp(mBoth,modelMax) 
}

mean(R4.) #0.8402
mean(R4)  #0.8226
mean(MAE4) #50641.52
mean(MSE4) #4119606777
mean(RMSE4) #62247.98
mean(MAPE4) #0.0875
mean(A3) #5296.41
mean(B3) #5373.89
mean(M3) #3.67

reszty<-sum3$residuals
library(fitdistrplus)
fitdist(reszty,"norm") -> fit.norm
plot(fit.norm)
qqnorm(sum3$residuals)
shapiro.test(sum3$residuals)
sum(reszty)
library(nortest)
#Testy normalności 
shapiro.test(reszty) #potwierdza
cvm.test(reszty) #potwierdza
ad.test(reszty) #potwierdza
lillie.test(reszty) #potwierdza
pearson.test(reszty) #odrzucatest
reszty
y_pred_test3
plot(y_pred_test3,test$Cena,xlab="Przewidywana wartość cen", ylab="Rzeczywista wartość cen", main="Dokładność modelu")
confint(mBoth)[,2]

przedział1<-matrix(0,nrow=22,ncol=1000)
przedział2<-matrix(0,nrow=22,ncol=1000)
for (i in 1:1000){
  index <- sample(1:nrow(dane4), 0.8*nrow(dane4)) 
  train <- dane4[index,]
  test  <- dane4[-index,]
  Model<-lm(Cena~Dzielnica+`powierzchnia [m^2]`+`liczba pokoi`+`ilość pięter w budynku`+stan+ogrzewanie+`typ ogłoszeniodawcy`+`rok budowy`+`rodzaj zabudowy`+winda+`piwnica/pomieszczenie użytkowe`+Balkon,data = train)
  sum<-summary(Model)
  przedział1[,i]<-confint(Model)[,1]
  przedział2[,i]<-confint(Model)[,2]
}
install.packages("matrixStats")
library(matrixStats)
rowMedians(przedział1)
rowMedians(przedział2)

(długość_przedziału<-rowMeans(przedział2)-rowMeans(przedział1))
###############################################################################################################
##########################################  NA OKO I    #######################################################
attach(dane4)
R5.<-c()
R5<-c()
MAE5<-c()
MSE5<-c()
RMSE5<-c()
MAPE5<-c()
for (i in 1:1000){
  index <- sample(1:nrow(dane4), 0.8*nrow(dane4)) 
  train <- dane4[index,]
  test  <- dane4[-index,]
  model6<-lm(Cena~Dzielnica+`powierzchnia [m^2]`+`liczba pokoi`+stan+`typ ogłoszeniodawcy`+winda+`rodzaj zabudowy`+`rok budowy`,data=train)
  sum5<-summary(model6)
  y_pred_test5<- predict(model6,test)
  R5.[i]<-sum5$r.squared
  R5[i]<-sum5$adj.r.squared
  MAE5[i]<-(sum(abs(y_pred_test5 -  test$Cena)) / nrow(test))
  MSE5[i]<-sum((y_pred_test5 -  test$Cena)^2) / nrow(test)
  RMSE5[i] <- sqrt (sum((y_pred_test5 -  test$Cena)^2) / nrow(test))
  MAPE5[i]<- 1/nrow(test)*sum(abs((test$Cena-y_pred_test5)/test$Cena))
}
mean(R5.)  #0.7937319
mean(R5)  #0.7896147
mean(MAE5) #53779.54
mean(MSE5) #4380695286
mean(RMSE5) #65988.3
mean(MAPE5) #0.09148105

###########################################################################################################
######################################  NA OKO II #########################################################
attach(dane4)
R6.<-c()
R6<-c()
MAE6<-c()
MSE6<-c()
RMSE6<-c()
MAPE6<-c()
for (i in 1:1000){
  index <- sample(1:nrow(dane4), 0.8*nrow(dane4)) 
  train <- dane4[index,]
  test  <- dane4[-index,]
  model5<-lm(Cena~`powierzchnia [m^2]`+Dzielnica+`liczba pokoi`+`ilość pięter w budynku`+`piwnica/pomieszczenie użytkowe`+stan+ogrzewanie+`typ ogłoszeniodawcy`+`rok budowy`+`rodzaj zabudowy`+winda+Balkon,data=train)
  sum6<-summary(model5)
  y_pred_test6<- predict(model5,test)
  R6.[i]<-sum6$r.squared
  R6[i]<-sum6$adj.r.squared
  MAE6[i]<-(sum(abs(y_pred_test6 -  test$Cena)) / nrow(test))
  MSE6[i]<-sum((y_pred_test6 -  test$Cena)^2) / nrow(test)
  RMSE6[i] <- sqrt (sum((y_pred_test6 -  test$Cena)^2) / nrow(test))
  MAPE6[i]<- 1/nrow(test)*sum(abs((test$Cena-y_pred_test6)/test$Cena))
}
mean(R6.)  # 0.8245143
mean(R6)  #0.807503
mean(MAE6) #51357.55
mean(MSE6) #3966528794
mean(RMSE6) #62808.1
mean(MAPE6) #0.08893575

MODEL4<-lm(Cena~.,dane4)
plot(cooks.distance(MODEL4))
which(cooks.distance(MODEL4)>0.025)

#############################################################################################################
#################################  RIDGE REGRESSION ########################################################
library(glmnet)
R7.<-c()
R7<-c()
MAE7<-c()
MSE7<-c()
RMSE7<-c()
MAPE7<-c()
for (i in 1:1000){
  index <- sample(1:nrow(dane4), 0.8*nrow(dane4)) 
  train <- dane4[index,]
  test  <- dane4[-index,]
  X=data.matrix(train[,-1])
  Y=train$Cena
  X_1=data.matrix(test[,-1])
  Y_1<-test$Cena
  ridge <- glmnet(X,Y, alpha=0)
  summary(ridge)
  cv_ridge<-cv.glmnet(X,Y,alpha=0)
  best_lambda<-cv_ridge$lambda.min
  best_model <- glmnet(X, Y, alpha = 0, lambda = best_lambda)
  y_predicted <- predict(best_model, s = best_lambda, newx = X_1)
  sst <- sum((Y_1 - mean(Y_1))^2)
  sse <- sum((y_predicted - Y_1)^2)
  R7.[i]<-1-sse/sst
  R7[i]<- 1 - ((sse/(271-17))/(sst/(271-1)))
  MAE7[i]<-(sum(abs(y_predicted -  test$Cena)) / nrow(test))
  MSE7[i]<-sum((y_predicted -  test$Cena)^2) / nrow(test)
  RMSE7[i] <- sqrt (sum((y_predicted -  test$Cena)^2) / nrow(test))
  MAPE7[i]<- 1/nrow(test)*sum(abs((test$Cena-y_predicted)/test$Cena))
}
mean(R7.)  #  0.7588165
mean(R7)  #0.7388217
mean(MAE7) #53723.39
mean(MSE7) #4439129880
mean(RMSE7) #66361.18
mean(MAPE7) #0.09122687






n3
dim(dane4)
#############################################################################################################
#################################  LASSO REGRESSION ########################################################
library(glmnet)
dane4
R8.<-c()
R8<-c()
MAE8<-c()
MSE8<-c()
RMSE8<-c()
MAPE8<-c()
for (i in 1:1000){
  index <- sample(1:nrow(dane4), 0.8*nrow(dane4)) 
  train <- dane4[index,]
  test  <- dane4[-index,]
  X=data.matrix(train[,-1])
  Y=train$Cena
  X_1=data.matrix(test[,-1])
  Y_1<-test$Cena
  lasso <- glmnet(X,Y, alpha=1)
  summary(lasso)
  cv_lasso<-cv.glmnet(X,Y,alpha=1)
  best_lambda<-cv_lasso$lambda.min
  best_model <- glmnet(X, Y, alpha = 1, lambda = best_lambda)
  coef(best_model)
  y_predicted <- predict(best_model, s = best_lambda, newx = X_1)
  sst <- sum((Y_1 - mean(Y_1))^2)
  sse <- sum((y_predicted - Y_1)^2)
  R8.[i]<-1-sse/sst
  R8[i]<- 1 - ((sse/(271-17))/(sst/(271-1)))
  MAE8[i]<-(sum(abs(y_predicted -  test$Cena)) / nrow(test))
  MSE8[i]<-sum((y_predicted -  test$Cena)^2) / nrow(test)
  RMSE8[i] <- sqrt (sum((y_predicted -  test$Cena)^2) / nrow(test))
  MAPE8[i]<- 1/nrow(test)*sum(abs((test$Cena-y_predicted)/test$Cena))
}
mean(R8.)  # 0.7637622
mean(R8)  #0.7488811
mean(MAE8) # 52079.79
mean(MSE8) #4795205662
mean(RMSE8) #64585.63
mean(MAPE8) #0.08847423


#############################################################################################################################
#########################################################################################################################
#########################################################################################################################

dane_D<-read_excel("DZIELNICE.xlsx")
attach(dane_D)
View(dane_D)
modelD<-lm(Cena~.,dane_D)
plot(cooks.distance(modelD))
which(cooks.distance(modelD)>4/(300-5))
plot(rstudent(modelD))
which(rstudent(modelD)>2)
dane_D1<-dane_D[-c(16,23,24,48,53,57,59,72,74,75,88,91,107,132,152,169,172,229,233,240,252,263,273,293,300 ),]
