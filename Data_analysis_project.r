dane = read.csv("https://raw.githubusercontent.com/StanislawC/Games/main/Video_Games_Sales_as_at_22_Dec_2016.csv")
str(dane)
dane <- dane[-1]                                  # Zmienna przechowywująca nazwy gier nie wnosi żadnej informacji do analizy
dane <- na.omit(dane)                             # Można zauważyć sporo brakujących danych, usuwamy wszystkie NA

table(dane$Rating)
dane <- dane[which(dane$Rating!=''), ]            # W kolumnie "Rating" pojawiają się też puste wartości, które należy usunąć dla skuteczności analizy
dane$Rating <- factor(dane$Rating)                # Dane nieliczbowe będziemy zamieniać na typ factor

table(dane$Platform)
dane$Platform <- factor(dane$Platform)
table(dane$Year_of_Release)
dane <- dane[which(dane$Year_of_Release!='N/A'), ] # W kolumnach "Year_of_Release" i "Publisher" pojawiają się braki informacji w postaci "N/A", również je usuwamy
dane <- dane[which(dane$Publisher!='N/A'), ]

dane$Year_of_Release <- as.integer(dane$Year_of_Release) # Zmieniamy typ "Year_of_Release" z char na integer

dane$User_Score <- as.numeric(dane$User_Score)     # Dla User_Score prawidłowym typem jest numeric

table(dane$Genre)
dane$Genre <- factor(dane$Genre)

# Aby algorytm C5.0 mógł zadziałać, trzeba usunąć znaki specjalne z tekstowych wartości
dane$Publisher <- gsub('[^[:alnum:] ]', '', dane$Publisher)
dane$Publisher <- factor(dane$Publisher)
dane$Developer <- gsub('[^[:alnum:] ]', '', dane$Developer)
dane$Developer <- factor(dane$Developer)


# Dla większego zrozumienia problemy spróbujemy zwizualizować dane liczbowe
library(lattice)    # Utworzymy macierz wykresów rozrzutu
splom(dane[, c(2, 9, 10, 11, 12, 13)], type=c("p"), pch='.', varname.cex = .55, axis.text.cex=0.35)

games_cor <- cor(dane[c(2, 9, 10, 11, 12, 13)])
games_cor       # Macierz korelacji może dać wartościowe wyniki. Tak jak można było zgadywać, znacząca liniowa zależność między User_Score i Critic_Score


#install.packages("corrplot")
library(corrplot)
corrplot(games_cor)

hist(dane$User_Score, main = "Histogram User Score", col = "darkmagenta")
hist(dane$Critic_Score, main = "Histogram Critic Score", col = "cyan")

xyplot(User_Score ~ Critic_Score, data = dane)  # Z wykresu widać, że zwykła regresja nie byłaby pozbawiona sensu

str(dane)
dane_cl <- dane
# Aby spróować klasyfikacji względem User_Score, tworzymy 10 klas odpowiadających sufitowi z wartości User_Score
# Jesteśmy też przyzwyczajeni do całkowitych ocen na stronach poświęconych grom czy filmom
dane_cl$User_Score<-ceiling(dane_cl$User_Score)


RNGversion("3.5.2"); set.seed(123)    # Ustalamy "ziarno" generatora liczb pseudolosowych dla powtarzalności eksperymentu
# Jako że zbiór obserwacji nie jest bardzo duży, zależy nam na wielu obserwacjach w zbiorze uczącym, wybieramy 6000 z 6825 obserwacji (ok. 88%)
train_sample <- sample(length(dane_cl[, 1]), 6000)  
games_train_cl <- dane_cl[train_sample, ]
games_test_cl <- dane_cl[-train_sample, ]
str(games_train_cl)
summary(games_train_cl)

#Budowa modelu drzewa klasyfikacyjnego
library("C50")
games_model <- C5.0(games_train_cl[, c(1, 2, 3, 4, 5, 6, 7, 8, 10, 11, 13, 15)],
                    as.factor(games_train_cl$User_Score))
games_model
summary(games_model)  # Tak jak można było się spodziewać, Critic_Score jest kluczowym czynnikiem dla User_Score
                      # Widoczny jest również niewielki wpływ na predykcje kolumn zawierających dane o sprzedaży
games_pred <- predict(games_model, games_test_cl)
#install.packages('gmodels')
library(gmodels)
CrossTable(games_test_cl$User_Score, games_pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE)


# Tworzymy pomocnicze funkcje do analizy predykcji modelów
accuracy = function(pred, test)
{
  sum <- 0
  for (i in 1:length(pred))
  {
    if (pred[i] == test[i])
    {
      sum <- sum + 1
    }
  }
  return (sum/length(pred))
}
mse = function(pred, test)
{
  return (mean((as.numeric(pred) - as.numeric(test))^2))
}
mae = function(pred, test)
{
  return (mean(abs(as.numeric(pred) - as.numeric(test))))
}
accuracy(games_pred, games_test_cl$User_Score)
mse(games_pred, games_test_cl$User_Score)
mae(games_pred, games_test_cl$User_Score)

# Dla poprawy wyników spróbujemy użyć boostingu
games_model_1<- C5.0(games_train_cl[, c(1, 2, 3, 4, 5, 6, 7, 8, 10, 11, 13, 15)], as.factor(games_train_cl$User_Score), trials=100)
games_model_1
#summary(games_model_1)
games_pred_1<-predict(games_model_1, games_test_cl)
CrossTable(games_test_cl$User_Score, games_pred_1,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE)

accuracy(games_pred_1, games_test_cl$User_Score)
mse(games_pred_1, games_test_cl$User_Score)
mae(games_pred_1, games_test_cl$User_Score)
# Zauważalnie lepsze wyniki od wyników klasycznego drzewa, szczególnie na poziomie średnich błędów
# Użyty podzbiór danych dawał najlepsze wyniki z wszystkich wypróbowanych, kolumna Developer powodawała nadmierne dopasowanie do danych uczących

# Teraz spróbujemy predykcji poprzez modele regresyjne, w tym celu tworzymy nowe zbiory testowe i uczące, z wartościami User_Score typu numeric
games_train_reg <- dane[train_sample, ]
games_test_reg <- dane[-train_sample, ]

library(rpart)
games_reg <- rpart(User_Score ~., data = games_train_reg[, c(1, 2, 3, 4, 10, 11, 12, 13)])
summary(games_reg)

games_pred_reg <- predict(games_reg, games_test_reg)
summary(games_pred_reg)              # Widać, że to drzewo zbytnio "obcina" duże i małe obserwacje, zwłaszcza te duże
summary(games_test_reg$User_Score)
mse(games_pred_reg, games_test_reg$User_Score) 
mae(games_pred_reg, games_test_reg$User_Score)

# Wydaje się, że chociażby z uwagi na widoczną liniową zależność między User_Score i Critic_Score, warto rozważyć drzewo modeli regresji

library(Cubist)
games_cubist <- cubist(x = games_train_reg[, c(1, 2, 3, 4, 10, 11, 13)],
                       y = games_train_reg[, 12])
games_cubist
summary(games_cubist)
games_pred_cubist <- predict(games_cubist, games_test_reg)
summary(games_pred_cubist)          # Dużo bardziej dopasowane kwartyle od zwykłego drzewa regresyjnego
summary(games_test_reg$User_Score)
mse(games_pred_cubist, games_test_reg$User_Score)
mae(games_pred_cubist, games_test_reg$User_Score)   # Ta metoda daje zdecydowanie najlepsze wyniki z dotychczasowych
plot(games_test_reg$User_Score, games_pred_cubist, col = 'red')  # Zadowalająca predykcja



# Teraz zajmiemy się analizą sprzedaży światowej
summary(dane$Global_Sales)                # Dane są "ściśnięte" do przedziału [0;1] z obserwacjami mocno odstającymi
boxplot(dane$Global_Sales)                # Potwierdzenie "ściśnięcia" danych, ledwo widoczne ramka i wąsy
quantile(dane$Global_Sales, c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 0.95, 0.99)) # Poglądowa tabela różnych kwantyli
plot(dane$Year_of_Release, dane$Global_Sales, col = 'red')
plot(dane$User_Score, dane$Global_Sales, col = 'red')

#Usuwamy inne dane sprzedaży, zostawiamy tylko światowe (dla większego sensu analizy)
dane1<-dane[-8]
dane1<-dane1[-7]
dane1<-dane1[-6]
dane1<-dane1[-5]
summary(dane1)

stripplot(Platform ~ Global_Sales,
          data=dane,
          jitter.data=TRUE,
          alpha=0.5,
          col="red")
train_sample1 <- sample(length(dane1[, 1]), 6000)   #Tworzymy nowy zbiór uczący i testowy
sales_train <- dane1[train_sample1, ]
sales_test <- dane1[-train_sample1, ]
str(sales_train)
summary(sales_train)

# Tworzymy drzewo modeli regresji dla sprzedaży światowej
sales_cubist <- cubist(x = sales_train[, c(1, 2, 3, 4, 6, 7, 8, 9, 10, 11)],
                       y = sales_train[, 5])
sales_cubist
summary(sales_cubist)
sales_pred <- predict(sales_cubist, sales_test)
summary(sales_pred)
summary(sales_test$Global_Sales)
mse(sales_pred, sales_test$Global_Sales)    # Dla tego zestawu danych błąd kwadratowy jest prawdopodobnie najmniejszy
mae(sales_pred, sales_test$Global_Sales)    # Po usunięciu kolumny Developer można zmniejszyć ten rodzaj błędu 
# Mały bląd bezwględny jest spowodowany skupieniem danych na przedziale [0,1]
plot(sales_test$Global_Sales, sales_pred, xlim = c(0, 5), ylim = c(0, 5), col = 'red')