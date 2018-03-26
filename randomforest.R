
library("randomForest") # случайный лес
library("rattle") # визуализация деревьев
library("caret") # стандартный подход к регрессионным и классификационным задачам
library("rpart") # классификационные и регрессионные деревья

####################################
# алгоритм случайного леса

# делим набор данных на две части: обучающую (75%) и тестовую
# получаем номера наблюдений из обучающей части
in_sample <- createDataPartition(f$price, p=0.75, list=FALSE)
head(in_sample) # несколько номеров наблюдений, входящих в обучающую выборку

f_train <- f[in_sample,] # отбираем наблюдения с номерами из in_sample
f_test <- f[-in_sample,] # отбираем наблюдения с номерами не из in_sample

# обычная регрессия
model_lm <- lm(data=f_train, price~totsp+kitsp+livesp+brick)
# случайный лес
model_rf <- randomForest(data=f_train, price~totsp+kitsp+livesp+brick)

# поместим цену в отдельную переменную
y <- f_test$price
# прогнозы по МНК
yhat_lm <- predict(model_lm, f_test)
# прогнозы по случайному лесу
yhat_rf <- predict(model_rf, f_test)

# сумма квадратов остатков прогнозов по тестовой выборке: МНК
sum( (y - yhat_lm)^2   )
# сумма квадратов остатков прогнозов по тестовой выборке: случайные лес
sum( (y - yhat_rf)^2   )
