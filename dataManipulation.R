# загружаем пакеты
#library("memisc") # две и более регрессий в одной табличке
#library("dplyr") # манипуляции с данными
#library("psych") # описательные статистики
#library("lmtest") # тестирование гипотез в линейных моделях
#library("sjPlot") # графики
#library("sgof")
#library("ggplot2") # графики
#library("foreign") # загрузка данных в разных форматах
#library("car")
#library("hexbin") # графики
#library("rlms") # загрузка данных в формате rlms (spss)

# сохранение результатов работы
stuff <- list(data=h, model=model2) # список ценных объектов
saveRDS(file = "mydata.RDS",stuff) # сохраняем всё ценное в файл

mylist <- readRDS("mydata.RDS") # читаем из файла что там есть
summary(mylist$model)

# формат csv. comma separated values

# первая (сознательно неудачная) попытка чтения файла
t <- read.csv("flats_moscow.txt")
glimpse(t)

# попытка чтения файла с указанием параметров:
t <- read.csv("flats_moscow.txt", sep="\t", dec=".", header=TRUE)
glimpse(t)

# простая модель зависимости цены от общей площади и кирпичности дома
mod_3 <- lm(data=t, price~totsp+brick)
summary(mod_3)

# читаем данные RLMS 

# предварительно их нужно скачать с http://www.hse.ru/rlms/spss
# для примера взята 21 волна, индивиды, репрезентативная выборка

# командой из пакета rlms
h <- read.rlms("r21i_os24a.sav")
# если пакет rlms почему-то не установился, то 
# нужно убрать комментарий и выполнить следующую строку:
# h <- read.spss("r21i_os24a.sav", to.data.frame=TRUE, reencode="UTF-8")
glimpse(h)

# отбираем часть переменных из таблички h в табличку h2
h2 <- select(h, qm1, qm2, qh6, qh5)
glimpse(h2) # смотрим на df_sel

# переименовываем переменные
h3 <- rename(h2, ves=qm1, rost=qm2, sex=qh5, b_year=qh6)
# добавляем возраст
h3 <- mutate(h3, vozrast=2012-b_year)

describe(h3) # описательные статистики

summary(h3$sex) # таблица частот

# отберём мужчин в отдельную табличку
h4 <- filter(h3, sex=="МУЖСКОЙ")

# диаграмма рассеяния
qplot(data=h4, rost, ves)

# гистограмма
qplot(data=h4, ves)

#####################
##################### 5
## Пример функции

f <- function(x) { # функция получает x на входе
  res <- x^2 # возводит его в квардарт 
  return(res) # и выдаёт на выходе значение res
}

f(3)
f(-1)


# функция с необязательным параметром
fs <- function(x, stepen=2) { # функция получает x на входе, если не указать, то stepen=2
  res <- x^2 # возводит его в stepen 
  return(res) # и выдаёт на выходе значение res
}

fs(4)
fs(2,stepen=5)


# функция считающая процент пропущенных наблюдений в data.frame

na_perc <- function(d) { # функция получает таблицу d на входе
  # проверяем корректность того, что d --- таблица:
  if (!is.data.frame(d)) stop("d should be a data.frame!")
  res <- sum(is.na(d))/nrow(d)/ncol(d) # делим количество NA на количество строк и количество столбцов
  return(res) # функция  выдаёт на выходе значение res
}

# тестируем функцию:

d <- cars
d[1,2] <- NA # искусствено внесем пропуски в таблицу
d[3,1] <- NA
na_perc(d) # получаем два процента пропусков 

x <- c(5,6,7)
na_perc(x) # с векторами наша функция не работает :)


## Пример цикла:

for (i in 5:10) { # переменная i пробежит значения от 5 до 10
  k <- i^2 
  cat("i=",i," i^2=",k,"\n") # значок \n означает новую строку
}

all_data <- NULL
for (fname in c("file01.csv","file02.csv")) { # имя файла пробежит оба значения
  temp <- read.csv(fname) # прочитаем файл с очередным именем
  all_data <- rbind(all_data,temp) # подклеим прочитанную табличку в конец таблички all_data
}

##################
################## 9
library("dplyr") # манипуляции с данными
library("caret") # стандартизованный подход к регрессионным и классификационным моделям
library("AER") # инструментальные переменные 
library("ggplot2") # графики
library("sandwich") # робастные стандартные ошибки
library("ivpack") # дополнительные плющки для инструментальных переменных
library("memisc") # табличка mtable 

#########################
# задача прогнозирования

# прочитаем данные из .txt файла
# есть заголовок, header = TRUE
# разделитель данных - табуляция, sep="\t"
# разделитель дробной части - точка, dec="."
h <- read.csv("flats_moscow.txt", header = TRUE, sep="\t", dec=".")

glimpse(h) # бросим взгляд на данные

# добавим логарифмы цены и площадей
h2 <- mutate(h, logprice=log(price), logtotsp=log(totsp), 
             logkitsp=log(kitsp), loglivesp=log(livesp))

# создадим разбиение данных, отберем 75% случайных номеров
in_train <- createDataPartition(y = h2$logprice, p = 0.75, list=FALSE)
h2_train <- h2[in_train,] # отберем обучающую часть выборки
h2_test <- h2[-in_train,] # оставшееся пойдет в тестовую часть выборки

# оценим две модели с помощью МНК
model_1 <- lm(data=h2_train, logprice~logkitsp+logtotsp+loglivesp)
model_2 <- lm(data=h2_train, logprice~logtotsp)

# построим прогнозы по двум моделям на тестовой выборке
pred_1 <- predict(model_1, h2_test)
pred_2 <- predict(model_2, h2_test)

# посчитаем руками суммы квадратов остатков по тестовой выборке
sum( (pred_1 - h2_test$logprice)^2)
sum( (pred_2 - h2_test$logprice)^2)


###############################################
# оценивание заданной формы модели с эндогенностью

## данные
data("CigarettesSW", package = "AER") # активируем набор данных
help("CigarettesSW") # читаем справку 


# для удобства назовем покороче
h <- CigarettesSW 
glimpse(h) # посмотрим на структуру данных

# построим диаграмму рассеяния
qplot(data=h, price, packs)

# отберем данные относящиеся к 1995 году
h2 <- filter(h, year=="1995")
# создадим новые переменные
h2 <- mutate(h2, rprice=price/cpi, 
             rincome=income/cpi/population, 
             tdiff=(taxs-tax)/cpi)
# снова глянем на диаграмму рассеяния
qplot(data=h2, price, packs)

# и бросим взгляд на набор данных
glimpse(h2) 



# оценим функцию спроса с помощью МНК забыв, что имеет место эндогенность
model_0 <- lm(data=h2, log(packs)~log(rprice))
summary(model_0)

# двухшаговый МНК руками
# Шаг 1. Строим регрессию эндогенного регрессора на инструментальную переменную
st_1 <- lm(data=h2, log(rprice)~ tdiff)
# сохраняем прогнозы из регрессии первого шага
h2$logprice_hat <- fitted(st_1)

# Шаг 2. Строим регрессию зависимой переменной на прогнозы с первого шага
st_2 <- lm(data=h2, log(packs)~logprice_hat)
coeftest(st_2) 
# здесь функция coeftest использует неверные стандартные ошибки (даже при гомоскедастичности)


help(ivreg) # документация по команде ivreg

# двухшаговый МНК в одну строчку
model_iv <- ivreg(data=h2, log(packs)~log(rprice) | tdiff )
coeftest(model_iv) # здесь стандартные ошибки --- корректные 

# сравним три модели в одной табличке
mtable(model_0, model_iv, st_2)

# используем для проверки гипотез робастные стандартные ошибки
coeftest(model_iv, vcov=vcovHC)


# модель с одной экзогенной, log(rincome), и одной эндогенной переменной, log(rprice)
iv_model_2 <- ivreg(data=h2, 
                    log(packs) ~ log(rprice) + log(rincome) | log(rincome) + tdiff )
# тестируем гипотезы с использованием робастных стандартных ошибок
coeftest(iv_model_2, vcov = vcovHC)

# модель с одной экзогенной, одной эндогенной и двумя инструментальными переменными для эндогенной 
iv_model_3 <- ivreg(data=h2, 
                    log(packs) ~ log(rprice) + log(rincome) | log(rincome) + tdiff + I(tax/cpi) )
# тестируем гипотезы с использованием робастных стандартных ошибок
coeftest(iv_model_3, vcov = vcovHC)
