#####################
##################### 6

library("lubridate") # работа с датами

library("sandwich") # vcovHC, vcovHAC
library("lmtest") # тесты
library("car") # еще тесты
library("zoo") # временные ряды
library("xts") # еще ряды
library("dplyr") # манипуляции с данными
library("broom") # манипуляции
library("ggplot2") # графики

library("quantmod") # загрузка с finance.google.com
library("rusquant") # загрузка с finam.ru
library("sophisthse") # загрузка с sophist.hse.ru
library("Quandl") # загрузка с Quandl

# задаём даты в виде простого текста
x <- c("2012-04-15","2011-08-17")

y <- ymd(x) # конвертируем в специальный формат дат
y

y + days(20) # прибавим 20 дней
y - years(10) # вычтем 10 лет
day(y) # вытащим из даты только число
month(y) # ... только месяц
year(y) # ... только год
vignette("lubridate") # более подробная справка про даты

# создадим временной ряд
x <- rnorm(5) # пять N(0,1) случайных величин
x

y <- ymd("2014-01-01")+days(0:4) # даты к этим величинам
y

ts <- zoo(x,order.by=y) # склеим числа и даты в один временной ряд
ts

lag(ts,-1) # лаг, то есть прошлое значение ряда
lag(ts,1) # форвардный лаг, то есть будущее значение
diff(ts) # приращение ряда

# те же пять чисел, только оформленные как квартальные данные
ts2 <- zooreg(x,start=as.yearqtr("2014-01"),freq=4)
ts2

# те же пять чисел, только оформленные как месячные данные
ts3 <- zooreg(x,start=as.yearmon("2014-01"),freq=12)
ts3

data("Investment") # встроенный набор данных
help("Investment")

start(Investment) # момент начала временного ряда
end(Investment) # окончания
time(Investment) # только моменты времени
coredata(Investment) # только сами числа без дат

dna <- Investment # скопируем набор данных Investment
dna[1,2] <- NA # и внесем туда искусственно пропуски
dna[5,3] <- NA
na.approx(dna) # линейная аппроксимация
na.locf(dna) # заполнение последним известным значением

# загрузка данных с sophist.hse.ru
# это численность населения России
a <- sophisthse("POPNUM_Y")
a
# другие названия рядов можно глянуть 
#на http://sophist.hse.ru/hse/nindex.shtml
# например, CPI_Y_CHI --- индекс потребительских цен

# загрузка данных с quandl
b <- Quandl("FRED/GNP")
b
# это огромная база, по ней есть удобный поиск
# https://www.quandl.com/

# загрузка данных finance.google.com
Sys.setlocale("LC_TIME","C") # это шаманское заклинание позволяет избежать проблем с русской кодировкой месяцев под windows
# цены акций компании Apple:
getSymbols(Symbols = "AAPL",from="2010-01-01",
           to="2014-02-03",src="google")
head(AAPL)
tail(AAPL)

# загрузка данных с finam.ru
# цены акций компании Газпром
getSymbols(Symbols="GAZP",from="2011-01-02",
           to="2014-09-09",src="Finam")
head(GAZP)
tail(GAZP)

# несколько вариантов графиков:
plot(GAZP)
autoplot(GAZP[,1:4])
autoplot(GAZP[,1:4],facets = NULL)
chartSeries(GAZP)

# возвращаемся к набору данных с инвестициями
# в R есть два популярных формата хранения табличных данных
# это data.frame для невременных рядов
# и zoo или xts для временных рядов
# некоторые функции заточены под один формат, некоторые - под другой
# мы превращаем data.frame Investment в zoo временной ряд
d <- as.zoo(Investment)
autoplot(d[,1:2],facets = NULL)

# простая линейная модель
model <- lm(data=d, RealInv~RealInt+RealGNP)


summary(model) # краткий отчет по модели
coeftest(model) # тесты на коэффициенты 
confint(model) # доверительные интервалы для коэффициентов
# в этих трех командах по умолчанию используются
# некорректные для автокорреляции станадртные ошибки

# добавим к исходных данным остатки и прогнозы
d_aug <- augment(model, as.data.frame(d))
glimpse(d_aug)
qplot(data=d_aug,lag(.resid),.resid) # график остатка от предыдущего значения

vcov(model) # обычная оценка ковариационной матрицы
# не годная в условиях автокорреляции

vcovHAC(model) # робастная оценка ковариационной матрицы
# годная в условиях автокорреляции

# тестируем гипотезы о равенстве коэффициентов нулю 
# с помощью правильной оценки ковариационной матрицы
coeftest(model,vcov. = vcovHAC(model))

# строим корректные при автокоррреляции доверительные интервалы
conftable <- coeftest(model,vcov. = vcovHAC(model))
ci <- data.frame(estimate=conftable[,1],
                 se_ac=conftable[,2])
ci <- mutate(ci,left_95=estimate-1.96*se_ac,
             right_95=estimate+1.96*se_ac)
ci

# Durbin-Watson
# H0: нет автокорреляции
# Ha: автокорреляции 1-го порядка
dwt(model)
res <- dwt(model)
res$dw # значение статистики DW 
res$p # симуляционное p-value. 
# В силу небольшого количества наблюдений и симуляционных свойств алгоритма может колебаться
res$r # оценка корреляции

# Тест Бройша-Годфри
# H0: нет автокорреляции
# Ha: автокорреляция k-го порядка
bgtest(model,order = 2)
# H0 не отвергается
res <- bgtest(model,order = 2)
res$statistic # значение статистики BG
res$p.value # P-значение

###############
############### 8
library("lubridate") # работа с датами

library("zoo") # временные ряды
library("xts") # еще ряды
library("dplyr") # манипуляции с данными
library("ggplot2") # графики
library("forecast")

library("quantmod") # загрузка с finance.google.com
library("sophisthse") # загрузка с sophist.hse.ru

# симулируем процесс AR(1) y_t=0.7y_{t-1}+\e_t
y <- arima.sim(n=100, list(ar=0.7))
plot(y) # график ряда
Acf(y)
Pacf(y)
tsdisplay(y) # все три графика одним махом

# симулируем процесс MA(1) y_t=\e_t-0.8\e_{t-1}
y <- arima.sim(n=100, list(ma=-0.8))
tsdisplay(y)

# симулируем процесс ARMA(1,1) y_t=0.5y_{t-1}+\e_t-0.8\e_{t-1}
y <- arima.sim(n=100, list(ma=-0.8, ar=0.5))
tsdisplay(y)

# симулируем процесс ARMA(1,1) y_t=-0.5y_{t-1}+\e_t-0.8\e_{t-1}
y <- arima.sim(n=100, list(ma=-0.8, ar=-0.5))
tsdisplay(y)

# симулируем процесс случайного блуждания y_t=y_{t-1}+\e_t
y <- arima.sim(n=100, list(order=c(0,1,0)))
tsdisplay(y)

# то же случайное блуждание, только 501 наблюдение
y <- arima.sim(n=500, list(order=c(0,1,0)))
tsdisplay(y)

# добавим в AR(1) процессу тренд
y <- seq(0, 10, length=100) + arima.sim(n=100, list(ar=0.7))
tsdisplay(y)

# добавим тренд послабее к AR(1) процессу
y <- seq(0, 2, length=100) + arima.sim(n=100, list(ar=0.7))
tsdisplay(y)

# динамика уровня воды в озере Гурон
y <- LakeHuron
tsdisplay(y)

# оценим AR(2)
mod_1 <- Arima(y, order=c(2,0,0))
# оценим ARMA(1,1)
mod_2 <- Arima(y, order=c(1,0,1))

# результаты оценивания:
summary(mod_1)
summary(mod_2)

# штрафной критерий AIC
AIC(mod_1)
AIC(mod_2)

# оценим ARMA(2,1)
mod_3 <- Arima(y, order=c(2,0,1))
summary(mod_3)
AIC(mod_3)

# прогнозируем по модели 2 на 5 шагов вперед
prognoz <- forecast(mod_2, h=5)
prognoz

# строим график прогноза
plot(prognoz)

# оценим ARIMA(1,1,0)
mod_4 <- Arima(y, order=c(1,1,0))
AIC(mod_4)

# прогноз по модели ARIMA(1,1,0)
prognoz_4 <- forecast(mod_4, h=5)
plot(prognoz_4)

# автоматический подбор модели по штрафному критерию
mod_a <- auto.arima(y)
summary(mod_a)

# прогноз по автоматически подбираемой модели
prognoz_a <- forecast(mod_a, h=5)
plot(prognoz_a)

# шаманское заклинание для перевода дат на английский
# чтобы корректно работала загрузка данных под русскоязычной windows
Sys.setlocale("LC_TIME","C")

# загружаем данные по стоимости акций Гугла
getSymbols(Symbols = "GOOG", from="2014-01-01", to="2014-12-01")


head(GOOG) # начало набора данных
y <- GOOG$GOOG.Close # поместим цену закрытия в переменную y

tsdisplay(y) # три графика для исходного ряда
dy <- diff(y)
tsdisplay(dy) # три графика для приращений исходного ряда (для первой разности)

# похоже на случайное блуждание, оценим ARIMA(0,1,0)
mod_1 <- Arima(y, order=c(0,1,0))
summary(mod_1)

# прогнозируем на 20 шагов вперед
prognoz_1 <- forecast(mod_1, h=20)
prognoz_1

# строим график прогноза
plot(prognoz_1)

# автоматический подбор модели
mod_a <- auto.arima(y)
summary(mod_a)

# численность населения России
y <- sophisthse("POPNUM_Y")
tsdisplay(y)

# ARIMA(1,1,0) со смещением
mod_1 <- Arima(y, order=c(1,1,0), include.drift = TRUE)
summary(mod_1)

# прогноз на 5 шагов вперед
prognoz_1 <- forecast(mod_1, h=5)
plot(prognoz_1)

# индекс потребительских цен (ежемесячные данные)
y <- sophisthse("CPI_M_CHI")
tsdisplay(as.ts(y))


time(y) # из временного ряда достанет номера моментов времени
ym <- y[97:nrow(y),] # возьмем наблюдения с 97 по последнее
tsdisplay(as.ts(ym))

# оценим модель AR(1) с сезонной составляющей SAR(1)
mod_1 <- Arima(ym, order=c(1,0,0), seasonal = c(1,0,0))
summary(mod_1)
AIC(mod_1)

# прогнозируем на год вперед
prognoz_1 <- forecast(mod_1, h=12)
plot(prognoz_1)

# оцениваем автоматически подбираемую модель
mod_a <- auto.arima(ym)

# упрощенная версия максимального правдоподобия не прошла
# запускаем полную, это чуть подольше
mod_a <- auto.arima(ym, approximation = FALSE)

# прогнозируем на год вперед
prognoz_a <- forecast(mod_a, h=12)
plot(prognoz_a)

