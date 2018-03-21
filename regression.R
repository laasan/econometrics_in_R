library("psych") # описательные статистики
library("dplyr") # манипуляции с данными
library("ggplot2") # графики
library("GGally") # еще графики

d <- cars # встроенный набор данных по машинам
glimpse(d) # что там?
help(cars) # справка. действует для встроенных наборов данных
head(d) # начало таблички d (первые 6 строк)
tail(d) # хвостик таблички d
describe(d) # среднее, мода, медиана и т.д.
ncol(d) # число столбцов
nrow(d) # число строк
str(d) # структура (похоже на glimpse)

# среднее арифметическое
mean(d$speed)

# создадим новую переменные и поместим их все в табличку d2
d2 <- mutate(d, speed=1.61*speed, 
             dist=0.3*dist, ratio=dist/speed)
glimpse(d2)

# графики
qplot(data=d2,dist)
qplot(data=d2,dist,xlab="Длина тормозного пути (м)",
      ylab="Число машин",main="Данные по машинам 1920х")

qplot(data=d2,speed,dist)

# оценим модель парной регрессии y_i = \beta_1 + \beta_2 x_i + \varepsilon_i
model <- lm(data=d2, dist~speed)
model

coef(model) # оценки бет
residuals(model) # остатки (эпсилон с крышкой)
y_hat <- fitted(model) # прогнозы (игрек с крышкой)
y <- d2$dist # значения зависимой переменной

RSS <- deviance(model) # так называют RSS
TSS <- sum((y-mean(y))^2) # TSS
TSS
R2 <- 1-RSS/TSS
R2
cor(y,y_hat)^2 # квадрат выборочной корреляции

X <- model.matrix(model) # матрица регрессоров
X

# создаем новый набор данных
nd <- data.frame(speed=c(40,60))
nd

# прогнозируем
predict(model,nd)

# добавляем на график линию регрессии
qplot(data=d2,speed,dist) + stat_smooth(method="lm")

#######################
#######################
h <- swiss # встроенный набор данных по Швейцарии
help(swiss)
glimpse(h)
describe(h)
ggpairs(h) # все диаграммы рассеяния на одном графике

# множественная регрессия
model2 <- lm(data=h,
             Fertility~Agriculture+Education+Catholic)
coef(model2) # оценки бет
fitted(model2) # прогнозы
residuals(model2) # остатки
deviance(model2) # RSS

report <- summary(model2)
report
report$r.squared # R^2

# второй способ расчета R^2
cor(t$Fertility,fitted(model2))^2

# создаем новый набор данных
nd2 <- data.frame(Agriculture=0.5,Catholic=0.5,
                  Education=20)
# прогнозируем
predict(model2,nd2)

########################
########################

library("lmtest") # тестирование гипотез в линейных моделях
library("sjPlot") # графики

# посмотрим результаты оценивания
summary(model2)

# отдельно табличка с тестами
coeftest(model2)

confint(model2) # доверительные интервалы для коэффициентов
sjp.lm(model2) # графическое представление интервалов

# проверка гипотезы b_Cath=b_Agri
# построение вспомогательной модели
model_aux <- lm(data=h, Fertility~Catholic+I(Catholic+Agriculture)+Examination)
summary(model_aux)

# проверка гипотезы без построения вспомогательной модели
linearHypothesis(model2, "Catholic-Agriculture=0")

# стандартизированные коэффициенты

# масштабируем каждую переменную (вычитаем среднее, делим на стандартную ошибку)
h_st <- mutate_all(h, "scale")
glimpse(h_st) # смотрим на новый набор данных
# оцениваем модель по стандартизированным данным
model_st <- lm(data=h_st, Fertility~Catholic+Agriculture+Examination)
summary(model_st) # отчет о новой модели

# графическое представление стандартизованных коэффициентов
sjp.lm(model2, type = "std") 

library("memisc") # две и более регрессий в одной табличке
# сравниваем несколько моделей
model3 <- lm(data=h, Fertility~Catholic+Agriculture)
compar_12 <- mtable(model2, model3)
compar_12

######################
######################
library("memisc")
library("lmtest")
library("ggplot2")
library("dplyr")
library("foreign")
library("vcd")
library("devtools")
library("hexbin")
library("pander")
library("sjPlot")
library("knitr")
# загружаем данные по стоимости квартир в Москве
# предварительно нужно установить рабочую папку
# Session --- Set working directory --- To source file location
f <- read.csv("flats_moscow.txt", sep="\t", header=TRUE, dec=".")

glimpse(f) # краткое содержимое таблички f
qplot(data=f, totsp, price) # диаграмма рассеяния
str(f) # структура таблички f
qplot(data=f, log(totsp), log(price)) # диаграмма рассеяния в логарифмах

# мозаичный график
mosaic(data=f, ~walk+brick + floor, shade=TRUE)

# преобразуем переменны walk, brick, floor, code  в факторные
f <- mutate_each(f, "factor", walk, brick, floor, code)
glimpse(f)  # краткое содержимое таблички f

qplot(data=f, log(price)) # гистограмма

# гистограмма для кирпичных и некирпичных домов
qplot(data=f, log(price), fill=brick) # вариант А
qplot(data=f, log(price), fill=brick, position="dodge") # вариант Б

# оцененные функции плотности
qplot(data=f, log(price), fill=brick, geom="density")

# добавляем прозрачность
g2 <- qplot(data=f, log(price), fill=brick, geom="density", alpha=0.5)

# несколько графиков получаемых путем видоизменения графика g2
g2 + facet_grid(walk~floor)
g2 + facet_grid(~floor)

# оценим три модели
model_0 <- lm(data=f, log(price)~log(totsp))
model_1 <- lm(data=f, log(price)~log(totsp)+brick)
model_2 <- lm(data=f, log(price)~log(totsp)+brick+brick:log(totsp))
# двоеточие в формуле модели в R --- произведение регрессоров

summary(model_0) # базовый вариант отчета о модели
mtable(model_2) # альтернативный вариант отчета


model_2b <- lm(data=f, log(price)~brick*log(totsp))
# умножение в формуле модели в R --- сами регрессоры и их произведение

# сравнение двух моделей
mtable(model_2, model_2b)

# оценки коэффициентов визуально
sjp.lm(model_2)

# создаем новый набор данных для прогнозирования
nw <- data.frame(totsp=c(60,60), brick=factor(c(1,0)))
nw

# точечный прогноз логарифма цены
predict(model_2, newdata=nw)
# переходим от логарифма к цене
exp(predict(model_2, newdata=nw))

# доверительный интервал для среднего значения y
predict(model_2, newdata=nw, interval="confidence") # для логарифма
exp(predict(model_2, newdata=nw, interval="confidence")) # для исходной переменной

# предиктивный интервал для конкретного значения y
predict(model_2, newdata=nw, interval="prediction")
exp(predict(model_2, newdata=nw, interval="prediction"))


# F-тест
waldtest(model_0, model_1) # H_0: model_0 H_a: model_1 
# H_0 отвергается

waldtest(model_1, model_2) # H_0: model_1 H_a: model_2
# H_0 отвергается

waldtest(model_0, model_2) # # H_0: model_0 H_a: model_2 
# H_0 отвергается


# добавляем линию регрессии на диаграмму рассеяния
gg0 <- qplot(data=f, log(totsp), log(price))
gg0 + stat_smooth(method="lm")
gg0 + stat_smooth(method="lm") + facet_grid(~walk)
gg0 + aes(col=brick) + stat_smooth(method="lm") + facet_grid(~walk)

# по-другому зададим дамми переменную
f$nonbrick <- memisc::recode(f$brick, 1 <- 0, 0 <- 1)
glimpse(f)

# сознательно попробуем попасть в ловушку дамми-переменных
model_wrong <- lm(data=f, log(price)~log(totsp)+brick+nonbrick)
summary(model_wrong)

# сравним три модели в одной табличке:
mtable(model_0, model_1, model_2)

# тест Рамсея
resettest(model_2)

########################
########################