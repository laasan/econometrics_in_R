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
library("HSAUR") # из этого пакета возьмем набор данных по семиборью
library("dplyr") # манипуляции с данными
library("psych") # описательные статистики
library("lmtest") # тесты для линейных моделей
library("glmnet") # LASSO + ridge
library("ggplot2") # графики
library("car") # vif

# Последствия мультиколлинеарности


# поместим в табличку h известный нам набор данных
h <- cars

# всегда строим график
qplot(data=h, speed, dist)
# настала пора признаться:
# мы не видим однозначно, 
# является ли зависимость линейной
# или квадратичной
# или даже кубической 


# добавим квадрат и куб скорости в исходный набор данных
h <- mutate(h, speed2=speed^2, speed3=speed^3)

# оценим модель с тремя объясняющими переменными
model_mk <- lm(data=h, dist~speed+speed2+speed3)
summary(model_mk)
# все коэффициенты незначимы
# явно имеет место мультиколлинеарность

# количественный признаки мультиколлинеарности
vif(model_mk) # коэффициент вздутия дисперсии

# создадим матрицу из регрессоров (без единичного столбца)
X0 <- model.matrix(data=h, dist~0+speed+speed2+speed3)
head(X0) # начало матрицы
cor(X0) # выборочные корреляции всех регрессоров

# доверительные интервалы для отдельных коэффициентов широкие
confint(model_mk)
# а сами коэффициенты незначимы:
coeftest(model_mk)

# однако попробуем спрогнозировать
# создадим набор данных с одной машиной:
nd <- data.frame(speed=10,speed2=100,speed3=1000)
# построим предиктивный интервал
predict(model_mk,newdata=nd,interval="prediction")
# и он оказывается не так уж плох :)

# для сравнения вспомним старую модель с одним регрессором
model <- lm(data=h, dist~speed)
coeftest(model) # коэффициент значим

# предиктивный интервал по модели без мультиколлинеарности
predict(model,newdata=nd,interval="prediction")

# доверительные интервалы
confint(model) # модель с одним регрессором без коллинеарности
confint(model_mk) # модель с тремя регрессорами и коллинеарностью





# Ридж и LASSO



y <- h$dist # в вектор y поместим зависимую переменную
# еще раз создадим матрицу регрессоров без свободного члена
X0 <- model.matrix(data=h, dist~0+speed+speed2+speed3)

# LASSO
# для функции реализующей алгоритм LASSO в R 
# важно, чтобы labmda шли в убывающем порядке (!!!)
lambdas <- seq(50,0.1,length=30)

# оценим LASSO регрессию
m_lasso <- glmnet(X0,y,alpha=1, lambda=lambdas)

# по горизонтали: логарифм штрафного коэффициента лямбда
# по вертикали: оценки коэффициентов
# как меняются коэффициенты с ростом штрафного коэффициента?
plot(m_lasso,xvar="lambda",label=TRUE)

# по горизонтали: доля объясненной дисперсии
# по вертикали: оценки коэффициентов
# какой долей объяснённой дисперсии мы жертвуем, 
# отказываясь от МНК оценок?
plot(m_lasso,xvar="dev",label=TRUE)
# правая граница --- это МНК (лямбда=0)
# видно, что 
# небольшая жертва в доле объясненной дисперсии (по горизонтали)
# приводит
# к существенному сокращению коэффициента 1

# по горизонтали: сумма модулей оценок коэффициентов
# по вертикали: модуль оценок коэффициентов
# какой коэффициент вносит наибольший вклад в суммарный штраф?
plot(m_lasso,xvar="norm",label=TRUE)

# коэффициенты LASSO модели
# для лямбда=0.1 и лямбда=1
coef(m_lasso,s=c(0.1,1))

# оценка ридж-регрессии:
m_rr <- glmnet(X0,y,alpha=0, lambda=lambdas)

# Можно точно так же посмотреть на результаты 
# коэффициенты ридж-модели
# для лямбда=0.1 и лямбда=1
coef(m_rr,s=c(0.1,1))

# и построить аналогичные графики
plot(m_rr,xvar="lambda",label=TRUE)
plot(m_rr,xvar="dev",label=TRUE)
plot(m_rr,xvar="norm",label=TRUE)



# Кросс-валидация позволяет выбрать оптимальное лямбда
cv <- cv.glmnet(X0,y,alpha=1)

# зависимость кросс-валидационной RSS от лямбда
plot(cv)

# для LASSO как правило используют либо
cv$lambda.min # лямбда минимизирующую кросс-валидационную RSS
cv$lambda.1se # консервативную лямбда (увеличенную на одну стандартную ошибку)

# модель соответствующая консервативной лямбда
coef(cv,s="lambda.1se")


# Метод главных компонент
h <- heptathlon # набор данных по семиборью
help(heptathlon) # он встроенный, поэтому глянем справку 
glimpse(h) # бросим взглян на набор данных
h <- select(h,-score) # удалим итоговый результат спортсмена
describe(h) # описательные статистики

# корреляционная матрица
cor(h)

# метод главных компонент с предварительной стандартизацией переменных
h.pca <- prcomp(h,scale=TRUE)

# извлекли первую главную компоненту:
pca1 <- h.pca$x[,1]
head(pca1)

# извлекли веса, с которыми переменные входят в первую главную компоненту:
v1 <- h.pca$rotation[,1]
v1

# выборочная дисперсия каждой компоненты:
summary(h.pca)
# например, первые две компоненты имеют 
# суммарную выборочную дисперсию равную 80% 
# от суммарной выборочный дисперсии 7 видов спорта

# и первая главная компонента
# действительно здорово дифференциирует спортсменов!
cor(heptathlon$score,pca1)

# выборочная дисперсия каждой компоненты на графике:
plot(h.pca)

# исходный набор данных в новых осях
# по горизонтали --- pc1
# по вертикали --- pc2
biplot(h.pca,xlim=c(-1,1))

######################
###################### 5
library("sandwich") # vcovHC, vcovHAC
library("lmtest") # тесты
library("car") # еще тесты
library("dplyr") # манипуляции с данными
library("broom") # манипуляции
library("ggplot2") # графики


## Гетероскедастичность

# в этот момент нужно установить рабочую папку
# Session - Set working directory - To source file location
# читаем данные из файла в таблицу h
h <- read.table("flats_moscow.txt", header=TRUE)

# смотрим, что всё корректно загрузилось
head(h) # носик
tail(h) # хвостик

qplot(data=h, x=totsp, y=price) # диаграмма рассеяния

# на первом шаге оценим модель с помощью МНК
# проигнорировав возможную гетероскедастичность
model <- lm(price~totsp, data=h) # простая модель парной регрессии

summary(model) # отчет по модели
coeftest(model) # тесты незначимости коэффициентов 
confint(model) # доверительные интервалы


# добавляем в исходную таблицу h прогнозы, остатки из модели model
h <- augment(model,h) 
glimpse(h) # смотрим, что добавилось в таблицу h

# строим зависимость модуля (функция abs) остатков от общей площади
qplot(data=h,totsp,abs(.resid))
# наличие любой зависимости на этом графике означает гетероскедастичность


# простая оценка ковариационной матрицы
# верная в условиях гомоскедастичности
# неверная в условиях гетероскедастичности
vcov(model) 



# робастная оценка ковариационной матрицы 
# устойчивая к гетероскедастичности
vcovHC(model,type="HC0") # формула Уайта
vcovHC(model) # современный вариант формулы Уайта "HC3"
vcovHC(model,type="HC2") # еще один вариант


# проверяем незначимость коэффициентов с помощью:
coeftest(model) # обычной оценки ковариационной матрицы

# робастной оценки ковариационной матрицы:
coeftest(model,vcov. = vcovHC(model))

# строим руками доверительные интервалы
# робастные к гетероскедастичности

# сначала сохраним таблицу с коэффициентами и робастными ст. ошибками
conftable <- coeftest(model,vcov. = vcovHC(model))

# возьмем из этой таблицы два столбика (1-ый и 2-ой) и поместим в таблицу ci
ci <- data.frame(estimate=conftable[,1],
                 se_hc=conftable[,2])
ci # глянем на таблицу ci
# добавим в ci левую и правую границу доверительного интервала
ci <- mutate(ci,left_ci=estimate-1.96*se_hc,
             right_ci=estimate+1.96*se_hc)
ci # смотрим на результат

# для сравнение доверительные интервалы
confint(model) # по формулам корректным для гомоскедастичности

# тест Бройша-Пагана
# Во вспомогательной регрессии квадраты остатков зависят от исходных регрессоров 
bptest(model)

# тест Уайта
# Во вспомогательной регрессии квадраты остатков 
# зависят от totsp и totsp^2
bptest(model, data=h, varformula = ~ totsp + I(totsp^2) )
# альтернативный вариант включить totsp и totsp^2
bptest(model, data=h, varformula = ~ poly(totsp, 2) )

# тест Голдфельда-Квандта
gqtest(model, order.by = ~totsp, data=h, fraction = 0.2)

# диаграмма рассеяния в логарифмах
qplot(data=h, log(totsp), log(price))
# визуально гетероскедастичность меньше выражена

# модель парной регрессии в логарифмах
model2 <- lm(data=h, log(price)~log(totsp))

# тест Голдфельда-Квандта для модели в логарифмах
gqtest(model2, order.by = ~totsp, data=h, fraction = 0.2)
# гетероскедастичность есть, но гораздо менее выраженная

####################
#################### 10

library("spikeslab") # регрессия пик-плато
library("ggplot2") # графики
library("dplyr") # манипуляции с данными
library("reshape2") # перевод таблиц: широкая-длинная
library("MCMCpack") # байесовский подход к популярным моделям
library("quantreg") # квантильная регрессия


####################################
# медианная и квантильная регрессия

# прочитаем данные из .txt файла
# есть заголовок, header = TRUE
# разделитель данных - табуляция, sep="\t"
# разделитель дробной части - точка, dec="."
f <- read.table("flats_moscow.txt", header=TRUE, sep="\t", dec=".")
glimpse(f) # бросим взгляд чтобы убедиться, что загрузка прошла успешно

# квантильная регрессия для квантилей 0.1, 0.5 (медианная), 0.9
model_q01 <- rq(data=f, price~totsp, tau=c(0.1,0.5,0.9))
summary(model_q01) # отчет по модели

# базовый график --- диаграмма рассеяния
base <- qplot(data=f, totsp, price)
base

# добавляем к базовому графику две линии квантильной регрессии
base_q <- base + stat_smooth(method="rq", tau=0.1, se=FALSE) + 
  stat_smooth(method="rq", tau=0.9, se=FALSE)

# добавляем к графику дележку в зависимости от того, кирпичный дом или нет
base_q + aes(colour=factor(brick))

####################################
# Регрессия пик-плато (spike and slab regression)

# переведем скорость и длину тормозного пути во встроенном наборе данных
# в привычные единицы измерения
h <- mutate(cars, speed=1.61*speed, dist=0.3*dist)

# добавим квадрат скорости и мусорную переменную
# nrow(h) --- количество строк в таблице h
h <- mutate(h, speed2=speed^2, junk = rnorm(nrow(h)))

# обычная регрессия с мусорной переменной
model_lm <- lm(data=h, dist~speed+junk)
summary(model_lm) # мусорная переменная незначима

# байесовская регрессия с мусорной переменной
# выборка из апостериорного распределения коэффициентов размера 4000
model_ss <- spikeslab(data=h, dist~speed+junk, n.iter2=4000)
print(model_ss) # отчет по модели

model_ss$summary # еще немного 

# посмотрим, равнялся ли идеально нулю каждый коэффициент бета 
# в выборке из апостериорного распределения
included_regressors <- melt(model_ss$model)

# глядим на полученный результат
head(included_regressors)
included_regressors

# апостериорные вероятности того, что коэффициент не равен нулю:

# сколько раз не был равен нулю бета при скорости / 4000 
sum( included_regressors$value==1 )/4000 
# сколько раз не был равен нулю бета при мусорной переменной / 4000
sum( included_regressors$value==2 )/4000 