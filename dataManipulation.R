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
