#1
has_dog <- c("Не, но искам", 
             "Не, но искам", 
             "Не, но искам", 
             "Не, но искам", 
             "Не, но искам", 
             "Да", 
             "Не, но искам", 
             "Не и не искам", 
             "Да", 
             "Да", 
             "Не, но искам", 
             "Не и не искам", 
             "Не, но искам", 
             "Да", 
             "Не и не искам", 
             "Не, но искам", 
             "Да", 
             "Да", 
             "Да", 
             "Да", 
             "Не, но искам", 
             "Не, но искам", 
             "Не, но искам", 
             "Не, но искам", 
             "Да", 
             "Да", 
             "Да", 
             "Да", 
             "Да", 
             "Да", 
             "Не, но искам", 
             "Не, но искам", 
             "Не, но искам", 
             "Не и не искам", 
             "Да", 
             "Не и не искам", 
             "Не и не искам", 
             "Не, но искам", 
             "Не и не искам", 
             "Не, но искам", 
             "Не, но искам", 
             "Да", 
             "Не и не искам", 
             "Да", 
             "Да", 
             "Да", 
             "Не, но искам", 
             "Да", 
             "Да", 
             "Не и не искам", 
             "Да", 
             "Не, но искам", 
             "Не, но искам", 
             "Да", 
             "Не и не искам")
#честотата се вижда най-добре чрез таблици
table_has_dog <- table(has_dog)
table_has_dog

prop_table_has_dog <- prop.table(table_has_dog)
prop_table_has_dog
#графично честотното разпределение на категорийните променливи
barplot(height = table_has_dog, col = rainbow(3), main = "Имате ли куче?")

#процентно разпределение
piepercent_has_dog<- round(100*table_has_dog/sum(table_has_dog), 1)
pie(table_has_dog, labels = piepercent_has_dog, main = "Имате ли куче?", col = rainbow(length(table_has_dog)))
legend(x = "bottomleft", legend = c("Да", "Не и не искам", "Не, но искам"), cex = 0.8,
       fill = rainbow(length(table_has_dog)))

#2
dogs_count <- c(2,
                2,
                5,
                1,
                2,
                1,
                1,
                2,
                3,
                1,
                1,
                0,
                1,
                1,
                0,
                2,
                2,
                2,
                2,
                1,
                1,
                1,
                1,
                1,
                1,
                2,
                2,
                2,
                1,
                2,
                1,
                3,
                1,
                0,
                1,
                0,
                0,
                1,
                0,
                3,
                1,
                1,
                0,
                3,
                1,
                2,
                2,
                5,
                1,
                0,
                1,
                4,
                1,
                2,
                0)
boxplot(dogs_count, col = "powderblue", main = "Колко кучета бихте искали да имате?", xlab = "брой кучета") #outlier

modeFunction <- function(x) {
  res_table <- table(x)
  return(names(res_table)[res_table == max(res_table)])
}
modeFunction(dogs_count) #мода

medianFunction <- function(x) {
  x_sorted <- sort(x)
  nn <- length(x_sorted)
  
  if(nn %% 2 == 0) {
    return(mean(x_sorted[nn/2 + c(0, 1)]))
  } else {
    return(x_sorted[round(nn/2 + 0.25)])
  }
}
medianFunction(dogs_count) #медиана

summary(dogs_count) #квартили

var(dogs_count) #вариация(дисперсия)
sd(dogs_count) #sd (стандартно отклонение)

rangeFunction <- function(x) {
  max(x) - min(x)
}
rangeFunction(dogs_count) #range

IQR(dogs_count) #IQR

#хистограма за разпределението на непрекъснатите променливи
hist(dogs_count, main = "Колко кучета бихте искали да имате?", xlab = "Кучета", ylab = "Честота", col = "blue", prob = T)
abline(v = mean(dogs_count), lwd = 4, lty = 3, col = "pink")
abline(v = median(dogs_count), lwd = 4, lty = 4, col = "black")

#3
dogs_or_cats <- c("Кучета", 
                  "Кучета", 
                  "И двете", 
                  "И двете", 
                  "И двете", 
                  "Кучета", 
                  "Котки", 
                  "И двете", 
                  "И двете", 
                  "И двете", 
                  "Кучета", 
                  "И двете", 
                  "Кучета", 
                  "Кучета", 
                  "Котки", 
                  "Кучета", 
                  "Кучета", 
                  "И двете", 
                  "И двете", 
                  "Кучета", 
                  "Котки", 
                  "Кучета", 
                  "Кучета", 
                  "Кучета", 
                  "Кучета", 
                  "Кучета", 
                  "Кучета", 
                  "Кучета", 
                  "Кучета", 
                  "И двете", 
                  "Кучета", 
                  "Кучета", 
                  "И двете", 
                  "Котки", 
                  "Кучета", 
                  "Котки", 
                  "Кучета", 
                  "Котки", 
                  "Котки", 
                  "И двете", 
                  "Кучета", 
                  "Кучета", 
                  "Кучета", 
                  "Кучета", 
                  "Кучета", 
                  "Кучета", 
                  "Кучета", 
                  "Кучета", 
                  "Кучета", 
                  "Котки", 
                  "Кучета", 
                  "Кучета", 
                  "И двете", 
                  "И двете", 
                  "Котки")
#честотата се вижда най-добре чрез таблици
table_dogs_or_cats <- table(dogs_or_cats)
table_dogs_or_cats

prop_table_dogs_or_cats <- prop.table(table_dogs_or_cats)
prop_table_dogs_or_cats


barplot(height = table_dogs_or_cats, col = rainbow(3), main = "Кучета или Котки?")

piepercent_dogs_or_cats<- round(100*table_dogs_or_cats/sum(table_dogs_or_cats), 1)
pie(table_dogs_or_cats, labels = piepercent_dogs_or_cats, main = "Кучета или Котки?", col = rainbow(length(table_dogs_or_cats)))
legend(x = "bottomleft", legend = c("И двете", "Котки", "Кучета"), cex = 0.8,
       fill = rainbow(length(table_dogs_or_cats)))

#4
dog_is = c("Най-добър приятел", 
              "Най-добър приятел", 
              "Като малко дете", 
              "Най-добър приятел", "Като малко дете", 
              "Най-добър приятел", "Като малко дете", 
              "Най-добър приятел", 
              "Най-добър приятел", 
              "Най-добър приятел", 
              "Най-добър приятел", 
              "Най-добър приятел", 
              "Най-добър приятел", "Като малко дете", 
              "Обикновено животно", 
              "Като малко дете", 
              "Най-добър приятел", "Като малко дете", 
              "Като малко дете", 
              "Най-добър приятел", "Като малко дете", 
              "Най-добър приятел", "Като малко дете", 
              "Най-добър приятел", 
              "Като малко дете", 
              "Най-добър приятел", 
              "Най-добър приятел", 
              "Като малко дете", "Обикновено животно", 
              "Най-добър приятел", 
              "Най-добър приятел", 
              "Най-добър приятел", "Като малко дете", 
              "Най-добър приятел", "Като малко дете", 
              "Най-добър приятел", "Като малко дете", 
              "Най-добър приятел", "Като малко дете", 
              "Като малко дете", 
              "Най-добър приятел", 
              "Като малко дете", 
              "Най-добър приятел", "Като малко дете", 
              "Най-добър приятел", 
              "Като малко дете", 
              "Като малко дете", 
              "Обикновено животно", 
              "Обикновено животно", 
              "Най-добър приятел", "Като малко дете", 
              "Обикновено животно", 
              "Най-добър приятел", 
              "Като малко дете", 
              "Най-добър приятел", "Като малко дете", 
              "Като малко дете", 
              "Като малко дете", 
              "Като малко дете", 
              "Като малко дете", 
              "Най-добър приятел", 
              "Като малко дете", 
              "Най-добър приятел", "Като малко дете", 
              "Обикновено животно", 
              "Най-добър приятел", 
              "Обикновено животно", 
              "Най-добър приятел", "Обикновено животно", 
              "Най-добър приятел", "Като малко дете", 
              "Обикновено животно")
#честотата се вижда най-добре чрез таблици
table_dog_is <- table(dog_is)
table_dog_is

prop_table_dog_is <- prop.table(table_dog_is)
prop_table_dog_is

barplot(height = table_dog_is, col = rainbow(3), main = "За Вас кучето е:")

piepercent_dog_is<- round(100*table_dog_is/sum(table_dog_is), 1)
pie(table_dog_is, labels = piepercent_dog_is, main = "За Вас кучето е:", col = rainbow(length(table_dog_is)))
legend(x = "bottomleft", legend = c("Като малко дете", "Най-добър приятел", "Обикновено животно"), cex = 0.8,
       fill = rainbow(length(table_dog_is)))

#5
fav_dog_color <- c(rep("бял", 15), rep("златист", 16), rep("кафяв", 18), rep("сив", 10), rep("черен", 22), rep("шарен", 14))
#честотата се вижда най-добре чрез таблици
table_fav_dog_color <- table(fav_dog_color)
table_fav_dog_color

prop_table_fav_dog_color <- prop.table(table_fav_dog_color)
prop_table_fav_dog_color


barplot(height = table_fav_dog_color, col = rainbow(length(table_fav_dog_color)), main = "Любим цвят куче:")

piepercent_fav_dog_color<- round(100*table_fav_dog_color/sum(table_fav_dog_color), 1)
pie(table_fav_dog_color, labels = piepercent_fav_dog_color, main = "Любим цвят куче:", col = rainbow(length(table_fav_dog_color)))
legend(x = "bottomleft", legend = c("бял", "златист", "кафяв", "сив", "черен", "шарен"), cex = 0.8,
       fill = rainbow(length(table_fav_dog_color)))

#6
monthly_expenses <- c(1000,
                      100,
                      300,
                      700,
                      1000,
                      100,
                      200,
                      500,
                      300,
                      250,
                      0,
                      300,
                      500,
                      500,
                      300,
                      100,
                      100,
                      200,
                      400,
                      500,
                      700,
                      200,
                      500,
                      500,
                      200,
                      100,
                      100,
                      200,
                      100,
                      1000,
                      400,
                      1000,
                      500,
                      0,
                      300,
                      0,
                      0,
                      200,
                      0,
                      900,
                      900,
                      100,
                      100,
                      200,
                      100,
                      600,
                      900,
                      800,
                      100,
                      0,
                      700,
                      800,
                      400,
                      1000,
                      0)
boxplot(monthly_expenses, col = "purple", main = "Колко пари месечно бихте отделили/отделяте за кучето си?", xlab = "лв") #outlier

modeFunction <- function(x) {
  res_table <- table(x)
  return(names(res_table)[res_table == max(res_table)])
}
modeFunction(monthly_expenses) #мода

medianFunction <- function(x) {
  x_sorted <- sort(x)
  nn <- length(x_sorted)
  
  if(nn %% 2 == 0) {
    return(mean(x_sorted[nn/2 + c(0, 1)]))
  } else {
    return(x_sorted[round(nn/2 + 0.25)])
  }
}
medianFunction(monthly_expenses) #медиана

summary(monthly_expenses) #квартили

var(monthly_expenses)
sd(monthly_expenses) #sd

rangeFunction <- function(x) {
  max(x) - min(x)
}
rangeFunction(monthly_expenses) #range

IQR(monthly_expenses) #IQR

hist(monthly_expenses, main = "Колко пари месечно бихте отделили/отделяте за кучето си?", xlab = "лв", ylab = "Честота", col = "purple1", prob = T)

abline(v = mean(dogs_count), lwd = 4, lty = 3, col = "blue")
abline(v = median(dogs_count), lwd = 4, lty = 4, col = "black")
#7
most_suitable_place <- c("Къща", "Двор", 
                             "Двор", 
                             "Къща", 
                             "Къща", 
                             "Къща", "Двор", 
                             "Къща", 
                             "Двор", 
                             "Къща", 
                             "Къща", "Двор", 
                             "Апартамент", "Къща", 
                             "Къща", "Двор", 
                             "Къща", 
                             "Двор", 
                             "Къща", 
                             "Двор", 
                             "Двор", 
                             "Двор", 
                             "Двор", 
                             "Апартамент", "Къща", "Двор", 
                             "Къща", 
                             "Апартамент", "Къща", "Двор", 
                             "Къща", "Двор", 
                             "Двор", 
                             "Къща", 
                             "Двор", 
                             "Двор", 
                             "Апартамент", "Къща", 
                             "Къща", "Двор", 
                             "Двор", 
                             "Апартамент", "Къща", "Двор", 
                             "Къща", "Двор", 
                             "Апартамент", "Къща", "Двор", 
                             "Двор", 
                             "Двор", 
                             "Къща", 
                             "Двор", 
                             "Къща", 
                             "Къща", "Двор", 
                             "Двор", 
                             "Къща", 
                             "Къща", 
                             "Къща", "Двор", 
                             "Къща", "Двор", 
                             "Къща", 
                             "Апартамент", 
                             "Апартамент", "Къща", 
                             "Къща", 
                             "Апартамент", 
                             "Къща", "Двор", 
                             "Двор", 
                             "Къща", 
                             "Апартамент", 
                             "Къща", 
                             "Къща", 
                             "Двор")
#честотата се вижда най-добре чрез таблици
table_most_suitable_place <- table(most_suitable_place)
table_most_suitable_place

prop_table_most_suitable_place <- prop.table(table_most_suitable_place)
prop_table_most_suitable_place


barplot(height = table_most_suitable_place, col = rainbow(length(table_most_suitable_place)), main = "Кое е най-подходящото място за отглеждане на куче?")

piepercent_most_suitable_place<- round(100*table_fav_dog_color/sum(table_most_suitable_place), 1)
pie(table_most_suitable_place, labels = piepercent_most_suitable_place, main = "Кое е най-подходящото място за отглеждане на куче?", col = rainbow(length(table_most_suitable_place)))
legend(x = "bottomleft", legend = c("Апартамент", "Двор", "Къща"), cex = 0.8,
       fill = rainbow(length(table_most_suitable_place)))

#8
befriend_non_dog_lover <- c(rep("Зависи от човека", 30), rep("Да", 13), rep("Не", 12))
table_befriend_non_dog_lover <- table(befriend_non_dog_lover)
table_befriend_non_dog_lover


prop_table_befriend_non_dog_lover <- prop.table(table_befriend_non_dog_lover)
prop_table_befriend_non_dog_lover

barplot(height = table_befriend_non_dog_lover, col = rainbow(length(table_befriend_non_dog_lover)), main = "Бихте ли се сближили с човек, който не обича кучета?")

piepercent_befriend_non_dog_lover<- round(100*table_befriend_non_dog_lover/sum(table_befriend_non_dog_lover), 1)
pie(table_befriend_non_dog_lover, labels = piepercent_befriend_non_dog_lover, main = "Бихте ли се сближили с човек, който не обича кучета?", col = rainbow(length(table_befriend_non_dog_lover)))
legend(x = "bottomleft", legend = c("Да", "Зависи от човека", "Не"), cex = 0.8,
       fill = rainbow(length(table_befriend_non_dog_lover)))

#9
has_had_dog <- c(rep("Да", 35), rep("Не", 20))
table_has_had_dog <- table(has_had_dog)
table_has_had_dog

prop_table_has_had_dog <- prop.table(table_has_had_dog)
prop_table_has_had_dog

barplot(height = table_has_had_dog, col = rainbow(length(table_has_had_dog)), main = "Имали ли сте куче до сега?	")

piepercent_has_had_dog<- round(100*table_has_had_dog/sum(table_has_had_dog), 1)
pie(table_has_had_dog, labels = piepercent_has_had_dog, main = "Имали ли сте куче до сега?	", col = rainbow(length(table_has_had_dog)))
legend(x = "bottomleft", legend = c("Да", "Не"), cex = 0.8,
       fill = rainbow(length(table_has_had_dog)))



#-----------------------------------------------------------
#категорийна числова
df1 <- data.frame(dogs_count, has_had_dog) 
dogs_count_vs_has_had_dog <- boxplot(df1$dogs_count  ~ df1$has_had_dog, main = "Wanted dogs depending on whether a person has had dog", xlab = "has had dog", ylab = "dogs count")
said_yes <- df1$dogs_count[df1$has_had_dog == 'Да']
said_no <- df1$dogs_count[df1$has_had_dog == 'Не']

shapiro.test(said_yes) 
shapiro.test(said_no)

wilcox.test(dogs_count ~ has_had_dog, data = dogs_data, conf.int = TRUE, exact = FALSE)

#числова числова
df2 <- data.frame(dogs_count, monthly_expenses)
plot(df2$monthly_expenses, df2$dogs_count, main = "Monthly expenses related to dogs count", xlab = "Monthly expenses", ylab = "Dogs count")

rho <- round(cor(df2$monthly_expenses, df2$dogs_count), 3)
rho # 0.436 < 0.5 => Слаба корелация  между x и y

model2 <- lm(dogs_count ~ monthly_expenses, data = df2)
model2

summary(model2)
abline(model2)









