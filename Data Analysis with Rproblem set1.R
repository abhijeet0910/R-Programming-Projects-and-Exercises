#question 1 
#consider the auto dataset in howmany instances the resale value of the vehicle is
#above 60 percent its price.
auto_abv = auto
price60 = (auto_abv$price * 0.60)
auto_abv = cbind(auto_abv,price60)
auto_abv = na.omit(auto_abv)
auto_abv$autof = auto_abv$resale>auto_abv$price60
sum(auto_abv$autof)

auto_abv1 = auto[,c(4,6)]
auto_abv1 = na.omit(auto_abv1)
auto_abv1$price61 = (auto_abv1$price * 0.60)
auto_abv1$pricef = auto_abv1$resale>auto_abv1$price61
sum(auto_abv1$pricef)
auto60 = nrow(subset(auto,subset = resale>0.6*price,select = c("resale","price")))
power = function(n,power){
  return(n^power)
}
power(2,power=3)
a=subset(auto, se=c(4,6), resale>price*0.60)
P = subset(auto,subset = resale>price*0.60,c(4,6))
#question2
#consider auto dataset power vehicles are those for which horsepower 
#to length ratio is above 1.1. how many trucks and howmany automobiles fall under 
# power vehicle category

khp = subset(auto, subset= (horsepow/length)>1.1, c(8,5,11))
k1 = khp[khp$type =="Truck",]
table(khp$type)
table(subset(auto,subset = horsepow/length>1.1,select = "type"))
#question3
#consider autompg dataset the variable name class is actually a mileage.
#what are the average mileages of 4 and 6 cylinders engines.
autompg1 = autompg
table(autompg1$cylinders)
ka=table(autompg1$cylinders)
qwe1 = subset(autompg1, subset= (cylinders== "4" | cylinders=="6"),select= c("cylinders","class"))
krt1 = table(aggregate(class~cylinders, data=autompg1, mean))
aggregate(class~cylinders, data=subset(autompg1, subset= (cylinders== "4" | cylinders=="6"),select= c("cylinders","class")), mean)
aggregate(class~cylinders,data = subset(autompg,subset = cylinders %in% c(4,6)),mean)
#question 4
#consider autompg dataset.Assume that the engine will be considered efficient if its horsepower
#to displacement ratio is above 0.9. 
#how many such efficient vehicles are there and what is there average mileage?
k4=(subset(autompg, subset=(horsepower/displacement)>0.9,select = "class"))
mean(k4$class)
eff.vehicle=subset(autompg,subset = horsepower/displacement > 0.9)
# No of efficient vehicles
nrow(eff.vehicle)
# Mileage
mean(eff.vehicle$class)
#consider autompg dataset if only 6 cylinders are considered 
#which origin vehicles are giving highest average mileage
autom1 = autompg
autom1 = autom1[autom1$cylinders=="6",c(2,8,9)]
e1 = aggregate(class~cylinders+origin, data = autom1, mean)
aggregate(class~origin,data=subset(autompg,subset = cylinders==6),mean)

# practice
autompg2 = subset(autompg, subset = cylinders=="4"| cylinders=="6", select= c("cylinders","class"))
k234 = aggregate(class~cylinders, data = autompg2, mean)
