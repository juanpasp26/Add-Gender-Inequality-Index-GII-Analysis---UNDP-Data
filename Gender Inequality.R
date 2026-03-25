#Welcome to my analysis, so I would like to put another important aspect of human rights into scope
#and that is sex equality, well as we can see recent year, many woman movements have occured around the world
#so I want to analyse it what is our situation right now
#source: https://hdr.undp.org/data-center/thematic-composite-indices/gender-inequality-index#/indicies/GII
#I will try to answer following issues:

#how is HDI affecting the equality index?
#what is the effects of the components of Gender Inequality Index to it?

options(scipen=999)
install.packages("readxl")         
library("readxl") #download the database using excel function
df <- read_excel("C:/Users/HP/Downloads/HDR25_Statistical_Annex_GII_Table.xlsx")


df$`Gender Inequality Index` = as.numeric(df$`Gender Inequality Index`)
df$`Gender Inequality Index Position`= as.numeric(df$`Gender Inequality Index Position`)
df$`Maternal mortality ratio`= as.numeric(df$`Maternal mortality ratio`)
df$`Adolescent birth rate`= as.numeric(df$`Adolescent birth rate`)

df$`Share of seats in parliament`= as.numeric(df$`Share of seats in parliament`)
df$`Female with at least some secondary education`= as.numeric(df$`Female with at least some secondary education`)
df$`Male with at least some secondary education`= as.numeric(df$`Male with at least some secondary education`)
df$`Male Labour force participation rate`= as.numeric(df$`Male Labour force participation rate`) 
df$`Female Labour force participation rate`= as.numeric(df$`Female Labour force participation rate`) 
#so without cleaning the database look extremly messy and hard to analyse, so i deleted 
#some of the unneccessary columns and rows so it would be more visible + na deleting
#I also changed all the character to numeric so it would work in analysing

boxplot(df$`Gender Inequality Index`)
#there is a low number of extreme value based on the graph
summary(df$`Gender Inequality Index`)
#as i can see minimum of the gender inequality index is 0.0030. maximum of 0.8380
#mean 0.392, 3rd quartile: 0,4905
inequality_country_group = df[df$`Gender Inequality Index`>0.4905,]
inequality_country_group = na.omit(inequality_country_group)

equality_country_group = df[df$`Gender Inequality Index`<0.1630,]
equality_country_group = na.omit(equality_country_group)

file_without_missing_val = na.omit(df)

cor(file_without_missing_val$`Gender Inequality Index`,file_without_missing_val$`HDI rank`)
#supposedly we will evaluate the correlations of GII and HDI with non missing data in other columns(NA)
#0.903 -> very strong correlation
plot(file_without_missing_val$`Gender Inequality Index`,file_without_missing_val$`HDI rank`,xlab =)
#strong correlations on the graph
#conclusion: HDI does affect the equality, the higher HDI ranking, the higher the chance of having equality.

cor(file_without_missing_val$`Male with at least some secondary education`,file_without_missing_val$`Female with at least some secondary education`)
#0.9725 super strong correlation, meaning equality between male and female exist in education

#lets see adolescent birth rate: the annual number of births in women aged 15–19 years old per 1000 women in the same respective age group
cor(file_without_missing_val$`Adolescent birth rate`,file_without_missing_val$`HDI rank`)
#0.7885 quite strong correlation
#here is tricky but if for example we increase the ranking in HDI meaning the lower HDI the higher adolescent birth rate
cor(file_without_missing_val$`Female Labour force participation rate`,file_without_missing_val$`Male Labour force participation rate`)
#0.46 weak correlation so lets see the reason
check =df[df$`Female Labour force participation rate`<43.59,]
check = na.omit(check)
#clearly some countries when having low female labour force participation rate tend to have 
#also low male female labour force but in some case it exists in high male labour force rate
#inequality exist here

#Overall what i can say it the components does strictly link to the GII in some way,either with a weak link
#high link

summary(df)
summary(file_without_missing_val)



