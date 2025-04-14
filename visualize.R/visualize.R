# Changer le working directory
setwd("/Users/nil/Desktop")



# Original Netflix data frame
# avec le prix et la library size 2021 des pays
# 
# dataset entier
netflix_dataset = read.csv("netflix price in different countries.csv")

# on define les colonnes
Ncountries = netflix_dataset$Country
N_standard_prices = netflix_dataset$Cost.Per.Month...Standard....
N_library_size = netflix_dataset$Total.Library.Size

# Short Netflix DF: Countries, Standard Prices, Library dimensions
N_short_DS = data.frame(country=Ncountries,
                        prices=N_standard_prices, libsize=N_library_size)

# Short DF ordered Alphabetically, by cost, by available titles
N_Alpha_DS = N_short_DS[order(N_short_DS$country), ]
N_cost_DS = N_short_DS[order(N_short_DS$prices),]
N_lib_DS = N_short_DS[order(N_short_DS$libsize),]

# Standard Subscription prices BARPLOT
# [doesn't fully show in the preview]

barplot(N_cost_DS$prices, names.arg=N_cost_DS$country, xlab="Cost in dollars",  
        las=2, ylim=c(0,280), width=4, horiz=TRUE, cex.names=0.5, col="red3")
title("Standard subscription prices in the world", line=3)

# Importing GDP per capita table: countries, 2014

gdp_global_DS = read.csv("updated_gdp.csv", sep=";")

gdp_countries_global = 
  gdp_global_DS$GDP.per.capita..current.prices...U.S..dollars.per.capita.
gdp_2021_global = gdp_global_DS$X2021

# Gibraltar, Liechtenstein, Monaco are excluded from the following analysis 
# because of missing GDP data
missing_countries = N_short_DS$country[
  !(N_short_DS$country %in% gdp_countries_global)]

print(missing_countries)


# Creating a compact DF for GDP

gdp_short = data.frame(country=gdp_countries_global, 
                       gdp2021=round(as.numeric(gsub(",", ".",gdp_2021_global))))

# Intersection between Netflix countries and GDP


Netflix_GDP = merge(N_short_DS, gdp_short[,c("country","gdp2021")], 
                    by="country")
# filtering the NA

Netflix_GDP = na.omit(Netflix_GDP)

# basic statistics
print(summary(Netflix_GDP$gdp2021))
print(summary(Netflix_GDP$prices))


#price_per_title
prices = Netflix_GDP$prices
libsize = Netflix_GDP$libsize
price_per_title = prices/libsize

# Dataframe with the “price per title” column
Netflix_GDP = cbind(Netflix_GDP,price_per_title)

# Same DF, but ordered by price per title in order to have a descending barplot
Netflix_GDP_ppt = Netflix_GDP[order(Netflix_GDP$price_per_title), ]
Netflix_GDP_ppt


# Juliette's barplot
barplot(Netflix_GDP_ppt$price_per_title, names.arg=Netflix_GDP_ppt$country , xlab = "Cost in dollars ($)", 
        las=2, ylim=c(0,280), width=4, horiz=TRUE, cex.names=0.5, col="red3")
title("Price for one title", line=3)

var(log(Netflix_GDP_ppt$price_per_title))


# Juliette's scatterplot
plot(Netflix_GDP$price_per_title, Netflix_GDP$prices,  xlab = "Price for one title", ylab = "Subscription price",
     col = ifelse(Netflix_GDP$country =="Turkey","red", 
                  ifelse(Netflix_GDP$country =="Croatia","green",
                         ifelse(Netflix_GDP$country =="Switzerland", "blue",
                                ifelse(Netflix_GDP$country == "San Marino", "darkorchid2", 
                                       ifelse(Netflix_GDP$country == "Ireland","orange","black"))))))

legend(0.0005, 20, legend=c("Turkey", "Croatia", "Switzerland",
                            "San Marino", "Ireland"),
       col=c("red", "green", "blue", "darkorchid2", "orange"), pch=16, 
       cex=0.7, box.lty=0)

#Calcul de l'écart-type
sd(Netflix_GDP$price_per_title)
sqrt(mean((Netflix_GDP$price_per_title - mean(Netflix_GDP$price_per_title))^2))

# covariance GDP per Capita (log and non log) and Netflix prices
cov(Netflix_GDP$prices, log(Netflix_GDP$gdp2021))
cov(Netflix_GDP$prices, Netflix_GDP$gdp2021)


# correlation GDP per Capita (log and non log) and Netflix prices
cor(Netflix_GDP$prices, log(Netflix_GDP$gdp2021))
cor(Netflix_GDP$prices, Netflix_GDP$gdp2021)


# scatterplots
par(mfrow=c(1,1))

plot(Netflix_GDP$prices, Netflix_GDP$gdp2021, main="Price and GDP",
     xlab="Prices", ylab="GDP per Capita", ylog=FALSE,
     pch = 16,
     col = ifelse(Netflix_GDP$country =="Turkey","red", 
                  ifelse(Netflix_GDP$country =="Croatia","green",
                         ifelse(Netflix_GDP$country =="Switzerland", "blue",
                                ifelse(Netflix_GDP$country == "San Marino", "darkorchid2", 
                                       ifelse(Netflix_GDP$country == "Ireland","orange","black"))))))


legend(3, 100000, legend=c("Turkey", "Croatia", "Switzerland",
                           "San Marino", "Ireland"),
       col=c("red", "green", "blue", "darkorchid2", "orange"), pch=16, 
       cex=0.7, box.lty=0)


plot(Netflix_GDP$prices, log(Netflix_GDP$gdp2021), main="Price and GDP",
     xlab="Prices", ylab="GDP per Capita (log)", ylog=FALSE,
     pch = 16,
     col = ifelse(Netflix_GDP$country =="Turkey","red", 
                  ifelse(Netflix_GDP$country =="Croatia","green",
                         ifelse(Netflix_GDP$country =="Switzerland", "blue",
                                ifelse(Netflix_GDP$country == "San Marino", "darkorchid2", 
                                       ifelse(Netflix_GDP$country == "Ireland","orange","black"))))))



legend(3, 11, legend=c("Turkey", "Croatia", "Switzerland",
                       "San Marino", "Ireland"),
       col=c("red", "green", "blue", "darkorchid2", "orange"), pch=16, 
       cex=0.7, box.lty=0)


plot(Netflix_GDP$prices, Netflix_GDP$libsize, main="Price and available titles",
     xlab="Prices", ylab="Number of available titles",
     pch = 16,
     col = ifelse(Netflix_GDP$country =="Turkey","red", 
                  ifelse(Netflix_GDP$country =="Croatia","green",
                         ifelse(Netflix_GDP$country =="Switzerland", "blue",
                                ifelse(Netflix_GDP$country == "San Marino", "darkorchid2", 
                                       ifelse(Netflix_GDP$country == "Ireland","orange","black"))))))
legend(3, 7000, legend=c("Turkey", "Croatia", "Switzerland",
                         "San Marino", "Ireland"),
       col=c("red", "green", "blue", "darkorchid2", "orange"), pch=16, 
       cex=0.7, box.lty=0)
