# Data Science with Market Basket Analysis

setwd()
getwd()


# Packages
library(dplyr)
library(arules)
library(arulesViz)
library(htmlwidgets)
library(writexl)
options(warn = -1)

# Load and explore dataset
data <- read.csv("dataset_bd3.csv")
View(data)

# Solving dataset line problem
even_rows <- seq(2, nrow(data), 2)
odd_rows <- seq(1, nrow(data), 2)

# Saparate the data and use the even rows (valid data rows)
df1 <- data[even_rows, ]
View(df1)

# Check if there is missing values in the first and second purchase item
sum(is.na(df1$Item01))
sum(is.na(df1$Item02))

# Check if there is missing values represented by white space
which(nchar(trimws(df1$Item01)) ==0)
which(nchar(trimws(df1$Item02)) ==0)

# Check if there is missing values represented by white space (using regular expression)
grepl("^\\s*$", df1$Item02)

# Number of distinct items
n_distinct(df1)

# Using only records where item 2 isn't null
df1_two <- df1[!grepl("^\\s*$", df1$Item02),]

# Number of distinct items
n_distinct(df1_two)

# Prepare the package by converting variables to factor type
# (variables that we will use from now on to reduce null values)
package <- df1_two

package$Item01 <- as.factor(package$Item01)
package$Item02 <- as.factor(package$Item02)
package$Item03 <- as.factor(package$Item03)
package$Item04 <- as.factor(package$Item04)
package$Item05 <- as.factor(package$Item05)
package$Item06 <- as.factor(package$Item06)

summary(package)
str(package)

split_package <- split(package$Item01,
                       package$Item02,
                       package$Item03,
                       package$Item04,
                       package$Item05,
                       package$Item06,
                       drop = FALSE)
View(split_package)

# Transactions
transactions <- as(split_package, "transactions")

# Rules inspection
inspect(head(transactions, 5))

# Checking the rules of some products: 
# 1 - Dust-Off Compressed Gas 2 pack
rules_product1 <- apriori(transactions, 
                          parameter = list(minlen = 3, sup = 0.2, conf = 0.5, target = "rules"),
                          appearance = list(rhs = "Dust-Off Compressed Gas 2 pack", default = "lhs")) 

# Filter redundant rules
rules_product1_clean <- rules_product1[!is.redundant(rules_product1)]

# Rules Inspection
inspect(head(sort(rules_product1, by = "confidence"), 5))

# Sumary
summary(rules_product1_clean)
rules_product1_clean


# 2 - HP 61 ink
rules_product2 <- apriori(transactions, 
                          parameter = list(minlen = 3, sup = 0.2, conf = 0.5, target = "rules"),
                          appearance = list(rhs = "HP 61 ink", default = "lhs")) 

# Filter redundant rules
rules_product2_clean <- rules_product2[!is.redundant(rules_product2)]

# Rules Inspection
inspect(head(sort(rules_product2, by = "confidence"), 5))

# Sumary
summary(rules_product2_clean)
rules_product1_clean


# 3 - VIVO Dual LCD Monitor Desk mount
rules_product3 <- apriori(transactions, 
                          parameter = list(minlen = 3, sup = 0.2, conf = 0.5, target = "rules"),
                          appearance = list(rhs = "VIVO Dual LCD Monitor Desk mount", default = "lhs")) 

# Filter redundant rules
rules_product3_clean <- rules_product3[!is.redundant(rules_product3)]

# Rules Inspection
inspect(head(sort(rules_product3, by = "confidence"), 5))

# Sumary
summary(rules_product3_clean)
rules_product1_clean


# Plot all 3 rules
plot(rules_product1_clean, measure = "support", shading = "confidence", method = "graph", engine = "html")
plot(rules_product2_clean, measure = "support", shading = "confidence", method = "graph", engine = "html")
plot(rules_product3_clean, measure = "support", shading = "confidence", method = "graph", engine = "html")

# Top 3 rules using "support" for Rule 1 and "confidence" for Rule 2 and 3
inspect(head(sort(rules_product1_clean, by = "support", decreasing = TRUE), 1))
inspect(head(sort(rules_product2_clean, by = "confidence", decreasing = TRUE), 1))
inspect(head(sort(rules_product3_clean, by = "confidence", decreasing = TRUE), 1))

# Saving as dataframe and saving it to disk
df_product1 <- as(rules_product1_clean, "data.frame")
View(df_product1)
write_xlsx(df_product1, "df_product1.xlsx")

df_product2 <- as(rules_product2_clean, "data.frame")
View(df_product2)
write_xlsx(df_product2, "df_product2.xlsx")

df_product3 <- as(rules_product3_clean, "data.frame")
View(df_product3)
write_xlsx(df_product1, "df_product3.xlsx")


















