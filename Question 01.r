#1a.	Think about what could be going wrong with our calculation. Think about a better way to evaluate this data. 

#Ans) his calculation tries to evaluate the AOV across all 100 shops over the 30 day window by averageing the order_amount column. This results in the high $3145.13 value.
#To obtain the AOV, we divide the total revenue by the total number of orders. Doing this for each shop and getting the average, we have


install.packages("xlsx")
install.packages("magrittr")
install.packages("dplyr")
install.packages("ggplot2")
library(xlsx)
library(readxl)
library(dplyr)
library(magrittr)
library(ggplot2)
excel_file <- read_excel("2019 Winter Data Science Intern Challenge Data Set.xlsx")

df <- data.frame(excel_file)

df %>%
  summary()

summary(df$order_amount)

df %>% 
  group_by(shop_id) %>% 
  mutate(sneaker_price = order_amount/total_items) %>%
  summarise(sneaker_price = mean(sneaker_price)) %>% 
  select(shop_id, sneaker_price) %>% 
  arrange((desc(sneaker_price))
          

          
aov_per_shop <- df %>% 
  group_by(shop_id) %>% 
  summarise(total_revenue = sum(order_amount),
            total_orders = sum(total_items)) %>% 
  transmute(shop_id = shop_id,
            aov_per_shop = total_revenue/total_orders) %>% 
  arrange(desc(aov_per_shop))
aov_per_shop


          
ggplot(aov_per_shop) +
  aes(x = "", y = aov_per_shop) +
  geom_boxplot(shape = "circle") +
  coord_trans(y = "log10") +
  scale_y_continuous(breaks=c(50,100, 150, 200, 350, 500, 20000)) +
  ylab("AOV per shop") +
  xlab("") +
  theme_bw()


#1b.	What metric would you report for this dataset?

#Ans) I would report the modal order value (sneaker prices) as this is safer than the omission method and reduces the effect of outliers. 
#Hence it gives a more accurate description of the value of each order across all 100 shops.


getmode <- function(x) {
   uniqx <- unique(x)
   uniqx[which.max(tabulate(match(x, uniqx)))]
}

aov_all <-  df %>% 
  group_by(shop_id) %>% 
  summarise(total_revenue = sum(order_amount),
            total_orders = sum(total_items)) %>% 
  transmute(shop_id = shop_id,
            aov_per_shop = total_revenue/total_orders) %>% 
  arrange(desc(aov_per_shop)) %>% 
  ungroup() %>% 
  summarise(median_ov = median(aov_per_shop),
            mode_ov = getmode(aov_per_shop))
aov_all

#1c.	What is its value?

#Ans) Its value is $153


aov_v2 <-  df %>% 
  group_by(shop_id) %>% 
  filter(shop_id != 78) %>% 
  summarise(total_revenue = sum(order_amount),
            total_orders = sum(total_items)) %>% 
  transmute(shop_id = shop_id,
            aov_per_shop = total_revenue/total_orders) %>% 
  arrange(desc(aov_per_shop)) %>% 
  ungroup() %>% 
  summarise(aov_v2 = mean(aov_per_shop))
aov_v2
