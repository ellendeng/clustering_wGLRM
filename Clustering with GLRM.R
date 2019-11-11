library(tidyverse)
library(lubridate)
install.packages('h2o')
library(h2o)
library('data.table')
library(caret)
library(purrr)
#install.packages('factoextra')
library('factoextra')

customer = read.csv(file.choose())
glimpse(customer)
summary(customer)

# random select a subset for modelling
set.seed(99)
dat_s = customer %>% na.omit() %>% sample_n(10000)

# select variables to include in the model
dat_m = dat_s %>% select(user_id,product_preference, food_preference, channel_credit,first_order_source,first_order_medium,first_order_type,days_to_conversion,first_order_date,first_order_total_revenue,X90d_total_revenue)

# using GLRM for dimention reduction

h2o.init()
dat_m.hex = as.h2o(dat_m, "dat_m.hex")
m_glrm = h2o.glrm(training_frame = dat_m.hex, transform = 'NORMALIZE',
                  cols = 2:ncol(dat_m),
                  k = 2,
                  seed = 123,
                  max_iterations = 100)
plot(m_glrm)

X.m = as.data.table(h2o.getFrame(m_glrm@model$representation_name))

# k-mean clustering

tot_withinss = map_dbl(1:10, function(k)
  {
  model=kmeans(x = X.m, centers = k)
  model$tot.withinss
  })

elbow_df <- data.frame(k = 1:10, tot_withinss = tot_withinss)

ggplot(elbow_df, aes(x = k, y = tot_withinss)) + geom_line() +
  scale_x_continuous(breaks = 1:10)

set.seed(45)
model_km = kmeans(X.m, 3, nstart = 25)
fviz_cluster(model_km, data = X.m, geom = 'point')

clust_customers = model_km$cluster
segment_customer = mutate(dat_s, Cluster = clust_customers) 
segment_customer$month = factor(month(segment_customer$first_order_date))
segment_customer$Arch1 = X.m$Arch1
segment_customer$Arch2 = X.m$Arch2

segment_customer %>% group_by(Cluster) %>% summarise(Avg_predicted_ltv = round(mean(predicted_total_ltv),0),
                                                         Avg_days_to_conversion = round(mean(days_to_conversion),0),
                                                         Avg_first_order_revenue = round(mean(first_order_total_revenue),1),
                                                         Avg_x90d_revenue = round(mean(X90d_total_revenue),1),
                                                         Number_of_customer = n()) %>% arrange(desc(Avg_predicted_ltv))

segment_customer %>% filter(Cluster == 3) %>% group_by(channel_credit,month) %>% 
  summarise(Avg_predicted_ltv = round(mean(predicted_total_ltv),0),
            Avg_days_to_conversion = round(mean(days_to_conversion),0),
            Avg_first_order_revenue = round(mean(first_order_total_revenue),1),
            Avg_x90d_revenue = round(mean(X90d_total_revenue),1),
            Number_of_customer = n()) %>% arrange(desc(Avg_predicted_ltv)) %>% 
  ggplot(aes(x = month,y=Avg_predicted_ltv)) + geom_point() + facet_wrap(~channel_credit) + ylim(c(0,400))

segment_customer %>% group_by(product_preference, Cluster) %>% 
  ggplot(aes(x = Arch1,y=Arch2, color=factor(Cluster))) + geom_text(aes(label = product_preference))

segment_customer %>% filter(Cluster == 3) %>% group_by(food_preference, channel_credit) %>% 
  summarise(Avg_predicted_ltv = round(mean(predicted_total_ltv),0),
            Avg_days_to_conversion = round(mean(days_to_conversion),0),
            Avg_first_order_revenue = round(mean(first_order_total_revenue),1),
            Avg_x90d_revenue = round(mean(X90d_total_revenue),1),
            Number_of_customer = n()) %>% arrange(desc(Avg_predicted_ltv)) %>% 
  ggplot(aes(x = channel_credit,y=Avg_predicted_ltv,color=food_preference)) + geom_point()

segment_customer %>% filter(Cluster == 3) %>% group_by(product_preference,food_preference,Cluster) %>% 
  summarise(Avg_predicted_ltv = round(mean(predicted_total_ltv),0),
            Avg_days_to_conversion = round(mean(days_to_conversion),0),
            Avg_first_order_revenue = round(mean(first_order_total_revenue),1),
            Avg_x90d_revenue = round(mean(X90d_total_revenue),1),
            Number_of_customer = n()) %>% arrange(desc(Avg_predicted_ltv)) %>% 
  ggplot(aes(x = food_preference,y=product_preference, size=Avg_predicted_ltv)) + geom_point() 

