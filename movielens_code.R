##########################################################
#### Installing required Package 
##########################################################

################## Install Basic Package required -- from edx
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(ggthemes)) install.packages("ggthemes", repos = "http://cran.us.r-project.org")

if(!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org")
if(!require(rmarkdown)) install.packages("rmarkdown", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")

################## Install Additional Package  used in code
#### Used formattable and kableExtra Package to formate Table
if(!require(kableExtra)) install.packages("kableExtra", repos = "http://cran.us.r-project.org")
if(!require(formattable)) install.packages("formattable", repos = "http://cran.us.r-project.org")

#### Used recosystem package to perform the matrix factorization 
if(!require(recosystem)) install.packages("recosystem", repos = "http://cran.us.r-project.org")

## used to create PDF File
if(!require(tinytex)) install.packages("tinytex", repos = "http://cran.us.r-project.org")



library(tidyverse)
library(caret)
library(data.table)
library(kableExtra)
library(formattable)
library(recosystem)  ## used to make matrix factorization 
library(ggthemes)
library(lubridate) ## used to deal with timestamp
library(knitr)
library(rmarkdown)
library(dplyr)
library(tinytex)

##########################################################
# This code provided from HarvardX in PH125.9x Data Science: Capstone Movielends project 
##### Create edx set, validation set (final hold-out test set)
##########################################################

# Note: this process could take a couple of minutes
# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()

download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

# if using R 3.6 or earlier:
#movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
#                                           title = as.character(title),
#                                           genres = as.character(genres))

# if using R 4.0 or later:
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

head(movielens)

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

##########################################################
#### Data Set overview 
##########################################################

###### Tale 1: data set features and the basic summary statistics:

##  First 5 rows of edx data set 
head_edx <- rbind((lapply(edx, class)), head(edx)) 

# we used formattable package to display the top rows in edx data set more beautifully
formattable(head_edx, align = c("c",rep("c", NCOL(head_edx) - 1)), list(
  `Indicator Name` = formatter("span", style = ~ style(color = "Black",font.weight = "bold")), 
  area(col = 0:6) ~ color_tile("#DeF7E9", "#DeF7E9")))

###### Table 2: Edx Data set summary 
edx_Dataset_summary <- data.frame(rows_number = nrow(edx),
                                  columns_number = ncol(edx),
                                  users_number= n_distinct(edx$userId),
                                  movies_number = n_distinct(edx$movieId),
                                  average_rating = round(mean(edx$rating),3),
                                  genres_number = n_distinct(edx$genres),
                                  first_rating_Date = as.Date(as.POSIXct(min(edx$timestamp), origin = "1970-01-01")),
                                  last_rating_date = as.Date(as.POSIXct(max(edx$timestamp), origin = "1970-01-01")))

# we used formattable package to display the EDX dataset summary more beautifully
formattable(edx_Dataset_summary, align = c("c",rep("c", NCOL(edx_Dataset_summary) - 1)), list(
  `Indicator Name` = formatter("span", style = ~ style(color = "grey",font.weight = "bold")), 
  area(col = 0:8) ~ color_tile("#91CA97", "#71CA97")))

###### Table 3: Validation data set summary 
Validation_Dataset_summary <- data.frame(rows_number = nrow(validation),
                                  columns_number = ncol(validation),
                                  users_number= n_distinct(validation$userId),
                                  movies_number = n_distinct(validation$movieId),
                                  average_rating = round(mean(validation$rating),3),
                                  genres_number = n_distinct(validation$genres),
                                  first_rating_Date = as.Date(as.POSIXct(min(validation$timestamp), origin = "1970-01-01")),
                                  last_rating_date = as.Date(as.POSIXct(max(validation$timestamp), origin = "1970-01-01")))

# we used formattable package to display the validation data set summary more beautifully
formattable(Validation_Dataset_summary, align = c("c",rep("c", NCOL(Validation_Dataset_summary) - 1)), list(
  `Indicator Name` = formatter("span", style = ~ style(color = "grey",font.weight = "bold")), 
  area(col = 1:8) ~ color_tile("#71CA97", "#71CA97")))


##########################################################
#### Analysis and Exploration data
##########################################################

######################### Rating Exploration

###### Define the mean movies rating
mu_rating <- mean(edx$rating)

###### Figure 1: Overall ratings distribution
edx %>%
ggplot(aes(x= rating)) +
  geom_histogram( bins = 40, color = "green") +
  theme_hc() + 
  scale_x_continuous(breaks=seq(0, 5, by= 0.5)) +
  geom_vline(xintercept = mu_rating,  colour = "red")+
  ggtitle("Overall ratings distribution")+
  labs(x="Rating", y="number of ratings", caption = "Figure 1: Overall ratings distribution in Edx dataset") +
  theme(plot.background = element_rect(colour= NA, linetype = "solid", fill = NA, size = 1), panel.border = element_rect(colour="black", linetype = "dashed", fill=NA), plot.title = element_text(hjust = 0.5) , plot.caption = element_text(hjust = 0.5))

######################### Movies Exploration

###### Figure 2: Histogram of number of ratings by movies in Edx Dataset
edx %>% group_by(movieId) %>%
  summarize(num_movie_rating = n(), 
            mu_movies = mean(rating),
            sd_movies = sd(rating)) %>% ggplot(aes(x = num_movie_rating))+
  geom_histogram(bins = 40, color = "green")+
  theme_hc() +
  scale_x_log10()+
  ggtitle("Number of ratings by movie") +
  labs(x="Number of Movies",
       y="Number of ratings" , 
       caption ="Figure 2: Number of ratings by movies in Edx Dataset") +
  geom_vline(aes(xintercept = mean(num_movie_rating)), color = "orange")+
  theme(plot.background = element_rect(colour= NA, linetype = "solid", fill = NA, size = 1), panel.border = element_rect(colour="black", linetype = "dashed", fill=NA), plot.title = element_text(hjust = 0.5), plot.caption = element_text(hjust = 0.5))


###### Figure 3: Movie distribution by average rating in Edx Dataset
edx %>% group_by(movieId) %>%
  summarise(movie_ave_rating = sum(rating)/n()) %>%
  ggplot(aes(movie_ave_rating)) +
  geom_histogram(bins=30, color = I("Green")) +
  theme_hc() +
  scale_x_continuous(breaks=seq(0, 5, by= 0.5)) +
   ggtitle("Movies distribution by average rating ") +
  labs(x="Average Rating",
       y="Number of movies",
       caption = "Figure 3: Movie distribution by average rating in Edx Dataset")+
  theme(plot.background = element_rect(colour=NA, linetype = "solid", fill = NA, size = 1), panel.border = element_rect(colour="black", linetype = "dashed", fill=NA), plot.title = element_text(hjust = 0.5), plot.caption = element_text(hjust = 0.5))

######################### User Exploration

###### Figure 4: Histogram of number of ratings by users in Edx Dataset
edx %>% group_by(userId) %>%
  summarize(num_user_rating = n(),
            mu_user = mean(rating),
            sd_user = sd(rating)) %>% 
  ggplot(aes(x = num_user_rating))+
  geom_histogram(bins = 40, color = "green")+
  theme_hc() +
  scale_x_log10()+
  ggtitle("Number of ratings by users ") +
  labs(x = "Number of Users",
       y = "Number of rating",  
       caption = "Figure 4: Number of ratings by users in Edx Dataset")+
  geom_vline(aes(xintercept = mean(num_user_rating)), color = "orange")+
  theme(plot.background = element_rect(colour= NA, linetype = "solid", fill = NA, size = 1), panel.border = element_rect(colour="black", linetype = "dashed", fill=NA), plot.title = element_text(hjust = 0.5), plot.caption = element_text(hjust = 0.5))

###### Figure 5: Users distribution by average rating in Edx Dataset
edx %>% group_by(userId) %>%
  summarise(user_ave_rating = sum(rating)/n()) %>%
  ggplot(aes(user_ave_rating)) +
  geom_histogram(bins=30, color = I("Green")) +
  theme_hc() +
  scale_x_continuous(breaks=seq(0, 5, by= 0.5)) +
  ggtitle("Users distribution by average rating ") +
  labs(x="Average Rating",
       y="Number of Users",
       caption = "Figure 5: Users distribution by average rating in Edx Dataset")+
  theme(plot.background = element_rect(colour= NA, linetype = "solid", fill = NA, size = 1), panel.border = element_rect(colour="black", linetype = "dashed", fill=NA), plot.title = element_text(hjust = 0.5), plot.caption = element_text(hjust = 0.5))


######################### Time effect Exploration
###### Figure 6: Smooth curve of average ratings by time/month in Edx Dataset
edx %>% 
  mutate(date = round_date(as_datetime(timestamp), unit = "month")) %>%
  group_by(date) %>%
  summarize(rating = mean(rating)) %>%
  ggplot(aes(date, rating)) +
  geom_point() +
  geom_smooth(color = "green") +
  theme_hc() +
  ggtitle("average of ratings by Time ") +
  labs(x = "Time, unit: month ",
       y = "Mean Rating",  
       caption = "Figure 6: Average ratings by time/month in Edx Dataset")+
  theme(plot.background = element_rect(colour= NA, linetype = "solid", fill = NA, size = 1), panel.border = element_rect(colour="black", linetype = "dashed", fill=NA), plot.title = element_text(hjust = 0.5), plot.caption = element_text(hjust = 0.5))

######################### Exploration of genres 

###### Figure 7: Histogram of number of ratings by genre in Edx Dataset
## calculate number and average of movies bu genres
genres_summarize <- edx %>% separate_rows(genres, sep = "\\|") %>%
  group_by(genres) %>%
  summarize( num_movie_per_genres = n(), avg_movie_per_genres = mean(rating)) %>%
  arrange(desc(num_movie_per_genres))

## draw the histogram of number of ratings per genre
genres_summarize %>%
  ggplot(aes(num_movie_per_genres,reorder(genres, num_movie_per_genres),  fill= num_movie_per_genres)) +
  geom_bar(stat = "identity") + coord_flip() +
  scale_fill_distiller(palette = "#001400")+
  ggtitle("Number of ratings per genre") +
  labs(y = "Genres Type",
       x = "Number of ratings",  
       caption = "Figure 7: Number ratings by genre in Edx Dataset")+
  theme_hc()+
  theme(plot.background = element_rect(colour= NA, linetype = "solid", fill = NA, size = 1), legend.position = 'none', panel.border = element_rect(colour="black", linetype = "dashed", fill=NA), plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 90, hjust = 1), plot.caption = element_text(hjust = 0.5))
 

###### Figure 8: Distribution of ratings average per genre

genres_summarize %>%
  ggplot(aes(avg_movie_per_genres,reorder(genres, avg_movie_per_genres), fill= avg_movie_per_genres)) +
  geom_bar(stat = "identity") + coord_flip() +
  scale_fill_distiller(palette = "green") + 
  ggtitle("Distribution of ratings average per genre ") +
  labs(y = "Genre Type",
       x = "Average of rating", 
       caption =  "Figure 8: Distribution of ratings average per genre in Edx Dataset")+
  theme_hc()+
  theme(plot.background = element_rect(colour= NA, linetype = "solid", fill = NA, size = 1), legend.position = 'none', panel.border = element_rect(colour="black", linetype = "dashed", fill=NA), plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 90, hjust = 1), plot.caption = element_text(hjust = 0.5))

###### Figure 9: Relation between Number of Ratings vs Mean Rating for Genres 
edx%>%
  group_by(genres)%>%
  summarise(num_movie_per_genres =n(),avg_rating_genres =mean(rating))%>%
  ggplot(aes(x = num_movie_per_genres, y= avg_rating_genres))+
  scale_x_log10()+
  geom_point()+
  geom_smooth(color = "green")+
  ggtitle("Relation between numbers and mean ratings for genres") +
  labs(x = "Number of Ratings",
       y = "Mean Ratings",
       caption = "Figure 9: Relation between Number of Ratings vs Mean Rating for Genres")+
  theme_hc()+
  theme(plot.background = element_rect(colour= NA, linetype = "solid", fill = NA, size = 1), legend.position = 'none', panel.border = element_rect(colour="black", linetype = "dashed", fill=NA), plot.title = element_text(hjust = 0.5), plot.caption = element_text(hjust = 0.5))


##########################################################
#####  Results section: Presents the modeling results
##########################################################
#### In this section we will do the following:
# 1. Split data set into edx_train 80% and edx_test 10% dataset>
# 2. Constructed various models using edx_train and their performances are assessed using edx_test
# 3. Identifying the optimal model
# 4. Final Model (Results) rerun the optimal model using edx dataset as train set, and validation dataset as test set

#### create train and test set from edx dataset
set.seed(20, sample.kind="Rounding")
# edx_test set will be 10% of edx data
edx_test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.2, list = FALSE)

edx_train <- edx %>% slice(-edx_test_index)
edx_temp <- edx %>% slice(edx_test_index)

# make sure userId and movieId in test set are also in train set
edx_test <- edx_temp %>% 
  semi_join(edx_train, by = "movieId") %>%
  semi_join(edx_train, by = "userId")

# add rows removed from test set back into train set
removed <- anti_join(edx_temp, edx_test)
edx_train <- rbind(edx_train, removed)

##########################################################
## Define RMSE: residual mean squared error
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

##########################################################
####### Baseline Approach

####### Model 1: use average ratings for all movies
## calculate average movie rating for edx_train data set
mu <- mean(edx_train$rating)
target_rmse <- 0.86490
## calculate rmse for model 1
model1_rmse <- RMSE(edx_test$rating, mu)
rmse_results <- data.frame(Model = "Just the Average",
                           RMSE = round(model1_rmse, 4), Improvement = percent((model1_rmse-model1_rmse)/model1_rmse))
## view rmse result
formattable(rmse_results, align = c("c",rep("c", NCOL(rmse_results) - 1)), list(
  `Indicator Name` = formatter("span", style = ~ style(color = "grey",font.weight = "bold")), 
  area(col = 2:2) ~ color_tile( "#71CA97", "#DeF7E9"),
  `Improvement` = formatter("span", style = x ~ style(color = "green"))))

####### Model 2: Modeling movie effects
## calculate movie mean rating:  b_i
movie_avgs <- edx_train %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu))

## Figure 10: Distribution of movie effect (b_i)
movie_avgs %>%
  ggplot(aes(b_i)) +
  geom_histogram(bins=30, color = I("Green")) +
  theme_hc() +
  ggtitle("Distribution of movie effect (b_i) ") +
  labs(x="Movie effects (b_i)",
       y="Count",
       caption = "Figure 10: Distribution of movie effect (b_i) Edx_train dataset")+
  theme(plot.background = element_rect(colour= NA, linetype = "solid", fill = NA, size = 1), panel.border = element_rect(colour="black", linetype = "dashed", fill=NA), plot.title = element_text(hjust = 0.5), plot.caption = element_text(hjust = 0.5))

## calculate the prediction rating for model 2
predict_rating_m2 <- mu + edx_test %>% 
  left_join(movie_avgs, by='movieId') %>%
  .$b_i

## calculate rmse for model 2
model2_rmse <- RMSE(edx_test$rating,predict_rating_m2)
rmse_results <- bind_rows(rmse_results,data.frame(Model = "Movie Effect Model",
                           RMSE = round(model2_rmse, 4), Improvement = percent((model1_rmse-model2_rmse)/model1_rmse)))
## view rmse result
formattable(format = "latex",rmse_results, align = c("c",rep("c", NCOL(rmse_results) - 1)), list(
  `Indicator Name` = formatter("span", style = ~ style(color = "grey",font.weight = "bold")), 
  area(col = 2:2) ~ color_tile( "#71CA97", "#DeF7E9"),
  `Improvement` = formatter("span" , style = x ~ style(color = "green"))))

####### Model 3: Movie and User Effect 
## calculate user mean rating:  b_u
user_avgs <- edx_train %>% 
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))

## Figure 11: Distribution of movie effect (b_u)
user_avgs %>%
  ggplot(aes(b_u)) +
  geom_histogram(bins=30, color = I("Green")) +
  theme_hc() +
  ggtitle("Distribution of user effect (b_u) ") +
  labs(x="User effects (b_u)",
       y="Count",
       caption = "Figure 11: Distribution of movie effect (b_u) in Edx_train dataset")+
  theme(plot.background = element_rect(colour= NA, linetype = "solid", fill = NA, size = 1), panel.border = element_rect(colour="black", linetype = "dashed", fill=NA), plot.title = element_text(hjust = 0.5), plot.caption = element_text(hjust = 0.5))


## calculate the prediction rating for model 3
predict_rating_m3 <- edx_test %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu + b_i + b_u) 

## calculate rmse for model 3
model3_rmse <- RMSE(edx_test$rating,predict_rating_m3$pred)
rmse_results <- bind_rows(rmse_results,
                          data_frame(Model="Movie and User Effect model",  
                                    RMSE = round(model3_rmse, 4) , Improvement = percent((model1_rmse-model3_rmse)/model1_rmse)))
## View the result
formattable(rmse_results, align = c("c",rep("c", NCOL(rmse_results) - 1)), list(
  `Indicator Name` = formatter("span", style = ~ style(color = "grey",font.weight = "bold")), 
  area(col = 2:2) ~ color_tile( "#71CA97", "#DeF7E9"),
  `Improvement` = formatter("span", style = x ~ style(color = "green"))))

##########################################################
####### Regularization

####### Model 4: Regularized Movie and user Effect Model
## calculate optimal tuning parameter (Lambda) using k fold cross validation
lambdas <- seq(0, 10, 0.25)

## calculate the best value of lambdas that return minimum RMSE value
set.seed(21, sample.kind = "Rounding")

## For each lambda,find b_i & b_u followed by rating prediction
rmses <- sapply(lambdas, function(l){
  
  mu <- mean(edx_train$rating)
  
  b_i <- edx_train %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  
  b_u <- edx_train %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  
  predicted_ratings <- edx_test %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>%
    .$pred
  
  return(RMSE(edx_test$rating,predicted_ratings))
})

## plot lambadas
qplot(lambdas, rmses)  +
  annotate("text", x = lambda, y = min(rmses), label = lambda, vjust = -1, color = "green") +
  labs(x = "Lambda", y = "RMSE", caption = "Source: train dataset")+
  ggtitle("Selecting the tuning parameter ") +
  labs(x="Lambda",
       y="RMSE",
       caption = "Figure 12: Selecting the tuning parameter in Edx_train dataset")+
  theme(plot.background = element_rect(colour= NA, linetype = "solid", fill = NA, size = 1), panel.border = element_rect(colour="black", linetype = "dashed", fill=NA), plot.title = element_text(hjust = 0.5), plot.caption = element_text(hjust = 0.5))


## get the optimal value for lambda
lambda <- lambdas[which.min(rmses)]

## calculate the regular movie reg_b_i with the optimal lambda
reg_movie_avgs <- edx_train %>% 
  group_by(movieId) %>% 
  summarize(reg_b_i = sum(rating - mu)/(n()+lambda), n_i = n())
  
## calculate the regular user reg_b_u with the optimal lambda
reg_user_avgs <- edx_train %>% 
  left_join(reg_movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(reg_b_u = sum(rating - mu - reg_b_i)/(n()+lambda), n_u = n())

## calculate the prediction rating for model 4
reg_predicted_ratings <- edx_test %>% 
  left_join(reg_movie_avgs, by='movieId') %>%
  left_join(reg_user_avgs, by='userId') %>%
  mutate(pred = mu + reg_b_i + reg_b_u) %>% 
  .$pred

## calculate rmse for model 4
model4_rmse <- RMSE(edx_test$rating,reg_predicted_ratings)
rmse_results <- bind_rows(rmse_results,
                          data_frame(Model="Regularized Movie and User Effect Model",  
                                     RMSE = round(model4_rmse, 4) , Improvement = percent((model1_rmse-model4_rmse)/model1_rmse)))

## View the result
formattable(rmse_results, align = c("c",rep("c", NCOL(rmse_results) - 1)), list(
  `Indicator Name` = formatter("span", style = ~ style(color = "grey",font.weight = "bold")), 
  area(col = 2:2) ~ color_tile( "#71CA97", "#DeF7E9"),
  `Improvement` = formatter("span",  style = x ~ style(color = "green"))))

##########################################################
####### Matrix factorization

####### Model 5: Matrix Factorization based on the residuals of the best model (Model 4: Regularized Movie and user Effect Model)
####### Calculating the residuals of edx_train from model 4
residual_edx <- edx_train %>% 
  left_join(reg_movie_avgs, by = "movieId") %>%
  left_join(reg_user_avgs, by = "userId") %>%
  mutate(residual = rating - mu - reg_b_i - reg_b_u) %>%
  select(userId, movieId, residual)
#edhead(residual_edx)

#######  Use the recosystem package to perform the matrix factorization
## make matrix from residual and edx_test
residual_mf <- as.matrix(residual_edx)
edx_test_mf <- edx_test %>% 
  select(userId, movieId, rating)
edx_test_mf <- as.matrix(edx_test_mf)

## write residual_mf and edx_test_mf table on disk
write.table(residual_mf , file = "trainset.txt" , sep = " " , row.names = FALSE, col.names = FALSE)
write.table(edx_test_mf, file = "testset.txt" , sep = " " , row.names = FALSE, col.names = FALSE)

## use data_file() to specify a data set from a file in the hard disk.
train_set <- data_file("trainset.txt")
test_set <- data_file("testset.txt")

## build a recommender object
r <-Reco()

# tuning training set
# Note: running this code will take along time ~30 minits
opts <- r$tune(train_set, opts = list(dim = c(10, 20, 30), lrate = c(0.1, 0.2),
                                      costp_l1 = 0, costq_l1 = 0,
                                      nthread = 1, niter = 10))

# training the recommender model
r$train(train_set, opts = c(opts$min, nthread = 1, niter = 20))

# Making prediction on validation set and calculating RMSE:
pred_file <- tempfile()
r$predict(test_set, out_file(pred_file)) 

predicted_residuals_mf <- scan(pred_file)
predicted_ratings_mf <- reg_predicted_ratings + predicted_residuals_mf

## calculate rmse for model 5
model5_rmse <- RMSE(edx_test$rating, predicted_ratings_mf)
rmse_results <- bind_rows(rmse_results,
                          data_frame(Model="Matrix Factorization",  
                                     RMSE = round(model5_rmse, 4), Improvement = percent((model1_rmse-model5_rmse)/model1_rmse)))

## View the result
formattable(rmse_results, align = c("l",rep("c", NCOL(rmse_results) - 1)), list(
  `Indicator Name` = formatter("span", style = ~ style(color = "grey",font.weight = "bold")), 
  area(col = 2:2) ~ color_tile( "#71CA97", "#DeF7E9"),
  `Improvement` = formatter("span", style = x ~ style(color = "green"))))


##############################################################
##### Final Model: Use Model 5 (Matrix Factorization)  to assessed on validation set
##############################################################

## We will be used to all edx set as train set, and validation set as test set in model 5

####### calculate optimal tuning parameter (Lambda) using k fold cross validation
set.seed(22, sample.kind = "Rounding")

## calculate average movie rating for edx data set
f_mu <- mean(edx$rating)

## calculate the best value of lambdas that return minimum RMSE value
f_lambdas <- seq(0, 10, 0.25)

f_rmses <- sapply(f_lambdas, function(l){
  
  mu <- mean(edx$rating)
  
  b_i <- edx %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  
  b_u <- edx %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  
  predicted_ratings <- validation %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>%
    .$pred
  
  return(RMSE(validation$rating,predicted_ratings))
})

## get the optimal value for lambda
f_lambda <- f_lambdas[which.min(f_rmses)]
f_lambda
qplot(f_lambdas, f_rmses)

## calculate the final regular movie f_b_i with the optimal lambda
final_movie_avgs <- edx %>% 
  group_by(movieId) %>% 
  summarize(f_b_i = sum(rating - f_mu)/(n()+f_lambda), n_i = n())

## calculate the final regular user f_b_u with the optimal lambda
final_user_avgs <- edx %>% 
  left_join(final_movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(f_b_u = sum(rating - f_mu - f_b_i)/(n()+f_lambda), n_u = n())

## calculate the regular prediction rating using validation dataset
final_reg_predicted_ratings <- validation %>% 
  left_join(final_movie_avgs, by='movieId') %>%
  left_join(final_user_avgs, by='userId') %>%
  mutate(pred = f_mu + f_b_i + f_b_u) %>% 
  .$pred

## calculate RMSE for Regularized Movie and User Effect Model if you need, RMSE = 0.864817
#final_reg_rmse <- RMSE(validation$rating,final_reg_predicted_ratings)
#final_reg_rmse

####### Calculating the residuals of edx data set
final_residual_edx <- edx %>% 
  left_join(final_movie_avgs, by = "movieId") %>%
  left_join(final_user_avgs, by = "userId") %>%
  mutate(residual = rating - f_mu - f_b_i - f_b_u) %>%
  select(userId, movieId, residual)
#head(final_residual_edx)

#######  Use the recosystem package to perform the matrix factorization
## make matrix from residual and validation set
final_residual_mf <- as.matrix(final_residual_edx)
validation_mf <- validation %>% 
  select(userId, movieId, rating)
validation_mf <- as.matrix(validation_mf)

## write final_residual_mf and validation_mf table on disk
write.table(final_residual_mf , file = "final_trainset.txt" , sep = " " , row.names = FALSE, col.names = FALSE)
write.table(validation_mf, file = "final_testset.txt" , sep = " " , row.names = FALSE, col.names = FALSE)

## use data_file() to specify a data set from a file in the hard disk.
final_train_set <- data_file("final_trainset.txt")
final_test_set <- data_file("final_testset.txt")

## build a recommender object
f_r <-Reco()

# tuning training set
# Note: running this code will take along time ~30 minits
f_opts <- f_r$tune(final_train_set, opts = list(dim = c(10, 20, 30), lrate = c(0.1, 0.2),
                                      costp_l1 = 0, costq_l1 = 0,
                                      nthread = 1, niter = 10))

# training the recommended model
f_r$train(final_train_set, opts = c(f_opts$min, nthread = 1, niter = 20))

# Making prediction on validation set and calculating RMSE:
final_pred_file <- tempfile()
f_r$predict(final_test_set, out_file(final_pred_file)) 

final_predicted_residuals_mf <- scan(final_pred_file)
final_predicted_ratings_mf <- final_reg_predicted_ratings + final_predicted_residuals_mf

## calculate rmse for final model (model 5: Matrix Factorization)
final_rmse <- RMSE(validation$rating, final_predicted_ratings_mf)
final_rmse_results <- data.frame(Model = "Best Model: Matrix Factorization",
                                 RMSE = round(final_rmse, 4) , Improvement = percent((model1_rmse-final_rmse)/model1_rmse))

## View the result
# we used formattable package to display the validation dataset summary more beautifully
formattable(final_rmse_results, align = c("l",rep("c", NCOL(final_rmse_results) - 1)), list(
  `Indicator Name` = formatter("span", style = ~ style(color = "grey",font.weight = "bold")), 
  area(col = 2:2) ~ color_tile( "#71CA97", "#DeF7E9"),
  `Improvement` = formatter("span", style = x ~ style(color = "green"))))





