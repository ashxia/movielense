

#Title: "Movielens Project"
#Author: "Ashraf Masadeh"
#Date: "November 23, 2020"

  
## Introduction
#This report is part of the capstone project for the HarvardX Online Program [Professional Certificate in Data Science][id] 
#The purpose of this project is to demonstrate that I acquired the skills in R language in the field of data science throughout the courses in this series , I will be creating a movie recommendation system using the MovieLens dataset. 

#[id]:https://online-learning.harvard.edu/series/professional-certificate-data-science "Title"

## Part(1/3): #Create Train and Validation Sets#
#The original datatset consists of 10M version of the movielens dataset, and beause of the limited ablitiltis to our computer we will be downloading part of it, therefore we will use the following code to generate the datasets


################################
# Create edx set, validation set
################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding")
# if using R 3.5 or earlier, use set.seed(1) instead
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


#save the and load the tables
#write.table(edx,file = 'edx.csv')
#data(edx)


## Part(2/3): #Data Exploration#
#Q1. How many rows and columns are there in the edx dataset?#
#  The follwoing small table shows the dimention of the data set, which is the number of rows and columns

edx %>% summarize(rows = nrow(edx), columns= ncol(edx)) %>% 
  select(rows, columns)

#Q2. How many zeros were given as ratings in the edx dataset?#
 # there is a number of sum(edx$rating[] == 0) movies with zero star rating 
#below is the code:
  
sum(edx$rating[] == 0)

#Q3.How many threes were given as ratings in the edx dataset?#
#  there is a number of sum(edx$rating[] == 3) movies with 3 star rating
#below is the code:
  
sum(edx$rating[] == 3)

#Q4. How many different movies are in the edx dataset?#
#there is summarize(c, n_movies = n_distinct(movieId)) movies in the edx dataset, below is the code:
  
summarize(edx, n_movies = n_distinct(movieId))

#Q5.How many different users are in the edx dataset??#
#there is a number of summarize(edx, n_users = n_distinct(userId)) users in the dataset, below is the code:
  
summarize(edx, n_users = n_distinct(userId))


#Q7. Which movie has the greatest number of ratings?#
  
edx %>% 
  group_by(movieId, title) %>%
  summarize(count = n()) %>%
  arrange(desc(count))

#Q8. What are the five most given ratings in order from most to least?#
  
edx %>% 
  group_by(rating) %>% 
  summarize(count = n()) %>% 
  arrange(desc(count))

#Q9. True or False: In general, half star ratings are less common than whole star ratings (e.g., there are fewer ratings of 3.5 than there are ratings of 3 or 4, etc.)#
  
edx %>% 
  group_by(rating) %>% 
  summarize(count = n())

## Part(3/3): RMSE Model

first we divide the edx dataset into a training and test set, we apply the model on the training set and we test the RMSE on the test set and finally we validate our final model with the validation set to reach the minimum RMSE

test_index <- createDataPartition(y = edx$rating, times = 1,
                                  p = 0.2, list = FALSE)
train_set <- edx[-test_index,]
test_set <- edx[test_index,]

#to make sure we do not include users and movies in the test set that do not appear in the trainin set

test_set <- test_set %>% 
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")




#Root Mean Square Error Loss Function
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

e <- seq(-0.5, 0.5, 0.25)

rmses <- sapply(e,function(i){
  
  #Calculate the mean of ratings from the edx training set
  mu <- mean(train_set$rating)
  
  #Adjust mean by movie effect and penalize low number on ratings
  b_i <- train_set %>% 
    group_by(movieId) %>%
    summarize(b_i = mean(rating - mu))
  
  #ajdust mean by user and movie effect 
  b_u <- train_set %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = mean(rating - b_i - mu))
  
  #predict ratings in the training set to derive optimal penalty value 'lambda'
  predicted_ratings <- 
    train_set %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = ifelse(mu + b_i + b_u + i > 5, 5,
                         ifelse ( mu + b_i + b_u + i<1, 1,mu + b_i + b_u + i ))) %>%
    .$pred
  
  return(RMSE(predicted_ratings, test_set$rating))
})

plot(e, rmses)

e <- e[which.min(rmses)]
paste('Optimal RMSE of',min(rmses),'is achieved with Lambda',e)

Apply Lamda on Validation set for Data-Export

lambda <- e

pred_y_lse <- sapply(lambda,function(i){
  
  #Derive the mearn from the training set
  mu <- mean(edx$rating)
  
  #Calculate movie effect with optimal lambda
  b_i <- edx %>% 
    group_by(movieId) %>%
    summarize(b_i = mean(rating - mu))
  
  #Calculate user effect with optimal lambda
  b_u <- edx %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = mean(rating - b_i - mu))
  
  #Predict ratings on validation set
  predicted_ratings <- 
    validation %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u + i) %>%
    .$pred #validation
  
  return(predicted_ratings)
  
})


#the final RMSE is:
RMSE(validation$rating, pred_y_lse)





