library(tidymodels)

head(iris)

## creating training and testing sets
index <- initial_split(iris)
training_data <- training(index)
testing_data <- testing(index)


#preprocess the data

data_recipe <- iris %>% 
  recipe(Species ~ Sepal.Length + Petal.Width, data = .) %>% 
  step_normalize(all_numeric_predictors())

data_recipe

## create a model specification

knn_model <- nearest_neighbor(mode = "classification",
                              engine = "kknn")


## create a model to bundle analysis and preprocessing
Iris_workflow <- workflow(preprocessor = data_recipe,
                          spec = knn_model)

## train the model and evaluate fit
Final_model <- last_fit(Iris_workflow,index)
Final_model$.metrics
Final_model$.predictions















