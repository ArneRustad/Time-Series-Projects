install.packages("keras")
library(keras)
install_keras(method = "conda", conda = "C:\\Users\\Arne\\anaconda3\\envs\\python_env_rstudio\\python.exe")
install_keras(method = "conda", conda = "C:\\Users\\Arne\\anaconda3\\envs\\python_env_rstudio\\python.exe")
install_keras()
Sys.setenv(RETICULATE_PYTHON = "C:\\Users\\Arne\\anaconda3\\envs\\python_env_rstudio\\python.exe")
?install_keras
?conda_python
model = keras::seq
conda_version()
conda_binary()
conda_python()

use_condaenv("C:\\Users\\Arne\\anaconda3\\envs\\python_env_rstudio\\python.exe")

reticulate::use_condaenv("/your/path/to/miniconda3-4.7.12/")
reticulate::conda_create(envname = "r-reticulate")
reticulate::use_condaenv("/your/path/to/miniconda3-4.7.12/envs/r-reticulate")

# Verify if it was properly selected
reticulate::py_config()
## python:         /your/path/to/miniconda3-4.7.12/envs/r-reticulate/bin/python
## ... 

# tensorflow::install_tensorflow(version = "gpu")

# To install Keras
keras::install_keras(method = 'conda', envname = 'r-reticulate')
keras::is_keras_available()

library(reticulate)
conda_list()


library(keras)
mnist <- dataset_mnist()
x_train <- mnist$train$x
y_train <- mnist$train$y
x_test <- mnist$test$x
y_test <- mnist$test$y

# reshape
x_train <- array_reshape(x_train, c(nrow(x_train), 784))
x_test <- array_reshape(x_test, c(nrow(x_test), 784))
# rescale
x_train <- x_train / 255
x_test <- x_test / 255

model <- keras_model_sequential()
model %>% 
  layer_dense(units = 256, activation = 'relu', input_shape = c(784)) %>% 
  layer_dropout(rate = 0.4) %>% 
  layer_dense(units = 128, activation = 'relu') %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = 10, activation = 'softmax')
model
