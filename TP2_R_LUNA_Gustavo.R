
library(readr)
library(tidyverse)

file->"https://raw.githubusercontent.com/gustavodluna/mineria_datos_tp2/main/detroit.csv"

detroit<-readr::read_csv(file=file)

dim(detroit)

head(detroit, n = 5) 
