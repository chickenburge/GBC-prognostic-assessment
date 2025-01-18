rm(list=ls()) 
library(randomForestSRC) 
library(survival) 
library(readr) 
rm(list = ls()) 
df_train <- read_csv("C:/Users/lenovo/Desktop/r_code/6_outcome.csv") 

library(survivalsvm) 
library(randomForestSRC) 

df_train$event <- as.numeric(as.character(df_train$event)) 

class(df_train$event) 

colnames(df_train)[ncol(df_train)] 

out.rsf.1 <- rfsrc(Surv(time, event) ~ . ,
                   data = df_train,
                   ntree = 100, ## Number of trees
                   nsplit = 10) ## Number of random splits (non-negative integer)

pred <- predict(out.rsf.1,
                df_train,
                OOB=TRUE,
                type="response") 
print.rfsrc(out.rsf.1) 
plot(out.rsf.1) 
gene.vs<-var.select(object = out.rsf.1,
                    method="vh") 

randomForestSRC_geneids <- gene.vs$topvars ## Extract the variables with the largest variance from the analysis result

save(gene.vs,randomForestSRC_geneids,file = 'xxxxxxvars.csv')
