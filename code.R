df<-read.csv("F:\\kaggle\\5. Prudential Life Insurance Assessment\\train.csv")
test_df<-read.csv("F:\\kaggle\\5. Prudential Life Insurance Assessment\\test.csv")
summary(df)
head(df)

lapply(df,function (x) sum(is.na(x)))
lapply(test_df,function (x) sum(is.na(x)))

df1<-df
test1<-test_df
colnames(df1)
colnames(test1)
col_categ<-c("Product_Info_1", "Product_Info_2", "Product_Info_3", "Product_Info_5",
           "Product_Info_6", "Product_Info_7", "Employment_Info_2", "Employment_Info_3",
           "Employment_Info_5", "InsuredInfo_1", "InsuredInfo_2", "InsuredInfo_3", "InsuredInfo_4",
           "InsuredInfo_5", "InsuredInfo_6", "InsuredInfo_7", "Insurance_History_1", "Insurance_History_2",
           "Insurance_History_3", "Insurance_History_4", "Insurance_History_7", "Insurance_History_8",
           "Insurance_History_9", "Family_Hist_1", "Medical_History_2", "Medical_History_3","Medical_History_4", "Medical_History_5", "Medical_History_6", "Medical_History_7",
           "Medical_History_8", "Medical_History_9", "Medical_History_11", "Medical_History_12", "Medical_History_13", "Medical_History_14", "Medical_History_16", "Medical_History_17",
           "Medical_History_18", "Medical_History_19", "Medical_History_20", "Medical_History_21", "Medical_History_22", "Medical_History_23", "Medical_History_25", "Medical_History_26",
           "Medical_History_27", "Medical_History_28", "Medical_History_29", "Medical_History_30", "Medical_History_31", "Medical_History_33", "Medical_History_34", "Medical_History_35",
           "Medical_History_36", "Medical_History_37", "Medical_History_38", "Medical_History_39", "Medical_History_40", "Medical_History_41")

df1[col_categ]=lapply(df1[col_categ],factor)
test1[col_categ]=lapply(test1[col_categ],factor)
summary(df1)
summary(test1)

## Converting the dummy variables Medical_Keyword_1-48 to factor.

## Creating the indexes
str<-"Medical_Keyword"
ind<-vector(mode="character")
i<-1
for (i in 1:48)
{
  ind=c(ind,paste(str,i,sep = "_"))
}

library(dplyr)
df1<-df1 %>% mutate_each_(funs(factor), ind)
test1<- test1 %>% mutate_each_(funs(factor), ind)

df1$Response<-factor(df1$Response)

summary(df1)
summary(test1)
str(df1)


## Removing the columns having >40% NA values.
i<-1
ind<-vector(mode="character")
ind_n<-vector(mode="numeric")
for(i in 1:length(df1))
{
  if(any(is.na(df1[,i])))
  {
    if((sum(is.na(df1[,i]))/nrow(df1))>0.4)
    {
     ind<-c(ind,colnames(df1)[i])
     ind_n<-c(ind_n,i)
    }
  }
}

df1<-df1[,!(colnames(df1) %in% ind)]
test1<-test1[,!colnames(test1) %in% ind]
## df1 and test1 is dataframe with factors and without cols having 40% NA values.


# # Dropping the column as it has 628 levels
# levels(df1$Medical_History_2)
# d1<-d[,!colnames(d) %in% "Medical_History_2"]

## Now replacing the remaining NA values
## with mean in continuous, and by mode in categorical for both train and test dataset.
i<-1
for(i in 1:ncol(df1))
{
  if(any(is.na(df1[,i])))
  {
    if(is.factor(df1[,i]))
    {
      #df1[,i]<-as.numeric(df1[,i])
      t<- table(df1[,i])
      df1[,i][is.na(df1[,i])]=as.integer(names(t))[(t==max(t))]
      #df1[,i]<-factor(df1[,i])
    }
    if(is.numeric(df1[,i]))
    {
      df1[,i][is.na(df1[,i])]=mean(df1[,i],na.rm = T)
    }
  }
}
any(is.na(df1))
summary(df1)
## Now for test data NA imputations.
i<-1
for(i in 1:ncol(test1))
{
  if(any(is.na(test1[,i])))
  {
    if(is.factor(test1[,i]))
    {
      #test1[,i]<-as.numeric(test1[,i])
      t<- table(test1[,i])
      test1[,i][is.na(test1[,i])]=as.integer(names(t))[(t==max(t))]
      #test1[,i]<-factor(test1[,i])
    }
    if(is.numeric(test1[,i]))
    {
      test1[,i][is.na(test1[,i])]=mean(test1[,i],na.rm = T)
    }
  }
}

any(is.na(test1))
summary(test1)

## some more data-processing
df1$Response<-as.numeric(df1$Response)

### as the factor levels are not in order, hence need to convert first into char and then into numeric,
## and not directly into numeric, else the facto numbres will be overwritten.
test1$Product_Info_3<-as.character(test1$Product_Info_3)
test1$Product_Info_3<-as.numeric(test1$Product_Info_3)
test1$Product_Info_3=ifelse(test1$Product_Info_3==7,8,ifelse(test1$Product_Info_3==14,15,
                            ifelse(test1$Product_Info_3==25,26,ifelse(test1$Product_Info_3==35,36,
                            test1$Product_Info_3))))
test1$Product_Info_3<-as.factor(test1$Product_Info_3)

test1$Employment_Info_2<-as.character(test1$Employment_Info_2)
test1$Employment_Info_2<-as.numeric(test1$Employment_Info_2)
test1$Employment_Info_2=ifelse(test1$Employment_Info_2==8,9,ifelse(test1$Employment_Info_2==24,26,test1$Employment_Info_2))
test1$Employment_Info_2<-as.factor(test1$Employment_Info_2)

df1<-df1[!names(df1) %in% "Medical_History_2"]
test1<-test1[!names(test1) %in% "Medical_History_2"]

test1$Medical_History_33<-as.character(test1$Medical_History_33)
test1$Medical_History_33<-as.numeric(test1$Medical_History_33)
test1$Medical_History_33=ifelse(test1$Medical_History_33==2,3,test1$Medical_History_33)
test1$Medical_History_33<-as.factor(test1$Medical_History_33)

test1$Medical_History_38<-as.character(test1$Medical_History_38)
test1$Medical_History_38<-as.numeric(test1$Medical_History_38)
test1$Medical_History_38=ifelse(test1$Medical_History_38==3,2,test1$Medical_History_38)
test1$Medical_History_38<-as.factor(test1$Medical_History_38)

class(df1$Response)


library(nnet)
model<-multinom(Response~.,data = df1, family="multinomial",maxit = 1000, MaxNWts =10000000)
summary(model)

library(e1071)
model1<-svm(Response~.,data = df1)
summary(model1)

results1<-predict(model1,test1)
table(results1)

x<-cbind("Id"=test1$Id,"Response"=results)
View(x)
write.csv(x,file="submission.csv", row.names = F)



