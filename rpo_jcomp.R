
working_directory <- "D:\\Semester 5\\8. FOUNDATION OF DATA ANALYTICS\\DA_JComp\\";
data <- read.csv(paste0(working_directory, "FashionDataset.csv"))


print(is.nan(data$SellPrice));

print(data$MRP=="Nan")

na.omit(data);
print(data[c(1:20),]);


library(dplyr)
df <- data[,c(2:8)]
df %>% group_by(BrandName) %>% summarise(total_Sales = sum(SellPrice), 
                                         .groups = 'drop')

df <- df %>% filter(Deatils != "Nan" & MRP != "Nan")

colnames(df) <- c("BrandName","Details","Sizes","MRP_Rs",
                  "SellPrice_Rs","Discount_Percent","Category")

for(i in df$MRP_Rs){
  df$MRP_Rs[df$MRP_Rs == i] <- (strsplit(i,"\n")[[1]][2])
}


for(i in df$Discount_Percent){
  df$Discount_Percent[df$Discount_Percent == i] <- (strsplit(i,"%")[[1]][1])
}

df <- transform(df, MRP_Rs = as.numeric(MRP_Rs))
df <- transform(df, Discount_Percent = as.numeric(Discount_Percent))

#install.packages("janitor")
library(janitor)

brandname_df <- tabyl(df, BrandName)
brandname_df <- arrange(brandname_df, desc(n))
Total_sales_df <- df %>% group_by(BrandName)  %>%
  summarise(total_sales = sum(SellPrice_Rs),.groups = 'drop')
brandname_df["Total_selling_price"]=Total_sales_df$total_sales;
print(brandname_df)

barplot(brandname_df$Total_selling_price,
        main = "Maximum Temperatures in a Week",
        xlab = "Degree Celsius",
        ylab = "Day",
        names.arg = brandname_df$BrandName,
        col = "darkred")


clearanceDates <- sample(seq(as.Date('2022/12/01'), as.Date('2022/12/30'),
                           by="day"), nrow(brandname_df), replace = TRUE)
brandname_df['Clearance_Date'] <- clearanceDates

sellingDates <- sample(seq(as.Date('2022/06/01'), Sys.Date(), by="day"), 
                       nrow(df), replace = TRUE)
df['SellingDate'] <- sellingDates

df

cor.test(df$SellPrice_Rs, df$Discount_Percent, method="pearson")

library("ggpubr")
ggscatter(df, x = "Discount_Percent", y = "SellPrice_Rs", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Discount %", ylab = "Sell Price(in Rs)")+
  geom_smooth(formula = y ~ x, method = "lm")

count_dates <- table(df$SellingDate)
date_count_df <- as.data.frame(count_dates)
date_count_df

library(lubridate)
hd=df[,c(2,4:6,8)]
for(a in 1:length(df)){
  if(hd$MRP_Rs[a]>=500){
    hd$MRP_Rs[a]=floor(runif(1, min=hd$MRP_Rs[a]-hd$MRP_Rs[a]*0.5, max=hd$MRP_Rs[a])+hd$MRP_Rs[a]*0.5)
  }
  hd$SellingDate[a] = ymd(hd$SellingDate[a]) - years(2)
}

write.csv(df, "D:\\Semester 5\\8. FOUNDATION OF DATA ANALYTICS\\Data_cleaned.csv")
write.csv(brandname_df, "D:\\Semester 5\\8. FOUNDATION OF DATA ANALYTICS\\Brandwise.csv")
write.csv(date_count_df, "D:\\Semester 5\\8. FOUNDATION OF DATA ANALYTICS\\SellingDateCount.csv")
write.csv(hd, "D:\\Semester 5\\8. FOUNDATION OF DATA ANALYTICS\\Histdata.csv")

hd1 = hd2 = hd
hd1$SellingDate = as.Date(hd1$SellingDate)-(365)
hd2$SellingDate = as.Date(hd2$SellingDate)-(365*2)
hd1$SellPrice_Rs = hd1$MRP_Rs - (hd1$MRP_Rs*0.25)
hd2$SellPrice_Rs = hd2$MRP_Rs + (hd2$MRP_Rs*0.25)

hd[1,]
hd1[1,]
hd2[1,]

quantity <- c(7:9)
hd$Quantity = sample(quantity, length(hd$MRP_Rs), replace=TRUE)
hd1$Quantity = sample(quantity, length(hd$MRP_Rs), replace=TRUE)
hd2$Quantity = sample(quantity, length(hd$MRP_Rs), replace=TRUE)

write.csv(hd, "D:\\Semester 5\\8. FOUNDATION OF DATA ANALYTICS\\Histdata1.csv")
write.csv(hd1, "D:\\Semester 5\\8. FOUNDATION OF DATA ANALYTICS\\Histdata2.csv")
write.csv(hd2, "D:\\Semester 5\\8. FOUNDATION OF DATA ANALYTICS\\Histdata3.csv")
