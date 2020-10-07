#Load Required Libraries
library(readr)
library(dplyr)
library(reshape2)
library(ggplot2)
library(zoo)
library(tseries)
library(corrplot)

#Load/Clean Data
df <- read_csv("vgsales-12-4-2019.csv") #load data
my_cols <- c("Name","Genre", "ESRB_Rating","Platform","Publisher","Critic_Score","User_Score","Global_Sales","Year")
df<- df[,my_cols] # keep only some columns
df$ESRB_Rating <- factor(df$ESRB_Rating) #make column a factor
df$Genre<- factor(df$Genre) #make column a factor


#Sales by Genre
df1 <-df[complete.cases(df$Year), ] #remove rows missing Year
df1 <-df[complete.cases(df$Global_Sales), ] # remove rows missing Global_Sales
df1<- subset(df1,df1$Year<2020) #remove unreleased games
df1$year_binned <- cut(df1$Year, breaks = c(1960,1970,1980,1990,2000,2010,2019), labels = c("1970s","1970s","1980s","1990s","2000s","2010s"))#add new column year_binned
df1 <- group_by(df1,year_binned)    #group data            
num_of_observations <- count(df1) # count number of observations
df1 <- group_by(df1,year_binned,Genre)#group data
summ <- summarize(df1, num = n(), global_sales_millions = sum(Global_Sales))
totsales_genre_year <- dcast(summ, year_binned ~ Genre, value.var = "global_sales_millions") # create pivot table showing sales of each genre throughout time

#Sales by ESRB Rating
df2 <-df1[complete.cases(df1$ESRB_Rating), ] # remove rows missing ESRB Rating
number_of_genres <- nlevels(df2$Genre)
df2 <- group_by(df2,year_binned,ESRB_Rating)
summ2 <- summarize(df2,global_sales_millions = sum(Global_Sales))
totsales_rating_year <- dcast(summ2, year_binned ~ ESRB_Rating, value.var = "global_sales_millions") #create pivot table showing sales in each rating throughout time
df2 <- group_by(df2,ESRB_Rating) #group data
rating_num_sold_totsales <- summarize(df2,num_of_games = n(), global_sales_millions = sum(Global_Sales)) #show number of  games and total sales of each rating

#Bar chart for number of games sold and global sales of differently rated games
mat2 <- as.matrix(rating_num_sold_totsales)
barplot(mat2, ylim = c(0,1000000), main = "Number of Games Sold of Different Rated Games", ylab= "Dollars", xlab="Differently Rated Games")


#Graph Sales vs Critic Rating
df3 <-df[complete.cases(df$Global_Sales), ] # remove rows missing Global Sales
df3 <-df[complete.cases(df$Critic_Score), ] #remove rows missing critic score
q1<- qplot(Critic_Score,Global_Sales, data = df3,geom = "point", shape = ESRB_Rating, color = ESRB_Rating) # graph Sales vs critic score
q1 <- q1 + ggtitle("Sales vs Critic Score") + theme(plot.title = element_text(size = 14, hjust = 0.50))  #add title

#Graph Sales vs User Rating
df4<- df[complete.cases(df$Global_Sales), ]#remove rows missing Global Sales
df4 <-df[complete.cases(df$User_Score), ]#remove rows missing user score

q2<- qplot(User_Score,Global_Sales, data = df4, geom = "point", shape = ESRB_Rating, color = ESRB_Rating) # graph Sales vs user score
q2 <- q2 + ggtitle("Sales vs User Score")

#Find Correlation between rating and sales
df6 <- df[, c("Global_Sales","Critic_Score", "User_Score")]
df6 <- df6[complete.cases(df6), ]
correlations <- cor(df6)
corrplot(correlations, method = "circle", type = "upper")
top10 <- head(df1[order(df1$Global_Sales,decreasing = TRUE),],n=10) #find top 10 games in terms of sales



#create sales and rating tables
mean_sales_m1 <- filter(df2, ESRB_Rating == "M") #keep only games rated m
mean_sales_m <-round(mean(mean_sales_m1$Global_Sales),2)*1000000 #find average sales of games rated M
mean_sales_E1 <- filter(df2, ESRB_Rating == "E") # keep only games rated E
mean_sales_E <-format(round(mean(mean_sales_E1$Global_Sales),2)*1000000,scientific = F) # find average sales of games rated E
mean_sales_T1 <- filter(df2, ESRB_Rating == "T") # keep only games rated T
mean_sales_T <-round(mean(mean_sales_T1$Global_Sales),2)*1000000 #find average sales of games rated T
m2 <- filter(mean_sales_m1, mean_sales_m1$Critic_Score>0) #keep games rated M and with a critic score
m3 <- filter(mean_sales_m1, mean_sales_m1$User_Score>0)#keep games rated M and with a user score
M_Critic_Score <- mean(m2$Critic_Score) #find mean critic score of M rated games
M_User_Score <- mean(m3$User_Score) #find mean user score of M rated games
e1 <- filter(mean_sales_E1,mean_sales_E1$Critic_Score>0)#keep games rated E and with a critic score
e2<- filter(mean_sales_E1,mean_sales_E1$User_Score>0)#keep games rated E and with a user score
E_Critic_Score <- mean(e1$Critic_Score)#find mean critic score of E rated games
E_User_Score <- mean(e2$User_Score)#find mean user score of E rated games
t1<- filter(mean_sales_T1, mean_sales_T1$Critic_Score>0)#keep games rated T and with a critic score
t2<- filter(mean_sales_T1, mean_sales_T1$User_Score>0)#keep games rated T and with a user score
T_Critic_Score <- mean(t1$Critic_Score)#find mean critic score of T rated games
T_User_Score <- mean(t2$User_Score)#find mean user score of T rated games
avg_scores <- data.frame(M_Critic_Score,M_User_Score,E_Critic_Score,E_User_Score, T_Critic_Score, T_User_Score) # put average scores into a table
avg_sales <- data.frame(mean_sales_E,mean_sales_T, mean_sales_m) # put average sales into a table

#Bar chart of average sales by different ratings
mat1 <- as.matrix(avg_sales)
barplot(mat1, ylim = c(0,1000000), main = "Avg Sales of Different Rated Games", ylab= "Dollars", xlab="Differently Rated Games")


#Sales by Console
df5<- group_by(df1,Platform)
df5 <- filter(df5, year_binned == "2010s")
sales_by_console <- summarize(df5, global_sales_millions = sum(Global_Sales))# display top selling consoles of 2010s

#convert totsales_genre_year to percentages
pivot5 <- totsales_genre_year #create new table
pivot5[is.na(pivot5)]<- 0 # replace NA with 0
pivot5$year_binned <- NULL #remove years column
mat<-as.matrix(pivot5) # convert to matrix
matnorm<-round(mat/rowSums(mat),3) # divide each entry by sum of its row and round to 3 decimal places
datnorm<-as.data.frame(matnorm) # change back to dataframe 
datnorm<- mutate(datnorm, Year = levels(df1$year_binned)) # add years to datnorm
#Clean/add year back to small_percentage
my_cols1 <- c("Action", "Fighting", "Racing", "Shooter","Sports") # keep certain columns
small_percentage <- datnorm[,my_cols1] # create smaller percentage table with poular genres
small_percentage <- mutate(small_percentage, Year = levels(df1$year_binned))#add years back 
small_percentage <- small_percentage[,c(6,1,2,3,4,5)] #change order of rows to put years first
q1

#Time series graph of total sales genre to percentages
ts3<- ts(small_percentage[2], frequency = .1, start = 1970)
ts4<- ts(small_percentage[3], frequency = .1, start = 1970)
ts5 <-ts(small_percentage[4], frequency = .1, start = 1970)
ts6 <-ts(small_percentage[5], frequency = .1, start = 1970)
ts7 <-ts(small_percentage[6], frequency = .1, start = 1970)
color1 = c("green", "blue", "orange", "red", "gold")
tsplot1 <- ts.plot(ts3,ts4,ts5,ts6,ts7, col = color1,xlab= "Year", ylab = "Sales")
legend("topright", legend=c(colnames(small_percentage[2]), colnames(small_percentage[3]),colnames(small_percentage[4]),colnames(small_percentage[5]),colnames(small_percentage[6])),
       col=c("green", "blue", "orange", "red", "gold"), lty=1,lwd=1.5,cex = .70,horiz=TRUE) 


#Create Function to Plot Two Genres at a time
plot_var2 <- function(x,y){
  if(missing(y)){
    ts1<- ts(datnorm[x], frequency = .1, start = 1970)
    color = c("purple")
    tsplot<- ts.plot(ts1,col = color, xlab = "Year", ylab = "Percentage of Decade Sales")
    legend("topright", legend=colnames(datnorm[x]), col = color, lty=1,lwd=1.5, cex=.70)
    title(main = paste("Percentage of Sales For ",x, sep = ""))
  }else{
    ts1<- ts(datnorm[x], frequency = .1, start = 1970)
    ts2<- ts(datnorm[y], frequency = .1, start = 1970)
    color = c("green", "blue")
    tsplot<- ts.plot(ts1, ts2, col= color, xlab = "Year", ylab = "Percentage of Decade Sales")
    legend("topright", legend=c(colnames(datnorm[x]), colnames(datnorm[y])),
           col=c('green','blue'), lty=1,lwd=1.5,cex = .70, horiz = TRUE) 
    title(main = paste("Percentage of Sales For ",x, " vs ",y, sep = ""))
  }
}
plot_var2("Action")
q1
q2
