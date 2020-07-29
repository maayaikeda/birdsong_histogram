# import readxl to read excel sheets
library(readxl)
library(ggplot2)
# import data from different birds
birdsong_1 <- read_excel("~/Documents/Home 2020/birdsong_1.xlsx")
birdsong_2 <- read_excel("~/Documents/Home 2020/birdsong_2.xls")
# check to see if they look ok
head(birdsong_1)
head(birdsong_1)
# combine the data
data <- rbind(birdsong_1, birdsong_2)
# check data
head(data)
# clean up data; get rid of durations that are too short to be songs 
new_data<- data[400 < data$motif_duration,]

birdsong2_filtered<- birdsong_2[400 < birdsong_2$motif_duration,]
p <- ggplot(birdsong2_filtered, aes(motif_duration,fill=context2, color=birdID)) + geom_histogram(binwidth = 2, , alpha = 0.5, position = "identity") + ggtitle("Motif duration: context")+ xlab("Motif duration") +labs(fill = "Sounds in Recording")
p <- p + scale_fill_brewer(palette="Set2")
# get rid of grid
p <- p + theme_classic()
# label y axis
p <- p + ylab("Count")
p


# Songs while young are begging can be considered "undirected song" 
# so relabel them as male singing alone
new_data[new_data == "alone"]<-"Male singing alone"
new_data[new_data == "interact"]<-"Mate responding to song"
new_data[new_data == "beg"]<-"Male singing alone"
# plot data and see how it looks
p <- ggplot(new_data, aes(motif_duration,fill=context2, color=birdID)) + geom_histogram(binwidth = 2, , alpha = 0.5, position = "identity") + ggtitle("Motif duration: context")+ xlab("Motif duration") +labs(fill = "Sounds in Recording")
p <- p + scale_fill_brewer(palette="Set2")
# get rid of grid
p <- p + theme_classic()
# label y axis
p <- p + ylab("Count")
p

# It seems like songs recorded while females are calling are shorter in length thansongs while males are singing alone
# so combine the birds and normalize it to their undirected (alone) mean. 
# (substract all points from undirected mean)

# Count number of rows 
rows = 1:length(new_data$motif_duration)
# Create empty column
new_data[,"diff_frm_mean"] <- NA
# Make sure it looks ok
head(new_data)

# Need this library to summarize data. Load it.
library(Rmisc)
# Summary table. Call it df1.
df1 <- summarySE(new_data, measurevar = "motif_duration", groupvars=c("birdID", "context2"))
# Take a look at df1
df1

# Extract means for undirected (alone) songs
G544undir_mean <- df1[1, 4]
O375undir_mean <- df1[3, 4]


# Add motif_duration-mean values in the new column. 
# If the bird is G544, substract G544 undirected mean, otherwise, substract O375's undirected mean.
for (r in rows){
  if (new_data$birdID[r] == "G544"){
    new_data$diff_frm_mean[r] <- new_data$motif_duration[r]-G544undir_mean
    } else { 
      new_data$diff_frm_mean[r] <- new_data$motif_duration[r]-O375undir_mean
  }
}

# Check if it looks ok
head(new_data$diff_frm_mean)
head(new_data$motif_duration)

anov <- aov(new_data$diff_frm_mean ~ new_data$contexts)
summary(anov)
TukeyHSD(anov)

# Plot the data
library(ggplot2)
p <- ggplot(new_data, aes(diff_frm_mean,color=context2)) + geom_histogram(fill = "white",position = "dodge") + ggtitle("Motif length")
p <- p + labs(color = "Context")
p <- p + theme_classic()
p

# Another way to summarize mean.
library(plyr)
mu2 <- ddply(new_data, "context2", summarise, grp.mean=mean(diff_frm_mean))
head(mu2)

# Plot density
p <- ggplot(new_data, aes(x=diff_frm_mean, color=context2, fill=context2))
p <- p + geom_histogram(aes(y=..density..), position="identity", alpha=0.5) 
p <- p + geom_density(alpha=0.5)
p <- p + geom_vline(data=mu2, aes(xintercept=grp.mean, color=context2),linetype="dashed")
p <- p + scale_color_manual(values=c("pink3", "darkseagreen3"),name="Experimental\nCondition",
                            breaks=c("Male singing alone", "Mate responding to song"),
                            labels=c("Suspected Undirected", "Suspected Directed"))
p <- p + scale_fill_manual(values=c("pink3", "darkseagreen3"),name="Experimental\nCondition",
                           breaks=c("Male singing alone", "Mate responding to song"),
                           labels=c("Suspected Undirected", "Suspected Directed"))
p <- p + labs(title="Motif Length", y = "Density", x="Motif length difference from directed mean")
p <- p + theme_classic()
p
#--
# Run statistics
S_Undirected <- subset(new_data == "Male singing alone")
S_Directed <-subset(new_data == "Mate responding to song")

t.test(S_Undirected,S_Directed)

# Plot to see how it looks for each bird.

# It is known from previous studies that directed songs are shorter than undirected songs. 
# For O375, "Male singing alone" is definately "undirected" song 
# and "Mate responding to song" is actually is a female call contaminated recording.
# So relabel so that that context description is more accurate.

new_data[,"contexts"] <- NA
for (r in rows){
  if (new_data$birdID[r] == "G544" && new_data$context2[r] == "Male singing alone"){
    new_data$contexts[r] <- "Undirected G544"
  } else if (new_data$birdID[r] == "G544" && new_data$context2[r] == "Mate responding to song"){
    new_data$contexts[r] <- "Female calls in recording G544"
  } else if (new_data$birdID[r] == "O375" && new_data$context2[r] == "Mate responding to song"){
    new_data$contexts[r] <- "Female calls in recording O375"
  } else { 
    new_data$contexts[r] <- "Male singing alone O375"
  }
}


# Get a stats summary for density plot.
df2 <- summarySE(new_data, measurevar = "diff_frm_mean", groupvars=c("birdID", "contexts"))
df2

# Density plot for each bird. Pink = G544, Green = O375; lighter color = undirected
# lighdarker color = directed
p <- ggplot(new_data, aes(x=diff_frm_mean, color=contexts, fill=contexts))
p <- p + geom_histogram(aes(y=..density..), position="identity", alpha=0.5) 
p <- p + geom_density(alpha=0.5)
p <- p + geom_vline(data=df2, aes(xintercept=diff_frm_mean, color=contexts),linetype="dashed")
p <- p + scale_color_manual(values = c("pink3", "pink1", "darkseagreen3", "darkseagreen1"), name = "Context/Bird",
                            breaks = c("Female calls in recording G544", "Undirected G544", "Female calls in recording O375", "Male singing alone O375"),
                            labels = c("Female calls in recording G544", "Undirected G544", "Female calls in recording O375", "Male singing alone O375"))
p <- p + scale_fill_manual(values = c("pink3", "pink1", "darkseagreen3", "darkseagreen1"), name = "Context/Bird",
                            breaks = c("Female calls in recording G544", "Undirected G544", "Female calls in recording O375", "Male singing alone O375"),
                            labels = c("Female calls in recording G544", "Undirected G544","Female calls in recording O375", "Male singing alone O375"))
p <- p + labs(title="Undirected vs Directed in birds G544 and O375", y = "Density", x="Motif length difference from directed mean")
p <- p + annotate("segment", x = 0, y = 0, xend = 0, yend =0.065 )
p <- p + annotate("text", x = 5,y = 0.07, label = "Undirected mean" )
p <- p + theme_classic()
p


# Conclusion, for both birds, "directed" songs or when the song files are contaminated with female
# calls, the songs are faster.
# Maybe looking at female call contamination in files is a way to find "directed" songs.



# number of variables
length(birdsong2_filtered)
bs2.pca <- prcomp(birdsong2_filtered[,2:12])

bs2.pca
library(ggfortify)
pca.plot <- autoplot(bs2.pca, data = birdsong2_filtered, colour = 'context2')
pca.plot


n_rows <- nrow(new_data)
nrow(new_data)
ncol(new_data)
new_data[c(4,6,7), 1:4]
set.seed(1234)
ind <- sample(1:2, size=n_rows, replace=TRUE, prob =c(0.67, 0.33))
new_data$diff_frm_mean[ind==2]
head(new_data)
data.training <- new_data[ind==1, c(2:16,27)]
data.test <- new_data[ind==2, c(2:16,27)]

data.trainlabels <-new_data$contexts[ind==1]
head(data.trainlabels)

data.testlabels <- new_data$contexts[ind==2]
head(data.testlabels)

# Build the model
library(class)
data_pred <- knn(train = data.training, test = data.test, cl = data.trainlabels,k=3)
head(data_pred)

data.Testlabels <- data.frame(data.testlabels)
head(data.Testlabels)
data_Pred <- data.frame(data_pred)
head(data_Pred)

merge <- cbind(data_Pred, data.Testlabels)
names(merge) <- c("predicted","true")
head(merge)

# suing package "caret"


library(caret)
library(e1071)
birdsong2 <- subset(new_data, birdID == "O375")
index <- createDataPartition(birdsong2$contexts, p=0.75, list=FALSE)
head(index)
# the data needs to be in a data frame or it wont work
birdsong2 <- as.data.frame(birdsong2)
str(birdsong2)
# convert contexts column to factor
birdsong2$contexts <- as.factor(birdsong2$contexts)
str(birdsong2)
data.training <- birdsong2[index,c(2:16,27,28)]
data.test <- birdsong2[-index,c(2:16,27,28)]

data.test <- as.data.frame(data.test )
head(birdsong2_filtered)
ncol(birdsong2_filtered)
head(data.training)
ncol(data.training)
nrow(data.training)

# Train model

model_knn <- train(data.training[,1:16], data.training$contexts, method = 'knn' )

predictions <- predict(object = model_knn, data.test[,1:16])
table(predictions)


resultstest <- data.test[,17]
confusionMatrix(predictions, resultstest)

model_knn <- train(data.training[,1:16], data.training$contexts, method = 'knn',preProcess=c("center", "scale") )
predictions <- predict(object = model_knn, data.test[,1:16], type="raw")


confusionMatrix(predictions,resultstest)


importance <- varImp(model_knn, scale=FALSE)
print(importance)

# bird 1
library(caret)
library(e1071)
birdsong1 <- subset(new_data, birdID == "G544")
index <- createDataPartition(birdsong1$contexts, p=0.75, list=FALSE)

# the data needs to be in a data frame or it wont work
birdsong1 <- as.data.frame(birdsong1)

# convert contexts column to factor
birdsong1$contexts <- as.factor(birdsong1$contexts)

data.training <- birdsong1[index,c(2:16,27,28)]
data.test <- birdsong1[-index,c(2:16,27,28)]

data.test <- as.data.frame(data.test )


# Train model

model_knn <- train(data.training[,1:16], data.training$contexts, method = 'knn' )

predictions <- predict(object = model_knn, data.test[,1:16])



resultstest <- data.test[,17]
confusionMatrix(predictions, resultstest)

model_knn <- train(data.training[,1:16], data.training$contexts, method = 'knn',preProcess=c("center", "scale") )
predictions <- predict(object = model_knn, data.test[,1:16], type="raw")


confusionMatrix(predictions,resultstest)


importance <- varImp(model_knn, scale=FALSE)
print(importance)




# plot scatter

p <- ggplot(birdsong1, aes(x = motif_duration, y = mean_entropy, color=contexts))
p <- p + geom_point()
p <- p +  theme_classic()
p








