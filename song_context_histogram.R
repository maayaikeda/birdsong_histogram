library(readxl)
G544_mMAN <- read_excel("G544_mMAN.xlsx")
data <- G544_mMAN
head(data)
new_data<- data[400 < data$motif_duration,]
library(ggplot2)
p <- ggplot(new_data, aes(motif_duration,color=comments)) + geom_histogram(fill = "white",position = "dodge") + ggtitle("motif length")
p <- p + scale_color_manual(values=c("pink1","pink3","green1", "green3" ))
#p <- p +theme(legend.position = "none")
p <- p + theme_classic()
p
library(plyr)
mu <- ddply(new_data, "comments", summarise, grp.mean=mean(motif_duration))
head(mu)
ggplot(new_data, aes(x=motif_duration, color=comments, fill=comments)) +
  geom_histogram(aes(y=..density..), position="identity", alpha=0.5)+
  geom_density(alpha=0.6)+
  geom_vline(data=mu, aes(xintercept=grp.mean, color=comments),
             linetype="dashed")+
  scale_color_manual(values=c("pink1","pink3","darkseagreen1", "darkseagreen3"),name="Experimental\nCondition",
                     breaks=c("post", "postfemale", "pre","prefemale"),
                     labels=c("Post surgery undirected", "Post surgery directed", "Pre surgery undirected", "Pre surgery directed"))+
  scale_fill_manual(values=c("pink1","pink3","darkseagreen1", "darkseagreen3"),name="Experimental\nCondition",
                    breaks=c("post", "postfemale", "pre","prefemale"),
                    labels=c("Post surgery undirected", "Post surgery directed", "Pre surgery undirected", "Pre surgery directed"))+
  labs(title="Motif Length", y = "Density")+
  theme_classic()
