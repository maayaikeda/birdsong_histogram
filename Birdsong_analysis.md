Birdsong analysis
================

This is an analysis of songs recorded from two zebra finches. Zebra
finche males sing to their mates during courtship. Previous studies have
found that males sing shorter (faster) songs to their mates (called
Directed song) than they are practicing alone (Undirected songs).

Here, I have audio files from males recorded while they were with their
mates. Noramally, we would know whether the songs are directed or
undirected from their behavior such as the direction they are facing and
whether or not they are dancing to their mate. However, there are no
videos recorded for this data.

### Question and hypothesis

Then, how do I figure out which ones are “directed” and which ones are
“undirected” songs?

I have noticed that some songs are sang while the females are making
calls. There are some indications from previous studies that females
like to respond to directed songs with calls.

**I hypothesize that the majority of songs recordings that are
contaminated with female calls are directed songs and the others are
undirected. **

If this is true, I expect songs with female calls to be shorter/faster
than songs with no calls.

## Preparing Data

Load and prepare data.

``` r
library(readxl)

birdsong_1 <- read_excel("~/Documents/Home 2020/birdsong_1.xlsx")
birdsong_2 <- read_excel("~/Documents/Home 2020/birdsong_2.xls")

# Combine data
data <- rbind(birdsong_1, birdsong_2)

# clean up data; get rid of durations that are too short to be songs 
new_data<- data[400 < data$motif_duration,]
birdsong2_filtered<- birdsong_2[400 < birdsong_2$motif_duration,]
```

## Preparing data/preliminary plot for Bird \#2

For bird2 (O375), songs were recorded when he was with his mate and
offspring, so some song audio files contain his mate’s calls and his
offsprings begging. I expect offspring begging to not make a difference.

``` r
library(ggplot2)
p <- ggplot(birdsong2_filtered, aes(motif_duration,fill=context2)) + geom_histogram(binwidth = 2, , alpha = 0.5, position = "identity") + ggtitle("Motif duration: context")+ xlab("Motif duration") +labs(fill = "Sounds in Recording")
p <- p + scale_fill_brewer(palette="Set2")
# get rid of grid
p <- p + theme_classic()
# label y axis
p <- p + ylab("Count")
p
```

![](Birdsong_analysis_files/figure-gfm/plot-1.png)<!-- -->

As expected, songs sang during begging calls do not seem to be shorter
in duration than males singing to themselves, so I decided to categorize
it as as male singing alone.

``` r
new_data[new_data == "alone"]<-"Male singing alone"
new_data[new_data == "interact"]<-"Mate responding to song"
new_data[new_data == "beg"]<-"Male singing alone"
```

For bird1 (G544), songs were recorded while he was completely alone and
when he was with a female. The songs while he was separated from his
mate can be safely assumed to be “directed”.

Plotting data to see how it
looks.

## Preliminary plot 2 to see how the overall distributions are for both birds

``` r
# plot data and see how it looks
p <- ggplot(new_data, aes(motif_duration,fill=context2, color=birdID)) + geom_histogram(binwidth = 2, , alpha = 0.5, position = "identity") + ggtitle("Motif duration: context")+ xlab("Motif duration") +labs(fill = "Sounds in Recording")
p <- p + scale_fill_brewer(palette="Set2")
# get rid of grid
p <- p + theme_classic()
# label y axis
p <- p + ylab("Count")
p
```

![](Birdsong_analysis_files/figure-gfm/testplot-1.png)<!-- -->

It does look like the songs sung when females are calling are shorter in
duration than the songs male are singing to themselves.

Since these two birds have different song duration, I am normalizing
them to their “undirected” song or solo song recordings.

## Summarizing data

Looking at data summary and mean durations to normalize data.

``` r
# Need this library to summarize data. Load it.
library(Rmisc)
```

    ## Loading required package: lattice

    ## Loading required package: plyr

``` r
# Summary table. Call it df1.
df1 <- summarySE(new_data, measurevar = "motif_duration", groupvars=c("birdID", "context2"))
df1
```

    ##   birdID                context2   N motif_duration        sd        se
    ## 1   G544      Male singing alone  72       616.4928  7.723779 0.9102561
    ## 2   G544 Mate responding to song  49       601.4827 10.983288 1.5690412
    ## 3   O375      Male singing alone 938       686.0850 11.265243 0.3678232
    ## 4   O375 Mate responding to song 161       676.6679 11.415768 0.8996885
    ##          ci
    ## 1 1.8149992
    ## 2 3.1547687
    ## 3 0.7218527
    ## 4 1.7767963

## Normalizing data to the mean

Taking the difference from the mean.

``` r
# Count number of rows 
rows = 1:length(new_data$motif_duration)
# Create empty column
new_data[,"diff_frm_mean"] <- NA

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
```

## Plot normalized grouped data

``` r
library(ggplot2)
p <- ggplot(new_data, aes(diff_frm_mean,color=context2)) + geom_histogram(fill = "white",position = "dodge") + ggtitle("Motif length")
p <- p + labs(color = "Context") + xlab("Motif duration difference from mean")
p <- p + theme_classic()
p
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](Birdsong_analysis_files/figure-gfm/plot2-1.png)<!-- -->

## Plot density plot for grouped data

Density plot.

``` r
# Another way to summarize mean.
library(plyr)
mu2 <- ddply(new_data, "context2", summarise, grp.mean=mean(diff_frm_mean))
head(mu2)
```

    ##                  context2      grp.mean
    ## 1      Male singing alone -1.733607e-14
    ## 2 Mate responding to song -1.072208e+01

``` r
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
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](Birdsong_analysis_files/figure-gfm/plot3-1.png)<!-- -->

Overall, for grouped data, the “undirected” songs (songs while male
singing alone)

## Run t-test

``` r
S_Undirected <- subset(new_data == "Male singing alone")
S_Directed <-subset(new_data == "Mate responding to song")

t.test(S_Undirected,S_Directed)
```

    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  S_Undirected and S_Directed
    ## t = 27.421, df = 43996, p-value < 2.2e-16
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  0.02899953 0.03346429
    ## sample estimates:
    ##   mean of x   mean of y 
    ## 0.037630641 0.006398732

## Plot by bird

Plot by bird to see if this is the the case for ungrouped data. But
first, rename the data so that it’s clearer.

``` r
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
```

    ##   birdID                       contexts   N diff_frm_mean        sd        se
    ## 1   G544 Female calls in recording G544  49 -1.501011e+01 10.983288 1.5690412
    ## 2   G544                Undirected G544  72 -4.263256e-14  7.723779 0.9102561
    ## 3   O375 Female calls in recording O375 161 -9.417028e+00 11.415768 0.8996885
    ## 4   O375        Male singing alone O375 938 -1.538796e-14 11.265243 0.3678232
    ##          ci
    ## 1 3.1547687
    ## 2 1.8149992
    ## 3 1.7767963
    ## 4 0.7218527

``` r
# Density plot for each bird. Pink = G544, Green = O375; lighter color = undirected
# lighdarker color = directed
p <- ggplot(new_data, aes(x=diff_frm_mean, color=contexts, fill=contexts))
p <- p + geom_histogram(aes(y=..density..), position="identity", alpha=0.5) 
p <- p + geom_density(alpha=0.5)
p <- p + geom_vline(data=df2, aes(xintercept=diff_frm_mean, color=contexts),linetype="dashed")
p <- p + scale_color_manual(values = c("pink3", "pink1", "darkseagreen3", "darkseagreen1"), name = "Context/Bird",
                            breaks = c("Female calls in recording G544", "Undirected G544", "Female calls in recording O375", "Male singing alone O375"),
                            labels = c("Suspected directed G544", "Undirected G544", "Suspected directed O375", "Suspected undirected O375"))
p <- p + scale_fill_manual(values = c("pink3", "pink1", "darkseagreen3", "darkseagreen1"), name = "Context/Bird",
                            breaks = c("Female calls in recording G544", "Undirected G544", "Female calls in recording O375", "Male singing alone O375"),
                            labels = c("Suspected directed G544", "Undirected G544", "Suspected directed O375", "Suspected undirected O375"))
p <- p + labs(title="Undirected vs Directed in birds G544 and O375", y = "Density", x="Motif length difference from directed mean")
p <- p + annotate("segment", x = 0, y = 0, xend = 0, yend =0.065 )
p <- p + annotate("text", x = 5,y = 0.07, label = "Undirected mean" )
p <- p + theme_classic()
p
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](Birdsong_analysis_files/figure-gfm/plotbybird-1.png)<!-- --> \#\#
Run stats by bird

``` r
# Run anova
anov <- aov(new_data$diff_frm_mean ~ new_data$contexts)
# output summary
summary(anov)
```

    ##                     Df Sum Sq Mean Sq F value Pr(>F)    
    ## new_data$contexts    3  21162    7054   57.27 <2e-16 ***
    ## Residuals         1216 149788     123                   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# Run tukey test for pairwise comparison
TukeyHSD(anov)
```

    ##   Tukey multiple comparisons of means
    ##     95% family-wise confidence level
    ## 
    ## Fit: aov(formula = new_data$diff_frm_mean ~ new_data$contexts)
    ## 
    ## $`new_data$contexts`
    ##                                                                       diff
    ## Female calls in recording O375-Female calls in recording G544 5.593083e+00
    ## Male singing alone O375-Female calls in recording G544        1.501011e+01
    ## Undirected G544-Female calls in recording G544                1.501011e+01
    ## Male singing alone O375-Female calls in recording O375        9.417028e+00
    ## Undirected G544-Female calls in recording O375                9.417028e+00
    ## Undirected G544-Male singing alone O375                       4.726990e-13
    ##                                                                      lwr
    ## Female calls in recording O375-Female calls in recording G544  0.9346463
    ## Male singing alone O375-Female calls in recording G544        10.8260244
    ## Undirected G544-Female calls in recording G544                 9.7223728
    ## Male singing alone O375-Female calls in recording O375         6.9813153
    ## Undirected G544-Female calls in recording O375                 5.3690297
    ## Undirected G544-Male singing alone O375                       -3.4916809
    ##                                                                     upr
    ## Female calls in recording O375-Female calls in recording G544 10.251520
    ## Male singing alone O375-Female calls in recording G544        19.194197
    ## Undirected G544-Female calls in recording G544                20.297849
    ## Male singing alone O375-Female calls in recording O375        11.852740
    ## Undirected G544-Female calls in recording O375                13.465026
    ## Undirected G544-Male singing alone O375                        3.491681
    ##                                                                   p adj
    ## Female calls in recording O375-Female calls in recording G544 0.0110487
    ## Male singing alone O375-Female calls in recording G544        0.0000000
    ## Undirected G544-Female calls in recording G544                0.0000000
    ## Male singing alone O375-Female calls in recording O375        0.0000000
    ## Undirected G544-Female calls in recording O375                0.0000000
    ## Undirected G544-Male singing alone O375                       1.0000000

As expected, songs with and without female calls in the background are
significantly different in duration.

## Conclusion

1.  For bird \#1 (G544), the songs that he sang while his mate was
    calling were overall shorter in duration than his undirected song.
2.  For bird \#2 (O375), while we don’t have recordings from him while
    he was alone, his song recording whithout the contamination of
    female calls were longer in duration than the songs sang while his
    mate was calling.

This suggest that we presence of female call contamination could be a
way to sort undirected vs. directed songs when visual analysis of
behavior cannot be done.

# Using Machine learning algorithm

Using Machine learning algorithm to 1. see how different are the songs
during different contexts for each bird. 2. find out what other
variables are different between contexts.

### Set up data

``` r
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
```

### Train and run KNN model.

``` r
# Train model

model_knn <- train(data.training[,1:16], data.training$contexts, method = 'knn',preProcess=c("center", "scale") )
predictions <- predict(object = model_knn, data.test[,1:16], type="raw")

resultstest <- data.test[,17]
confusionMatrix(predictions,resultstest)
```

    ## Confusion Matrix and Statistics
    ## 
    ##                                 Reference
    ## Prediction                       Female calls in recording G544 Undirected G544
    ##   Female calls in recording G544                              7               0
    ##   Undirected G544                                             5              18
    ##                                                         
    ##                Accuracy : 0.8333                        
    ##                  95% CI : (0.6528, 0.9436)              
    ##     No Information Rate : 0.6                           
    ##     P-Value [Acc > NIR] : 0.005659                      
    ##                                                         
    ##                   Kappa : 0.6269                        
    ##                                                         
    ##  Mcnemar's Test P-Value : 0.073638                      
    ##                                                         
    ##             Sensitivity : 0.5833                        
    ##             Specificity : 1.0000                        
    ##          Pos Pred Value : 1.0000                        
    ##          Neg Pred Value : 0.7826                        
    ##              Prevalence : 0.4000                        
    ##          Detection Rate : 0.2333                        
    ##    Detection Prevalence : 0.2333                        
    ##       Balanced Accuracy : 0.7917                        
    ##                                                         
    ##        'Positive' Class : Female calls in recording G544
    ## 

### Variable importance

Looking at which variables contributed the most. We expect motif
duration to be ranked high.

``` r
importance <- varImp(model_knn, scale=FALSE)
print(importance)
```

    ## ROC curve variable importance
    ## 
    ##                    Importance
    ## start                  0.9369
    ## diff_frm_mean          0.8834
    ## motif_duration         0.8834
    ## mean_entropy           0.6847
    ## mean_pitch             0.6094
    ## mean_FM                0.5856
    ## var_FM                 0.5581
    ## var_entropy            0.5480
    ## mean_amplitude         0.5408
    ## var_mean_freq          0.5165
    ## mean_freq              0.5153
    ## var_pitch_goodness     0.5130
    ## mean_AM2               0.5110
    ## mean_pitchgoodness     0.5105
    ## var_pitch              0.5103
    ## var_AM                 0.5060

As expected, motif duration is the most important variable other than
start time. Mean entropy is also expected to be different between the
two groups because isolated recordings are clean while recordings while
females are calling are messy (less clean) by definition.

### Plot data

Plot data to see how well the top two variables separate out the two
contexts.

``` r
# plot scatter

p <- ggplot(birdsong1, aes(x = motif_duration, y = mean_entropy, color=contexts))
p <- p + geom_point()
p <- p +  theme_classic()
p
```

![](Birdsong_analysis_files/figure-gfm/scatterplot-1.png)<!-- -->

### Repeat for Bird \#2

``` r
birdsong2 <- subset(new_data, birdID == "O375")
index <- createDataPartition(birdsong2$contexts, p=0.75, list=FALSE)

# the data needs to be in a data frame or it wont work
birdsong2 <- as.data.frame(birdsong2)

# convert contexts column to factor
birdsong2$contexts <- as.factor(birdsong2$contexts)

data.training <- birdsong2[index,c(2:16,27,28)]
data.test <- birdsong2[-index,c(2:16,27,28)]

data.test <- as.data.frame(data.test )
```

Using KNN model.

``` r
# Train model

model_knn <- train(data.training[,1:16], data.training$contexts, method = 'knn',preProcess=c("center", "scale") )
predictions <- predict(object = model_knn, data.test[,1:16], type="raw")

resultstest <- data.test[,17]
confusionMatrix(predictions,resultstest)
```

    ## Confusion Matrix and Statistics
    ## 
    ##                                 Reference
    ## Prediction                       Female calls in recording O375
    ##   Female calls in recording O375                             11
    ##   Male singing alone O375                                    29
    ##                                 Reference
    ## Prediction                       Male singing alone O375
    ##   Female calls in recording O375                       2
    ##   Male singing alone O375                            232
    ##                                                         
    ##                Accuracy : 0.8869                        
    ##                  95% CI : (0.8433, 0.9218)              
    ##     No Information Rate : 0.854                         
    ##     P-Value [Acc > NIR] : 0.06941                       
    ##                                                         
    ##                   Kappa : 0.37                          
    ##                                                         
    ##  Mcnemar's Test P-Value : 3.016e-06                     
    ##                                                         
    ##             Sensitivity : 0.27500                       
    ##             Specificity : 0.99145                       
    ##          Pos Pred Value : 0.84615                       
    ##          Neg Pred Value : 0.88889                       
    ##              Prevalence : 0.14599                       
    ##          Detection Rate : 0.04015                       
    ##    Detection Prevalence : 0.04745                       
    ##       Balanced Accuracy : 0.63323                       
    ##                                                         
    ##        'Positive' Class : Female calls in recording O375
    ## 

### Variable importance for bird \#2

``` r
importance <- varImp(model_knn, scale=FALSE)
print(importance)
```

    ## ROC curve variable importance
    ## 
    ##                    Importance
    ## diff_frm_mean          0.7204
    ## motif_duration         0.7204
    ## var_pitch              0.7177
    ## mean_pitch             0.7162
    ## mean_entropy           0.6991
    ## mean_freq              0.6864
    ## mean_FM                0.6563
    ## start                  0.6444
    ## mean_pitchgoodness     0.6380
    ## var_entropy            0.6039
    ## mean_AM2               0.5725
    ## var_FM                 0.5649
    ## var_pitch_goodness     0.5596
    ## var_AM                 0.5391
    ## var_mean_freq          0.5370
    ## mean_amplitude         0.5148

## Plot bird 2

``` r
# plot scatter

p <- ggplot(birdsong2, aes(x = motif_duration, y = mean_entropy, color=contexts, alpha=0.5))
p <- p + geom_point()
p <- p +  theme_classic()
p
```

![](Birdsong_analysis_files/figure-gfm/scatterplotb2-1.png)<!-- -->

## Running stats to see other variables are significantly different between two contexts.

Mean pitch and mean entropy are two variables that ranked high for both
birds. Running statistical tests to see if these mesures are different
in different contexts.

### Mean pitch

``` r
# Run anova
anov <- aov(new_data$mean_pitch ~ new_data$contexts)
# output summary
summary(anov)
```

    ##                     Df    Sum Sq   Mean Sq F value Pr(>F)    
    ## new_data$contexts    3 744561905 248187302    1540 <2e-16 ***
    ## Residuals         1216 195953152    161146                   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# Run tukey test for pairwise comparison
TukeyHSD(anov)
```

    ##   Tukey multiple comparisons of means
    ##     95% family-wise confidence level
    ## 
    ## Fit: aov(formula = new_data$mean_pitch ~ new_data$contexts)
    ## 
    ## $`new_data$contexts`
    ##                                                                       diff
    ## Female calls in recording O375-Female calls in recording G544  2253.851819
    ## Male singing alone O375-Female calls in recording G544         2635.299117
    ## Undirected G544-Female calls in recording G544                    1.934807
    ## Male singing alone O375-Female calls in recording O375          381.447298
    ## Undirected G544-Female calls in recording O375                -2251.917012
    ## Undirected G544-Male singing alone O375                       -2633.364309
    ##                                                                      lwr
    ## Female calls in recording O375-Female calls in recording G544  2085.3603
    ## Male singing alone O375-Female calls in recording G544         2483.9644
    ## Undirected G544-Female calls in recording G544                 -189.3180
    ## Male singing alone O375-Female calls in recording O375          293.3497
    ## Undirected G544-Female calls in recording O375                -2398.3295
    ## Undirected G544-Male singing alone O375                       -2759.6553
    ##                                                                      upr
    ## Female calls in recording O375-Female calls in recording G544  2422.3434
    ## Male singing alone O375-Female calls in recording G544         2786.6338
    ## Undirected G544-Female calls in recording G544                  193.1876
    ## Male singing alone O375-Female calls in recording O375          469.5449
    ## Undirected G544-Female calls in recording O375                -2105.5045
    ## Undirected G544-Male singing alone O375                       -2507.0733
    ##                                                                   p adj
    ## Female calls in recording O375-Female calls in recording G544 0.0000000
    ## Male singing alone O375-Female calls in recording G544        0.0000000
    ## Undirected G544-Female calls in recording G544                0.9999937
    ## Male singing alone O375-Female calls in recording O375        0.0000000
    ## Undirected G544-Female calls in recording O375                0.0000000
    ## Undirected G544-Male singing alone O375                       0.0000000

Mean pitch is differs with context for O375 but not for G544.

### Mean entropy stats

``` r
# Run anova
anov <- aov(new_data$mean_entropy ~ new_data$contexts)
# output summary
summary(anov)
```

    ##                     Df Sum Sq Mean Sq F value Pr(>F)    
    ## new_data$contexts    3  1.955  0.6516   33.52 <2e-16 ***
    ## Residuals         1216 23.637  0.0194                   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# Run tukey test for pairwise comparison
TukeyHSD(anov)
```

    ##   Tukey multiple comparisons of means
    ##     95% family-wise confidence level
    ## 
    ## Fit: aov(formula = new_data$mean_entropy ~ new_data$contexts)
    ## 
    ## $`new_data$contexts`
    ##                                                                        diff
    ## Female calls in recording O375-Female calls in recording G544  0.0332031943
    ## Male singing alone O375-Female calls in recording G544        -0.0794106001
    ## Undirected G544-Female calls in recording G544                -0.0799886621
    ## Male singing alone O375-Female calls in recording O375        -0.1126137944
    ## Undirected G544-Female calls in recording O375                -0.1131918565
    ## Undirected G544-Male singing alone O375                       -0.0005780621
    ##                                                                       lwr
    ## Female calls in recording O375-Female calls in recording G544 -0.02531571
    ## Male singing alone O375-Female calls in recording G544        -0.13197076
    ## Undirected G544-Female calls in recording G544                -0.14641280
    ## Male singing alone O375-Female calls in recording O375        -0.14321102
    ## Undirected G544-Female calls in recording O375                -0.16404248
    ## Undirected G544-Male singing alone O375                       -0.04444028
    ##                                                                       upr
    ## Female calls in recording O375-Female calls in recording G544  0.09172210
    ## Male singing alone O375-Female calls in recording G544        -0.02685044
    ## Undirected G544-Female calls in recording G544                -0.01356452
    ## Male singing alone O375-Female calls in recording O375        -0.08201657
    ## Undirected G544-Female calls in recording O375                -0.06234123
    ## Undirected G544-Male singing alone O375                        0.04328415
    ##                                                                   p adj
    ## Female calls in recording O375-Female calls in recording G544 0.4623975
    ## Male singing alone O375-Female calls in recording G544        0.0006187
    ## Undirected G544-Female calls in recording G544                0.0107276
    ## Male singing alone O375-Female calls in recording O375        0.0000000
    ## Undirected G544-Female calls in recording O375                0.0000001
    ## Undirected G544-Male singing alone O375                       0.9999860

Mean entropy is differs with context for both birds.

Just looking at how the accuracy changes with different k values.

``` r
set.seed(400)
ctrl <- trainControl(method="repeatedcv",repeats = 3) #,classProbs=TRUE,summaryFunction = twoClassSummary)
knnFit <- train(contexts ~ ., data = data.training, method = "knn", trControl = ctrl, preProcess = c("center","scale"), tuneLength = 20)

#Output of kNN fit
knnFit
```

    ## k-Nearest Neighbors 
    ## 
    ## 825 samples
    ##  16 predictor
    ##   2 classes: 'Female calls in recording O375', 'Male singing alone O375' 
    ## 
    ## Pre-processing: centered (16), scaled (16) 
    ## Resampling: Cross-Validated (10 fold, repeated 3 times) 
    ## Summary of sample sizes: 742, 743, 743, 743, 743, 743, ... 
    ## Resampling results across tuning parameters:
    ## 
    ##   k   Accuracy   Kappa    
    ##    5  0.8678640  0.2961444
    ##    7  0.8763955  0.3202761
    ##    9  0.8784181  0.3159664
    ##   11  0.8768069  0.2871440
    ##   13  0.8768020  0.2833154
    ##   15  0.8743581  0.2604346
    ##   17  0.8727369  0.2478508
    ##   19  0.8719337  0.2382095
    ##   21  0.8727273  0.2404710
    ##   23  0.8739419  0.2454084
    ##   25  0.8739419  0.2424052
    ##   27  0.8723306  0.2283040
    ##   29  0.8699013  0.2098597
    ##   31  0.8678835  0.1912041
    ##   33  0.8666835  0.1799411
    ##   35  0.8650574  0.1673602
    ##   37  0.8646557  0.1584554
    ##   39  0.8646557  0.1583129
    ##   41  0.8630395  0.1429604
    ##   43  0.8638476  0.1472019
    ## 
    ## Accuracy was used to select the optimal model using the largest value.
    ## The final value used for the model was k = 9.

Plot the result.

``` r
ggplot(knnFit, aes(x=k, y=Accuracy)) + ylim(0,1) + theme_classic()
```

![](Birdsong_analysis_files/figure-gfm/knnparametersplot-1.png)<!-- -->

It doesn’t seem to matter which numbers we choose for neighbors.

## PCA (Principal component analysis)

Will demention reduction separate out the songs from different contexts?

### Bird \#1 (G544)

``` r
bs2.pca <- prcomp(birdsong1[,2:12])


library(ggfortify)
pca.plot <- autoplot(bs2.pca, data = birdsong1, colour = 'contexts')
pca.plot
```

![](Birdsong_analysis_files/figure-gfm/PCA1-1.png)<!-- -->

### Bird \#2 (O375)

``` r
bs2.pca <- prcomp(birdsong2[,2:12])

library(ggfortify)
pca.plot <- autoplot(bs2.pca, data = birdsong2, colour = 'context2')
pca.plot
```

![](Birdsong_analysis_files/figure-gfm/PCA-1.png)<!-- -->

## Exploring different plots

### Bird 1

``` r
library(FactoMineR)
library(factoextra)
```

    ## Welcome! Want to learn more? See two factoextra-related books at https://goo.gl/ve3WBa

``` r
new_birdsong1 <- birdsong1[,c(2:16,27:28)]
bird1.pca <- PCA(new_birdsong1[,-17], graph = FALSE)

fviz_pca_ind(bird1.pca,
             geom.ind = "point", # show points only (nbut not "text")
             col.ind = new_birdsong1$contexts, # color by groups
             palette = c("#00AFBB", "#E7B800"),
             addEllipses = TRUE, # Concentration ellipses
             legend.title = "Groups"
             )
```

![](Birdsong_analysis_files/figure-gfm/PCAbird1_2-1.png)<!-- -->

### Bird 2

``` r
new_birdsong2 <- birdsong2[,c(2:16,27:28)]

bird2.pca <- PCA(new_birdsong2[,-17], graph = FALSE)

fviz_pca_ind(bird2.pca,
             geom.ind = "point", # show points only (nbut not "text")
             col.ind = new_birdsong2$contexts, # color by groups
             palette = c("#00AFBB", "#E7B800"),
             addEllipses = TRUE, # Concentration ellipses
             legend.title = "Groups"
)
```

![](Birdsong_analysis_files/figure-gfm/PCAbird2_2-1.png)<!-- -->

## Conclusion 2

Although the songs that are recorded in different contexts are
statistically different in song length, It is hard to distinguish song
types by just comparing the audio parameters. For more accuate analysis,
we will need more recordings of actual direct songs (verified through
behavioral analysis on video), which is time consuming.
