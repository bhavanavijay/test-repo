Probability Practice
====================

Part A
------

*P**r**o**b*(*R**C*)=0.3

*P**r**o**b*(*R**C*/*Y**e**s*)=*P**r**o**b*(*R**C*/*N**o*)=0.5

*P**r**o**b*(*T**C*)=0.7

*P**r**o**b*(*Y**e**s*)=0.65

*P**r**o**b*(*N**o*)−0.35

*P**r**o**b*(*T**C*/*Y**e**s*)=?

*P**r**o**b*(*Y**e**s*)=*P**r**o**b*(*R**C*/*Y**e**s*)\**P**r**o**b*(*R**C*)+*P**r**o**b*(*T**C*/*Y**e**s*)\**P**r**o**b*(*T**C*)

0.65 = 0.5 \* 0.3 + *P**r**o**b*(*T**C*/*Y**e**s*)\*0.7

*P**r**o**b*(*T**C*/*Y**e**s*)= \* \*0.7142857 \* \*

#### Therefore, the fraction of people who are truthful clickers and answered yes is 0.71

Part B
------

*P**r**o**b*(*P**o**s**i**t**i**v**e*/*D**i**s**e**a**s**e*)=0.993

*P**r**o**b*(*N**e**g**a**t**i**v**e*/*N**o**D**i**s**e**a**s**e*)=0.9999

*P**r**o**b*(*D**i**s**e**a**s**e*)=0.000025

*P**r**o**b*(*D**i**s**e**a**s**e*/*P**o**s**i**t**i**v**e*)=*P**r**o**b*(*D**i**s**e**a**s**e*)\**P**r**o**b*(*P**o**s**i**t**i**v**e*/*D**i**s**e**a**s**e*)/\[*P**r**o**b*(*D**i**s**e**a**s**e*)\**P**r**o**b*(*P**o**s**i**t**i**v**e*/*D**i**s**e**a**s**e*)+*P**r**o**b*(*N**o**D**i**s**e**a**s**e*)\**P**r**o**b*(*P**o**s**i**t**i**v**e*/*N**o**D**i**s**e**a**s**e*)

*P**r**o**b*(*D**i**s**e**a**s**e*/*P**o**s**i**t**v**e*)=0.000025 \* 0.993/\[0.000025 \* 0.993 + (1 − 0.000025)\*(1 − 0.9999)\]

*P**r**o**b*(*D**i**s**e**a**s**e*/*P**o**s**i**t**v**e*)= \* \*0.1988824 \* \*

#### Therefore, The probability of someone having a disease having tested positive is about 20%

In implementing a universal testing policy, there might be problems as
the test accurately testing a disease positive is only about 1/5ths.
Hence, there are about 80% positive results that are false positives.

Market Segmentation
===================

    ## Warning: namespace 'DBI' is not available and has been replaced
    ## by .GlobalEnv when processing object 'call.'

    ## Warning: namespace 'DBI' is not available and has been replaced
    ## by .GlobalEnv when processing object 'call.'

Lets do some exploratory data analysis by looking at correlations of the
different types

    soc_mkt = soc_mkt_all[,-1]
    corr_mat = cor(soc_mkt)
    corrplot(corr_mat, type="lower")

![](Ex1_Q1_Q4_files/figure-markdown_strict/unnamed-chunk-2-1.png)

Looking at the correlation plot, dots that are darker indicate a strong
relationship. Results here, seem to be intuitive and in the right
direction. The highest correlation is between Health-Nutrition and
Personal Fitness, as they are related topics, tweeting about these topic
is highly possible. There also seems to be high correlation between
Cooking and Fashion, and Cooking and Beauty, which intuitively make
sense. We observe a relationship between Online Gaming and
College/University often occur together, as a lot of college students
are interested in gaming this also conforms with human intuition.

Now we can try our hand at clustering and confirm our hypothesis.

#### Heirarchical Clustering

    soc_mkt_scaled <- scale(soc_mkt, center=TRUE, scale=TRUE)

    ##Heirarchical Clustering ## 
    par(mfrow=c(2,2))
    # Form a pairwise distance matrix using the dist function
    distance_matrix = dist(soc_mkt_scaled, method='euclidean')

    # Now run hierarchical clustering
    hier_p = hclust(distance_matrix, method='average')
    cluster1 = cutree(hier_p, k=5)

    # Plot the dendrogram
    plot(hier_p, cex=0.8)

    # Now run hierarchical clustering
    hier_p = hclust(distance_matrix, method='centroid')
    cluster2 = cutree(hier_p, k=5)

    # Plot the dendrogram
    plot(hier_p, cex=0.8)

    # Now run hierarchical clustering
    hier_p = hclust(distance_matrix, method='complete')
    cluster3 = cutree(hier_p, k=10)

    # Plot the dendrogram
    plot(hier_p, cex=0.8)

    # Now run hierarchical clustering
    hier_p = hclust(distance_matrix, method='single')
    cluster4 = cutree(hier_p, k=5)

    # Plot the dendrogram
    plot(hier_p, cex=0.8)

![](Ex1_Q1_Q4_files/figure-markdown_strict/unnamed-chunk-3-1.png)

    #ind =which(cluster1 == 3)
    summary(factor(cluster1))

    ##    1    2    3    4    5 
    ## 7867   10    2    1    2

    summary(factor(cluster2))

    ##    1    2    3    4    5 
    ## 7876    2    1    2    1

    summary(factor(cluster3))

    ##    1    2    3    4    5    6    7    8    9   10 
    ##  487 5628  284  859  130   16  410   49    9   10

    summary(factor(cluster4))

    ##    1    2    3    4    5 
    ## 7831   47    2    1    1

Hierarchical clustering results are hard to interpret and even after we
cut the trees to 5. Hierarchical clustering for this data is not useful
since there arent tiers or levels among the distribution and so it ends
up grouping incorrectly into a single cluster only.

Next lets try Principle Component Analysis

#### PCA

    ## PCA ## 

    pc1 = prcomp(as.matrix(soc_mkt_scaled), scale.=TRUE)
    #compute standard deviation of each principal component
    std_dev <- pc1$sdev
    #compute variance
    pr_var <- std_dev^2
    #proportion of variance explained
    prop_varex <- pr_var/sum(pr_var)
    par(mfrow=c(1,1))
    plot(cumsum(prop_varex), xlab = "Principal Component",
                 ylab = "Proportion of Variance Explained",
                 type = "b")

![](Ex1_Q1_Q4_files/figure-markdown_strict/unnamed-chunk-4-1.png)

    loadings = pc1$rotation
    scores = pc1$x
    #autoplot(pc1,loadings=TRUE)
    autoplot(pc1, loadings = TRUE, loadings.colour = 'blue',
             loadings.label = TRUE, loadings.label.size = 4) + theme_bw()

![](Ex1_Q1_Q4_files/figure-markdown_strict/unnamed-chunk-4-2.png)

80% of the variance is explained by about 15 principle components. 50%
of the variance is covered with just 6 PCs

Next, we will try clustering and try to visualize these clusters using
PCs to validate them.

#### K-means

We used elbow method for K-means to get the vaue of k that understands
cluster composition.

    k.max <-20 # Maximal number of clusters
    wss <- sapply(1:k.max, 
            function(k){kmeans(soc_mkt_scaled, k, nstart=10 )$tot.withinss})

    ## Warning: did not converge in 10 iterations

    ## Warning: did not converge in 10 iterations

    plot(1:k.max, wss,
           type="b", pch = 19, frame = FALSE, 
           xlab="Number of clusters K",
           ylab="Total within-clusters sum of squares")

![](Ex1_Q1_Q4_files/figure-markdown_strict/unnamed-chunk-6-1.png)

The elbow starts to tilt at about 8 clusters. That should be our answer.

Below we will look at 4 clusters first to see why lesser custers give us
no good answer.

**Trying with 4 clusters:**

    # K-means clustering with 4
    set.seed(1)
    km.res1 <- kmeans(soc_mkt_scaled, 4, nstart = 25)
    # k-means group number of each observation

    # Visualize k-means clusters
    fviz_cluster(km.res1, data = soc_mkt_scaled, geom = "point",
                 stand = FALSE, frame.type = "norm")

    ## Warning: argument frame is deprecated; please use ellipse instead.

    ## Warning: argument frame.type is deprecated; please use ellipse.type
    ## instead.

![](Ex1_Q1_Q4_files/figure-markdown_strict/unnamed-chunk-7-1.png)

    clusters_pars = km.res1$centers
    transposed = t(clusters_pars)
    cluster_1 = transposed[which(abs(transposed[,1])>=0.4),1]
    cluster_2 = transposed[which(abs(transposed[,2])>=0.4),2]
    cluster_3 = transposed[which(abs(transposed[,3])>=0.4),3]
    cluster_4 = transposed[which(abs(transposed[,4])>=0.4),4]

    cat("\n")

    cluster_1

    ##     travel   politics       news  computers automotive 
    ##   1.768714   2.375881   1.941556   1.549122   1.122043

    cat("\n")

    cluster_2

    ## sports_fandom          food        family        crafts      religion 
    ##     2.0214691     1.8089459     1.4618754     0.6915247     2.2295333 
    ##     parenting        school 
    ##     2.1003788     1.6298873

    cat("\n")

    cluster_3

    ## numeric(0)

    cat("\n")

    cluster_4

    ##          chatter    photo_sharing    uncategorized            music 
    ##        0.5492028        0.7981651        0.4156293        0.4847336 
    ##         shopping health_nutrition   sports_playing          cooking 
    ##        0.5762315        0.6590026        0.4000114        0.8742163 
    ##              eco         outdoors           beauty personal_fitness 
    ##        0.4171848        0.5438048        0.6516625        0.6722260 
    ##          fashion 
    ##        0.7667315

    cat("\n")

    fact_clust <- factor(km.res1$cluster)
    summary(fact_clust)

    ##    1    2    3    4 
    ##  714  768 4563 1837

With 4 clusters, one cluster is getting grouped generally with very weak
relationships to the center. This cluster has 4563 observations and that
is 4563 observations which are wasted into a general group. We suspect
this could be the influence of spam and chatter. To see more detailed
clusters we will next see 8 clusters.

#### Trying with 8 clusters to see if we can fine tune without over splitting

    # K-means clustering with 8
    set.seed(1)
    km.res <- kmeans(soc_mkt_scaled, 8, nstart = 25)
    # k-means group number of each observation

    # Visualize k-means clusters
    fviz_cluster(km.res, data = soc_mkt_scaled, geom = "point",
                 stand = FALSE,  frame.type = "norm")

    ## Warning: argument frame is deprecated; please use ellipse instead.

    ## Warning: argument frame.type is deprecated; please use ellipse.type
    ## instead.

![](Ex1_Q1_Q4_files/figure-markdown_strict/unnamed-chunk-9-1.png)

    clusters_pars = km.res$centers
    transposed = t(clusters_pars)
    cluster_1 = transposed[which(abs(transposed[,1])>=0.5),1]
    cluster_2 = transposed[which(abs(transposed[,2])>=0.5),2]
    cluster_3 = transposed[which(abs(transposed[,3])>=0.5),3]
    cluster_4 = transposed[which(abs(transposed[,4])>=0.5),4]
    cluster_5 = transposed[which(abs(transposed[,5])>=0.5),5]
    cluster_6 = transposed[which(abs(transposed[,6])>=0.5),6]
    cluster_7 = transposed[which(abs(transposed[,7])>=0.5),7]
    cluster_8 = transposed[which(abs(transposed[,8])>=0.5),8]

    cat("\n")

    cluster_1

    ## numeric(0)

    cat("\n")

    cluster_2

    ## photo_sharing         music       cooking        beauty       fashion 
    ##     1.2128710     0.5290238     2.7779173     2.5561090     2.6469317

    cat("\n")

    cluster_3

    ##     travel   politics       news  computers automotive 
    ##   1.896260   2.481778   2.004493   1.668934   1.053455

    cat("\n")

    cluster_4

    ## health_nutrition              eco         outdoors personal_fitness 
    ##        2.1937620        0.5265472        1.7197451        2.1597033

    cat("\n")

    cluster_5

    ##  online_gaming    college_uni sports_playing 
    ##       3.498681       3.272318       2.177727

    cat("\n")

    cluster_6

    ##       chatter photo_sharing       tv_film      shopping 
    ##     1.2094136     0.8714549     0.5044661     1.1076373

    cat("\n")

    cluster_7

    ##      spam     adult 
    ## 12.418865  3.750222

    cat("\n")

    cluster_8

    ## sports_fandom          food        family        crafts      religion 
    ##     2.0679074     1.8422809     1.5030332     0.7163354     2.2590238 
    ##     parenting        school 
    ##     2.1499150     1.6727291

    cat("\n")

    fact_clust <- factor(km.res$cluster)
    summary(fact_clust)

    ##    1    2    3    4    5    6    7    8 
    ## 3561  513  617  801  368 1270   49  703

The cluster composition we got from eight clusters makes sense
intuitively and is interpretable. We earlier saw a huge cluster getting
generalized. Here we again have a group of 3561 users who are not
strongly biased or opinionated about any specific genre of topic. The
clusters of 8 did do a good job singling out a spam and adult cluster
from the more specific clusters which can do a good job with targetted
marketing.

Lets see our 8 clusters on the principle component scatter plots we saw
earlier:

    par(mfrow=c(1,1))

    fact_clust <- factor(km.res$cluster)
    summary(fact_clust)

    ##    1    2    3    4    5    6    7    8 
    ## 3561  513  617  801  368 1270   49  703

    ggplot(pc1,aes(x=pc1$x[,1],y=pc1$x[,2],col=fact_clust))+
       geom_point(size=3,alpha=0.5)+
       theme_bw()

![](Ex1_Q1_Q4_files/figure-markdown_strict/unnamed-chunk-11-1.png)

    ggplot(pc1,aes(x=pc1$x[,2],y=pc1$x[,3],col=fact_clust))+
       geom_point(size=3,alpha=0.5)+
       theme_bw()

![](Ex1_Q1_Q4_files/figure-markdown_strict/unnamed-chunk-11-2.png)

These scatterplots seem to confirm our clusters very well. 8 clusters
seems to give us specific target groups by successfully removing the
spammers into one of the clusters.

In order to visualize these clusters and understand their prominent
characteristics, we used word cloud.

    # # A word cloud
    par(mfrow=c(3,3))
    library(wordcloud)

    ## Loading required package: RColorBrewer

    #t(colnames(soc_mkt_scaled))
    colnames(soc_mkt_scaled)[19] <-"econ"
    colnames(soc_mkt_scaled)[25] <-"arts"
    for (i in 2:8) {
    wordcloud(colnames(soc_mkt_scaled), km.res$centers[i,], min.freq=0, max.words=100,scale=c(4,.4))
    }

    ## Warning in wordcloud(colnames(soc_mkt_scaled), km.res$centers[i, ],
    ## min.freq = 0, : health_nutrition could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(colnames(soc_mkt_scaled), km.res$centers[i, ],
    ## min.freq = 0, : personal_fitness could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(colnames(soc_mkt_scaled), km.res$centers[i, ],
    ## min.freq = 0, : online_gaming could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(colnames(soc_mkt_scaled), km.res$centers[i, ],
    ## min.freq = 0, : shopping could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(colnames(soc_mkt_scaled), km.res$centers[i, ],
    ## min.freq = 0, : chatter could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(colnames(soc_mkt_scaled), km.res$centers[i, ],
    ## min.freq = 0, : sports_fandom could not be fit on page. It will not be
    ## plotted.

![](Ex1_Q1_Q4_files/figure-markdown_strict/unnamed-chunk-12-1.png)

### Conclusion

We found 8 distinct clusters to be the best way to visualize these users
and target them for marketing.

-   Cluster 1 - 3561 users. These users tweet about everything or they
    are inactive. They dont specifically tweet about anything topical.
    We could use these customers along with any other cluster if more
    users are required in the campaign
-   Cluster 2 - 513 users. This cluster is a young female cluster. They
    are active on social media, they like beauty and fashion.
-   Cluster 3 - 617 users. This is a corporate male cluster. They are
    into computers, politics, news and they travel.
-   Cluster 4 - 801 users. These group of people are big
    outdoor enthusiasts.
-   Cluster 5 - 368 users. Young college coing people who are
    into sports.
-   Cluster 6 - 1270 users. Seems like a young demographic that is into
    a bunch of stuff but mainly into shopping and being active of
    social media. This cluster can be grouped with cluster 2 for a
    bigger population of young users.
-   Cluster 7 - 49 users. Prominent spammers who talk alot about
    adult stuff.
-   Cluster 8 - 703 users. This is a typical parent group. Into
    religion, school, family and food.
