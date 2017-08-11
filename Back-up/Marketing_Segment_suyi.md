    social_market <- read.csv('~/Documents/SourceTree/STA380/data/social_marketing.csv')

> yue: Why do you discard these columns?

    discard<-c('spam','adult','uncategorize','chatter')
    social_market<-social_market[,!names(social_market)%in%discard]

> Since certain high-frequency terms have little discriminating power
> like photo-sharing, we use TF-IDF to recalculate the weight of each
> term for every follower. TF stands for term-frequency, measuring how
> frequent a term occurs in a follower's tweets: the more frequent a
> term occurs, the more important it is to the follower; IDF stands for
> inverse-document-frequency, measuring how frequent the term occurs in
> the whole dataset: the more frequent a term occurs, the less important
> it is to every follower.

    #TFIDF
    TF<-social_market[,-1]/rowSums(social_market[,-1])
    tmp=sort(apply(TF,2,mean),decreasing=TRUE)
    EXI_NUM<-apply(social_market[,-1]>0,2,function(x){table(x)['TRUE']})
    IDF<-as.numeric(log(nrow(social_market)/EXI_NUM))
    TFIDF = data.frame(t(t(TF)*IDF))

> We use 'cosine' as a measurement for the similarity. It calculates the
> cosine of the angle between two vectors It measures difference in
> orientation instead of magnitude. For example, we have 3 follower
> A,B,C with features like A={'travelling':10,'cooking':5},
> B={'travelling':20,'cooking':10}, C={'travelling':10,'cooking':12}, we
> would consider A more similar with B than C even though A and C are
> 'closer'.

    #hclust
    d.cosine<-dist(TFIDF,method='cosine')
    hc.ratio.cosine<-hclust(d.cosine,method='ward.D2')

> By looking at different outputs of different Ks, we chose k=3 as our
> final parameter since its output makes more sense to us.

    #hclust
    #choose cluster 3
    out.cluster = cutree(hc.ratio.cosine,k=3)
    TFIDF['cluster'] = out.cluster
    tfidf<-c()
    names<-c()
    for(j in 1:3){
        cate = sort(apply(TFIDF[TFIDF['cluster']==j,-ncol(TFIDF)],2,mean),decreasing=TRUE)[1:5]
        name = names(cate)
        tfidf<-c(tfidf,unname(cate))
        names<-c(names,name)
    }
    cate.df = data.frame(names=names,tfidf_scores=tfidf,cluster=c(rep('A',length(tfidf)/3),rep('C',length(tfidf)/3),rep('B',length(tfidf)/3)))
    cate.df$names<-factor(cate.df$names, levels=unique(cate.df$names))

    colourCount = length(unique(cate.df$names))
    getPalette = colorRampPalette(brewer.pal(11, "Spectral"))

    ggplot(cate.df,mapping=aes(x=cluster,y=tfidf_scores,fill=names))+geom_bar(stat='identity',position='dodge')+theme(panel.grid.minor=element_blank(),panel.grid.major=element_blank())+scale_fill_manual(values=getPalette(colourCount))+theme_bw()

![](Marketing_Segment_suyi_files/figure-markdown_strict/unnamed-chunk-5-1.png)

    names_df<-data.frame('A'=cate.df$names[cate.df$cluster=='A'],'B'=cate.df$names[cate.df$cluster=='B'],'C'=cate.df$names[cate.df$cluster=='C'])
    pander(names_df)

<table style="width:67%;">
<colgroup>
<col width="26%" />
<col width="26%" />
<col width="13%" />
</colgroup>
<thead>
<tr class="header">
<th align="center">A</th>
<th align="center">B</th>
<th align="center">C</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="center">health_nutrition</td>
<td align="center">online_gaming</td>
<td align="center">politics</td>
</tr>
<tr class="even">
<td align="center">personal_fitness</td>
<td align="center">college_uni</td>
<td align="center">news</td>
</tr>
<tr class="odd">
<td align="center">outdoors</td>
<td align="center">sports_playing</td>
<td align="center">religion</td>
</tr>
<tr class="even">
<td align="center">cooking</td>
<td align="center">health_nutrition</td>
<td align="center">cooking</td>
</tr>
<tr class="odd">
<td align="center">food</td>
<td align="center">art</td>
<td align="center">shopping</td>
</tr>
</tbody>
</table>

> From the topics of high TFIDF-socres in the clusters, we can infer
> that first cluster represents people who care a lot about health,
> mostly likely housewives; the second cluster represents college/high
> school students; the third cluster represents people who care about
> current events, most likely working people. yue: what kind of
> marketing strategies?
