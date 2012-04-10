---
layout: static
title: stat_gene
---




### Introduction

`stat_gene` is lower level API parsing a *TranscriptDb* object and create gene
structures, two geoms supported

 *  gene: showing a full transcripts with cds/utr/introns
 *  reduced_gene: reduce cds/utrl/introns to generate single gene structure.

### Objects
  * *TranscriptDb*
  
### Usage
  upcomming

### Examples
Load packages


{% highlight r %}
library(ggbio)
library(TxDb.Hsapiens.UCSC.hg19.knownGene)
data(genesymbol, package = "biovizBase")
txdb <- TxDb.Hsapiens.UCSC.hg19.knownGene
{% endhighlight %}




Let's create a track for both geoms


{% highlight r %}
p1 <- ggplot() + stat_gene(txdb, which = genesymbol["RBM17"], fill = "gray40", 
    geom = "gene")
p2 <- ggplot() + stat_gene(txdb, which = genesymbol["RBM17"], geom = "reduced_gene")
tracks(p1, p2, heights = c(3, 1))
{% endhighlight %}

![plot of chunk tracks](http://tengfei.github.com/ggbio/stat/stat_gene-tracks1.png) 

{% highlight r %}
library(biovizBase)
p3 <- ggplot() + stat_gene(txdb, which = genesymbol["RBM17"], geom = "gene", 
    truncate.gaps = TRUE)
p3
{% endhighlight %}

![plot of chunk tracks](http://tengfei.github.com/ggbio/stat/stat_gene-tracks2.png) 

{% highlight r %}
autoplot(txdb, which = genesymbol["RBM17"], geom = "gene", truncate.gaps = TRUE)
{% endhighlight %}

![plot of chunk tracks](http://tengfei.github.com/ggbio/stat/stat_gene-tracks3.png) 


  