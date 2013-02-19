My SEO Scripts
========================================
Adrian Haldenby a.haldenby@gamil.com

Just a little repo to store any usefull R and python SEO scripts

**CTR estimator**

Estimates Google click-through rate with generalized additive model based on webmaster tools data dump or, if you don't have enough data use the data from the  http://www.optify.net study.

**SEO Competitor Opportunities**

Identify high level keyword opportunities based on a tokenized keywords. The process is as follows: 
* Consumes three organic keyword dumps from SEMrush
* Uses CTR Estimator to estimate keyword traffic  
* Breaks out each keyword into 1-grams with stemming, punctuation and stop words removed
* Compares tokenized words between competitors and ourselves by traffic volume, plotting keyword theme opportunity index on a dotplot

