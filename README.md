My SEO Scripts
========================================
Adrian Haldenby a.haldenby@gamil.com

Just a little repo to store any usefull R and python SEO scripts

**SEO Competitor Opportunities**

Identify high level keyword opportunities based on a tokenized keywords. The process is as follows: 
* Consumes three organic keyword dumps from SEMrush
* Estimates traffic by keyword with simple GAM based on rank and keyword search volume
* Breaks out each keyword into 1-grams with stemming, punctuation and stop words removed
* Compares tokenized words between competitors and ourselves by traffic volume, plotting keyword theme opportunity index on a dotplot

