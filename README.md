# TravelOAC

This is the code and associated files for the TravelOAC project. The paper is currently in preparation and this code will be published alongside the paper. At the moment this is just an easy way to make the code available to others.

Any questions, please email me (n.bearman@liverpool.ac.uk).

The [keywords](clusters-keywords.Rmd) used for each cluster are also avaliable; please add to them as you see fit. 

There are a number of different files with R code. Here is a brief description, and the rough order they should be used in:

**1-calc-nearest-stop.R**  
- used to read in and preprocess OA centriod data and NAPTAN data  
- use Routino to calculate routes for each transport type / each OA  
- work out nearest stop to each OA population weighted centriod  

**2-census-data.R**  
- reads in census data  and aggregates it  
- looks at how the different variables are correlated with each other  
- combines census data with distances  

**3-calc-clusters.R**  
- create elbow plot to establish number of clusters required  
- calculate the K-means clustering for the classification  

These are some additional files:

**A-clustergrams.R**  
- Alternative method for establishing number of clusters for K-means cluster classification  

**B-compare-travel-to-OAC.r**  
- Code to compare TravelOAC to OAC, splitting up travel variables by 2011 OAC  

These files are setup for use my computer, so you will need to upadte the file paths to work. Also, Routino needs to be setup for the routing element - see [www.routino.org](www.routino.org) for details.

Please feel free to fork the repo, and make pull requests.
