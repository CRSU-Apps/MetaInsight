# Guide For Offline Users of MetaInsight  

This guide is designed to for users who wish to download MetaInsight and run the app on their own machine. We have attempted to provide step-by-step instructions, but users with no experience of using R Studio may find it helpful to follow a short tutorial such as those found in the first bulletpoint from [RStudio Education](https://education.rstudio.com/learn/beginner/)  

1 Download and install R from [CRAN](https://cran.r-project.org/)  
2 Download and install [R Studio](https://posit.co/download/rstudio-desktop/) (free version)]  
3.	Download and install the latest version of JAGS from JAGS: Just Another Gibbs Sampler - Browse /JAGS/4.x/Windows at SourceForge.net (or JAGS: Just Another Gibbs Sampler - Browse /JAGS/4.x/Mac OS X at SourceForge.net if using a Mac)
4.	Download the latest version of MetaInsight from Github CRSU-Apps/MetaInsight: An interactive web-based tool for analyzing, interrogating, and visualizing network meta-analyses using R-shiny (github.com). Click on code -> Download ZIP to download all the files. Unzip the folder (likely automatically named ‘Metalnsight-main’). You can move or rename this file if you wish but do not change the file structure within this folder
5.	The MetaInsight app requires a number of R packages to run. Open R Studio and click File -> Open File to open the file named ‘MetaInsight_new_user.R’  contained within the Github folder downloaded in Step 4. Run this file by selecting all the lines of code and clicking ‘Run’. This will take some time to install all the packages; while R is still working, a red ‘Stop’ symbol will appear within the console.  Wait until this disappears before moving to the next step.
6.	In RStudio, open either the file ‘server.R’ or ‘ui.R’
7.	Run the MetaInsight app by clicking on ‘Run app’

