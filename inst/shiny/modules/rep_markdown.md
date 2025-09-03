### **Module:** ***Session Code***

**BACKGROUND**

Via the *Session Code* module, the user can download files that document the analyses run in a given *MetaInsight* session (including executable code that can reproduce them).  
This functionality supports reproducible science.

**IMPLEMENTATION**

Here, the user can download documented code that corresponds to the analyses run in the current session of *MetaInsight*. Two formats are available for download - the .qmd format
is an executable R script that will reproduce the analysis when run in an R session; the .html format is a report which can be opened in a web browser which contains all of the 
code chunks used in the analysis. You can choose to include the outputs generated in the analysis in the .html report by toggling the *Include outputs?* switch. If you would like
a permanent record of your analysis, but don't intend to change it in the future the .html format, including the outputs is best. If you would like to alter your analysis in the 
future then the use the .qmd format. If you download the .qmd you can also produce the .html by using `quarto::quarto_render()`

The *MetaInsight* session code .qmd file is composed of a chain of code chunks with module functions that are for internal use in *MetaInsight*. Each of these functions corresponds
to a single module that the user ran during the session. Users can modify their analysis, for example by adding new studies or removing different studies and rerun their analysis.
Open the .qmd in RStudio, click on “Run” in the upper-right corner, and run chunk by chunk or all at once. 

