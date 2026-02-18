### **Module:** ***CINeMA export***

**BACKGROUND**

CINeMA (Confidence in Network Meta-Analysis) is a method of assessing confidence in the findings of a network meta-analysis. The easiest way to apply the method is to use the accompanying online application, which is also called CINeMA ([https://cinema.ispm.unibe.ch/](https://cinema.ispm.unibe.ch/)). Previously, in order to use the CINeMA app, the meta-analysis model had to be applied within the app itself. This module allows the results from an NMA fit in MetaInsight to be downloaded, in a file that can then be uploaded into CINeMA. Currently, only the frequentist and Bayesian NMA models are supported; results from meta-regression or baseline risk meta-regression cannot be exported.

**IMPLEMENTATION**

There are four options available for the download, corresponding to the frequentist or Bayesian NMA, and to the full uploaded dataset or a smaller dataset with studies excluded by the user (in the Study results module under the Summary tab).

The CINeMA method requires treatment effect statistics for direct and indirect evidence separately, as well as overall. It also requires estimates of the contribution each study makes towards the treatment effect estimates. Due to computational complexity in the former, and undeveloped theory in the latter, MetaInsight always calculates these values from the frequentist model. For the purpose of the CINeMA method these frequentist approximations suffice.

**REFERENCES**

Nikolakopoulou A, Higgins JPT, Papakonstantinou T, Chaimani A, Del Giovane C, Egger M, et al. (2020) CINeMA: An approach for
assessing confidence in the results of a network  meta-analysis. PLoS Med 17(4): e1003082. https://doi.org/10.1371/journal.pmed.1003082
