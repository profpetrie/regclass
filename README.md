Issues:

1.  Building/checking as CRAN says I have too many "depends" packages.  
I've checked trying to update to Imports and modifying the NAMESPACE to no avail.  
I've followed examples, but can only get it to work with depends.

Depends: includes the non-default packages:
  ‘bestglm’ ‘leaps’ ‘VGAM’ ‘rpart’ ‘rpart.plot’ ‘randomForest’
Adding so many packages to the search path is excessive and importing
selectively is preferable.

2.  The naming of my functions goes against conventions because I use a . instead of a _
I have changed all.correlations, outlier.demo, and influence.plot to all_correlations, etc., and have amended the help file
to include an alias (so that you can look it up with ?all.correlations), but I'd REALLY prefer to have them called with the .
instead of of the _ because my published book refers to them with the .   If I have to make the plunge and rename those three with _ I should
go back and rename ALL my function that use . and that's a lot!
