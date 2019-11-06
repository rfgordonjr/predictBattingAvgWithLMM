## Modeling MLB Statistical Batting Line with Linear Mixed Models

This is the remote repository for work demonstrating the use of LMMs to predict batting lines of Major League Baseball Players.

This work heavily references existing work, including:

- This talk by Don Hedeker https://bstt513.class.uic.edu/L1LS_SAS_SPSS.pdf
- The excellent documentation of SAS's proc mixed: https://support.sas.com/documentation/cdl/en/statug/63033/HTML/default/viewer.htm#statug_mixed_sect022.htm

Predicting MLB player statistics into the future is challenging for reasons, but here are two:

-	Small data. A long illustrious career will be over in 20 years. Even if we study a batting line on a monthly basis instead of yearly, the number of rows needed for a typical machine learning model would be insufficient. 
-	There is inherent correlation. It stands to reason that seasons (or months) that are closer together are more similar than seasons (or months) that are farther apart in a players’ career. One technique that might be considered is a time series, but that too may require more data than we have at our disposal. Generating predictions for new young players – while not impossible – would also be challenging.

The LMM allows us to account for between-player differences while accounting for correlated measures of the same player over time. 




