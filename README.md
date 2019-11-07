## Modeling MLB Statistical Batting Line with Linear Mixed Models

This is the remote repository for work demonstrating the use of LMMs to predict batting lines of Major League Baseball Players.

This work heavily references existing work, including:

- This talk by Don Hedeker https://bstt513.class.uic.edu/L1LS_SAS_SPSS.pdf
- The excellent documentation of SAS's proc mixed: https://support.sas.com/documentation/cdl/en/statug/63033/HTML/default/viewer.htm#statug_mixed_sect022.htm
- A discussion of Stein's paradox describing the benefits of shrunken fixed effects parameters: http://statweb.stanford.edu/~ckirby/brad/other/Article1977.pdf

Predicting MLB player statistics into the future is challenging for several reasons, but here are two:

-	Small data. A long illustrious career will be over in 20 years. Even if we study a batting line on a monthly basis instead of yearly, the number of rows needed for a typical machine learning model would be insufficient. 
-	There is inherent correlation. It stands to reason that seasons (or months) that are closer together are more similar than seasons (or months) that are farther apart in a players’ career. One technique that might be considered is a time series, but that too may require more data than we have at our disposal. Generating predictions for new young players – while not impossible – would also be challenging.

The LMM allows us to account for between-player differences while accounting for correlated measures of the same player over time. 

## Brief description

This work pulls data from the [Lahman](https://github.com/cdalzell/Lahman) package. The LMM is fit with batting average as the target and the following as fixed effects:

- GS = Games started in a season
- soPer = strikeout percentage of outs
- yearService = the number of years the player has been in the league at the time of observation

Data is gathered and collected in the `pullData.R` code using mostly [tidy data principles](https://r4ds.had.co.nz/tidy-data.html).

The following random effects are used:

- intercept
- slope (yearService)

This creates a player-specific change to the population-level intercept and yearService coefficients.

## Preliminary Results

A train/test set was created to demonstrate the efficacy of this model on players that have not been included with the model construction. For simplicity I pulled data for all players debuting at or after 1993 and only considered mostly starters and careers lasting at least 5 years.

Root mean square errors were calculated for each player. Despite the simplicity (and frankly lack of data) collected, the models perform quite well.  Latent player-specific effects are powerful.

Here are the predicted/actual batting averages for the worst-fitting players in the training data set:

![](plots/plotTrainWorst.png)

For being the players with the 6 worst RMSEs in the model, they do quite well. Perhaps not surprising as they're from the training set.

Here are the worst 6 from the test set. Here I should note that the test set contains player careers not used to train the model. Since they were not included in the original model, their predictions are only on the level of the fixed effects (i.e. no player-specific correctios to the intercept and yearService coefficients). 

![](plots/plotTestWorst.png)

This is still more effective than an ordinary least squares model, as the fixed effects parameter estimates are shrunk as described in the Efron link above. *That said*, this is unrealistic because we don't know the future values of the fixed effects parameters (games started, strike-out percentage, etc), so this plot isn't realistic either!

## Predict Future Performance Based on Mid-Career Data

In practice we'll want to use all the data available to use. A more realistic situation occurs when you have a player's performance history up until the current date and you'd like to forecast what happens years from now. In this case things change in the sense that any continuous fixed covariates used must either be forecasted themselves or removed from the model, so that only an intercept and time-factor is included. Then we need only use the changing time value and the random effects estimates to predict each year.

Here's the worst test set predictions when we have time as a fixed and random component and remove the other fixed effects:

![](plots/plotTestWorst3.png)

We get some straight line nonsense. If we continually update the BLUP (random effects vector) though, we can get closer and closer to a good prediction as long as we have enough data points to learn from at the start. For example, take Alcides Escobar (top left plot) from before and use the first 3 known points to calculate and update the BLUPs:

![](plots/escobal02_3.png)

Clearly 3 years isn't enough. 

