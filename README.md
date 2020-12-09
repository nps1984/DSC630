# DSC630
This project was an attempt to predict WAR (wins above replacement) for MLB players.

The notebook uses the pybaseball library to download MLB statistics. It then uses the sklearn library to perform feature selection. And finally it goes through a pipeline and 
grid search cross validation to find the best model parameters.

I didn't love the output of the model identified from the GridSearchCV, so I manually generated several models to review output. 
