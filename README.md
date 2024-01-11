Python Group Project

In our housing prices competition project, we've diligently documented our work, placing a strong emphasis on extracting meaningful insights throughout the entire process. To streamline our analysis, we categorized variables into numeric, categorical, and discrete based on their inherent characteristics.

Our exploratory data analysis (EDA) was extensive, leveraging Seaborn to create a series of graphical plots that allowed us to intuitively grasp the relationships among different variables. We delved into correlations among numeric variables and their correlation with the target variable, identifying crucial factors influencing housing prices.

Taking a closer look at categorical variables, we examined how the mean of the target variable varied across subcategories. For some variables with intrinsic ordering, we graded the levels accordingly, enhancing our understanding of their impact.

To address missing data, the MissForest algorithm was applied to numeric and discrete variables, ensuring a robust dataset. For categorical variables, missing values were replaced by a 'Missing' indicator.

In our modeling phase, we employed a diverse set of algorithms, including Linear Regression, ElasticNet, XGBoost, and RandomForest. Hyper-parameter tuning through GridSearch and RandomizedSearch fine-tuned these models, optimizing their performance.

Identifying the top 15 important features using XGBoost and RandomForest, we gained valuable insights into the key factors influencing housing prices. To enhance predictive accuracy, we implemented a Stacked Regressor, strategically combining predictions from individual models. Our approach was comprehensive, ensuring that every step contributed to the overall success of the project.
