## 2018 EA Survey Analysis

Run the model and analysis:

```
cd ~/path/to/ea-data
R
```

```R
run("2018/easurvey")
```

-

Run the full data pipeline, assuming you possess the confidential un-anonymous CSVs in your `data` directory:

```R
source("models/2018/easurvey/ea_id.R")
source("models/2018/easurvey/convert_money.R")
run("2018/survey")
```
