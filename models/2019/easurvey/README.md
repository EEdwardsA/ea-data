## 2019 EA Survey Analysis

Run the model and analysis:

```
cd ~/path/to/ea-data
R
```

```R
run("2019/easurvey")
```

-

Run the full data pipeline, assuming you possess the confidential un-anonymous CSVs in your `data` directory:

```R
source("models/2019/easurvey/ea_id.R")
source("models/2019/easurvey/convert_money.R")
source("models/2019/easurvey/munge.R")
run("2019/survey")
```
