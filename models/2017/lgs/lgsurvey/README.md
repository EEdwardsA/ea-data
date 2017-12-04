## 2017 Local Group Survey Analysis

This code is made public for reproducible analysis, but the dataset the code relies upon unfortunately cannot be shared. Only those in the EA survey team will actually be able to generate the analysis.

-

To run the model and analysis if you have the data:

```
cd ~/path/to/ea-data
R
```

```R
run("2017/lgs/lgs")
```

-

Run the full data pipeline, assuming you possess the confidential un-anonymous CSVs in your `data` directory:

```R
source("models/2017/lgs/lgsurvey/import.R")
run("2017/lgs/lgs")
```
