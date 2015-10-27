* repeatedly attaches data frames, littering the namespace with generic and short variable names like "y" which on some students computers generated 'masked .Global_env' errors
* doesn't provide an example of how to change a value on line p.35 (divide 10) and saving the file, recommending instead to edit the CSV in a spreadsheet - R is perfectly capable of this.

```
das_original_table <- read.csv("das.csv")
das_mod_table <- das_original_table
das_mod_table$y[50] <- das_mod_table$y[50]/10
write.table(das_mod_table, file="das_mod.csv", sep =",")
```

* doesn't introduce `setwd` as an option to change working directory to the folder with data files
* heteroscedasticity = non-constant variance
* `plot` defaults to box-and-whiskers with categorical data
