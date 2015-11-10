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

### Conditioning plots (a.k.a. trellis plots)

via [1](https://rollingyours.wordpress.com/2014/02/17/conditioning-and-grouping-with-lattice-graphics/) [2](https://stat.ethz.ch/R-manual/R-devel/library/graphics/html/coplot.html) [3](http://astrostatistics.psu.edu/su07/R/html/graphics/html/coplot.html) [4](http://www.itl.nist.gov/div898/handbook/eda/section3/condplot.htm)

The conditioning plot allows us to view relationships across "panels" with common scales. Each panel contains a plot whose data is "conditional" upon records drawn from that record

* can be done in base R (`coplot`), or the `lattice`, `ggplot2`, or `grid` packages
 
```
coplot_table <- read.csv("coplot.csv")

# base R:
coplot(coplot_table$y ~ coplot_table$x | coplot_table$z)

# ggplot2 (failed) attempt according to syntax comparison given in http://ggplot2.org/book/appendices.pdf
# seemingly should be:
qplot(coplot_table$x, coplot_table$y, facets = coplot$z)

# error is not very informative...
# Error in formula.default(eval(parse(text = x)[[1L]])) : invalid formula
```

  
