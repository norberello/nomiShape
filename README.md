# nomiShape
**nomiShape** provides tools for visualizing and summarizing **nominal (categorical, unordered) variables**, with a focus on understanding the *shape* of categorical frequency distributions rather than relying on arbitrary category ordering.

The package introduces **centered frequency plots**, where categories are ordered from the most frequent at the center toward less frequent categories on both sides. This design helps reveal patterns such as dominance, uniformity, symmetry, skewness, and long-tail behavior in nominal data.

The package is designed for **exploratory data analysis**, teaching, and methodological research, and produces **ggplot2-compatible** visualizations.

---

## Installation

You can install the released version from CRAN:

```r
install.packages("nomiShape")
library(nomiShape)
```
