# TimeSeriesR: Time Series Machine Learning in R

The TimeSeriesR package was used to verify the effectiveness of the solution proposed in [^1], in terms of cluster validity and execution time (on real UK smart meter data [^2]). Please note that this implementation is made as a proof of concept, and not as a complete production-ready package. However, if you are using this software, please cite reference [^1].

Prerequisites: 
1. [R 4.1.2+](https://cran.r-project.org/bin/windows/base/)
2. [RTools 4.0+](https://cran.r-project.org/bin/windows/Rtools/)

You can also try an extended solution from the [culearn](https://github.com/igormanojlovic/culearn) package, written entirely in Python.

## References

[^1]: Igor Manojlović, Goran Švenda, Aleksandar Erdeljan, Milan Gavrić: *Time series grouping algorithm for load pattern recognition*, Computers in Industry 111: 140-147 (2019), DOI: [10.1016/j.compind.2019.07.009](https://doi.org/10.1016/j.compind.2019.07.009)

[^2]: LCL dataset, https://data.london.gov.uk/dataset/smartmeter-energy-use-data-in-london-households
