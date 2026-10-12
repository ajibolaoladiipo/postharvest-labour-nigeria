# Reproducibility protocol

The controlled Stage 3 run began from eight authenticated raw `.dta` files, ran construction, an independent raw-source audit, household-design descriptive estimation, and Figure 1 generation. It passed 196 grouped source comparisons, 153 released descriptive rows, and 24 Figure 1 cells with no failures. Counts match exactly; floating comparisons use the tolerance documented in the benchmark results file. The runner uses `survey.lonely.psu = "adjust"`, stratified cluster designs, and household-level aggregation before ratio estimation.

Public rerun status: not yet demonstrated, because restricted source acquisition has not been tested by an external user. Do not represent the package as public data replication without that test.
