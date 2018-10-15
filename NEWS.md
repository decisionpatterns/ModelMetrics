# ModelMetrics 1.2.0

* added s3 methods for `glm`, `lm`, `randomForest`, `merMod`, and `glmerMod`
* speed up `auc` with `data.table::frankv`
* added `gini`

# ModelMetrics 1.1.0

* added Matthews correlation coefficient (`mcc`)
* added multiclass auc (`mauc` )
* lots more tests
* fixed bug when rank ties were present in `auc` (#10)
* added code to handle different classes in functions


# ModelMetrics 1.0.0
* Initializing package with basic metric functions
