# ml4rg_project_02

Totally 18 different architectures, including baseline DeepSTARR and one best architecture.

You can find results of cross validation for the baseline DeepSTARR, model_3 and the best architecture.

Based on the cross validation results, the best architecture is chosen for training 4 models for 4 different cell types, you can also find the performance for test set compared with the baseline.

Because the computaion of SHAP values requires a different code style (https://github.com/shap/shap/issues/2511), I rewrote the best architecture (same architecture, different code style) and trained again 4 models for 4 different cell types.

You can find jupyter notebooks, best checkpoints and train_val_curves for all the trainings.
