# ml4rg_project_02

In multicellular organisms, cells exhibit diverse appearances and functions despite containing identical DNA, due to differential gene expression regulated by non-coding DNA sequences. These sequences bind transcription factors that can either activate or repress nearby genes. The accessibility of these regulatory regions is governed by the displacement of histone proteins upon transcription factor binding, thereby exposing DNA for regulatory interactions. Experimental methods like ATAC-seq for measuring DNA accessibility are costly and  time-consuming. In this project, we leverage convolutional neural network model to accurately predict ATAC-seq signals directly from DNA sequences. The model not only predicts accessibility but also reveals transcription factor preferences and combinatorial binding rules, crucial for understanding cell-specific gene regulatory mechanisms.

The "model_interpretation" folder contains the scripts for preparing the data set, running modisco-lite and visualizing the detected motifs and transciption factors.

The "models_training" folder contains the jupyter notebooks for processing training data, training models using different architectures.

The "contribution_scores" folder contains the jupyter notebooks for computation and visualization of contribution scores.

Complete progress can be found in google drive:
https://drive.google.com/drive/u/0/folders/1MKXhbnPxQldePJRj_XJCCDfs1xDS96te
