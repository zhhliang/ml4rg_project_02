# ml4rg_project_02

In multicellular organisms, cells exhibit diverse appearances and functions despite containing identical DNA, due to differential gene expression regulated by non-coding DNA sequences. These sequences bind transcription factors that can either activate or repress nearby genes. The accessibility of these regulatory regions is governed by the displacement of histone proteins upon transcription factor binding, thereby exposing DNA for regulatory interactions. Experimental methods like ATAC-seq for measuring DNA accessibility are costly and  time-consuming. In this project, we leverage convolutional neural network model to accurately predict ATAC-seq signals directly from DNA sequences. The model not only predicts accessibility but also reveals transcription factor preferences and combinatorial binding rules, crucial for understanding cell-specific gene regulatory mechanisms.

The "model_interpretation" folder contains the scripts for preparing the data set, running modisco-lite and visualizing the detected motifs and transciption factors.
