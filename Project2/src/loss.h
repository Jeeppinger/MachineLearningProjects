int squareLoss(
    const double *train_inputs_ptr, //n_observations x n_features (Matrix)
    const double *train_label_ptr,  //n_observations (vector)
    const int max_iterations_ptr,    //max iterations (double)
    const double step_size_ptr   //step size (double)
  
);
int logisticLoss(
    const double *train_inputs_ptr, //n_observations x n_features (Matrix)
    const double *train_label_ptr,  //n_observations (vector)
    const int max_iterations_ptr,    //max iterations (double)
    const double step_size_ptr   //step size (double)
  
);
