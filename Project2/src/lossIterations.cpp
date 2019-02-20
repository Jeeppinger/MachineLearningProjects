#include <Eigen/Dense>

#include "loss.h"
#include <R.h>
#include <R_ext/Rdynload.h>
void loss_interface(
    const double *train_inputs_ptr, //n_observations x n_features
    const double *train_label_ptr,  //n_observations
    const double *max_iterations,   //integer
    const int *step_size            // double
    ){
  //int status = knn(train_inputs_ptr,train_label_ptr, test_input_ptr, *n_observations_ptr, *n_features_ptr, *max_neighbors_ptr, test_predictions_ptr);
  //if(status != 0){
  //  error("non-zero exit status from knn");
  }
//}