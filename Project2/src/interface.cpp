#include <Eigen/Dense>

#include "loss.h"
#include <R.h>
#include <R_ext/Rdynload.h>
void loss_interface(
    const double *train_inputs_ptr, //n_observations x n_features
    const double *train_label_ptr,  //n_observations
    const int *max_iterations,   //integer
    const double *step_size            // double
    ){
  
  int status = squareLoss(train_inputs_ptr,train_label_ptr, *max_iterations, *step_size);
  if(status != 0){
    error("non-zero exit status from squareLoss");
  }
}

