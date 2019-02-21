#include <Eigen/Dense>

#include "loss.h"
#include <R.h>
#include <R_ext/Rdynload.h>
void squareloss_interface(
    const double *train_inputs_ptr, //n_observations x n_features
    const double *train_label_ptr,  //n_observations
    const int *max_iterations,      //integer
    const double *step_size         // double
){
  
  int status = squareLoss(train_inputs_ptr,train_label_ptr, *max_iterations, *step_size);
  if(status != 0){
    error("non-zero exit status from squareLoss");
  }
}

void logisticloss_interface(
    const double *train_inputs_ptr, //n_observations x n_features
    const double *train_label_ptr,  //n_observations
    const int *max_iterations,      //integer
    const double *step_size         // double
){
  
  int status = logisticLoss(train_inputs_ptr,train_label_ptr, *max_iterations, *step_size);
  if(status != 0){
    error("non-zero exit status from logisticLoss");
  }
}

