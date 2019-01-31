#include <Eigen/Dense>

int knn(
  const double *train_inputs_ptr, //n_observations x n_features (Matrix)
  const double *train_label_ptr,  //n_observations (vector)
  const double *test_input_ptr,   //n_features (vector)
  const int n_observations, const int n_features, const int max_neighbors,
  double *test_predictions_ptr    //max_neighbors (vector)
){
  Eigen::Map< Eigen::MatrixXd > train_inputs_mat((double*)train_inputs_ptr, n_observations,n_features);
  Eigen::Map< Eigen::vectorxd > test_input_vec((double*)test_input_ptr,n_features);
  Eigen::vectorxd dist_vec(n_observations);
  Eigen::vectorxi sorted_index_vec(n_observations);
  for (int i=0; i<n_observations;i++){
    dis_vec(i) = (train_inputs_mat.row(i).transpose() - test_input_vec).norm();
  sorted_index_vec(i) = i;
  }
  std::sort(
    sorted_index_vec.data(),
    sorted_index_vec.data()+sorted_index_vec.size(),
    [&dist_vec](int left, int right){
      return dis_vec(left) < dist_vec(right);
    }
  );
  double total = 0.0;
  for(int model_i=0; model_i<max_neighbors; model_i++){
    int neighbors = model_i +1;
    int row_i = sorted_index_vec(model_i);
    total+=train_label_ptr[row_i];
    test_predictions_ptr[model_i] = total/neighbors;
  }
  return 0;
}