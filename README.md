# PEABRAIN
Extremely naive recursive implementation of feed forward (pb/compute-guess) and back-propagation (pb/back-prop) in emacs lisp.

Why would anyone do this? It's not clear. Will it probably break your max-specpdl-size and max-lisp-eval-depth? Yeah, I bet.

Credit for clear instruction on building these algorithms from scratch goes to Florian Courtial with this excellent tutorial: https://matrices.io/deep-neural-network-from-scratch/ . The `data.csv` file was also cribbed from `normalized_car_features.csv` from Florian's github here: https://github.com/theflofly/dnn_from_scratch_py/blob/master/normalized_car_features.csv . The math isn't bad but lining up matrix dimensions to do the math -- that's no fun. Agh.

An example using Peabrain is found in `learner.el`. This will read `data.csv` into a temporary buffer, construct a calc matrix from it (already whole seconds of work) and then run 1000 cycles training a neural network (10s of seconds of work) to estimate the last column given the first three (estimate car price based on some features). It prints out the Mean Absolute Error (output vs. training data) every 100 steps, just to indicate that it's "learning," like that's something that I expect it to do. 

Oh yeah, you should probably know, right now I'm using just 44 cars from the set of ~8000 because emacs chokes otherwise and I need to type words into the editor for MY JOB so I just can't have that happening intermittently.

I also skipped regularization because at this point I'm not concerned with practical considerations like "overfitting". Whatever that is.

Someone please tell me if any of the algorithms are straight up wrong because I absolutely cannot be bothered to validate my 44 car model.

## Useage Notes

Peabrain operates directly on matrices of weights. The "neurons" are virtual and fall out of the activation math. pb/compute-guess and pb/back-prop both take a "layers" argument that is a list of calc matrices (... W3 W2 W1 INPUTS), with the leftmost matrices in the list being the closest to the output layer (this is maybe backwards from the way most people would think about it). In practice, the matrices will have a number of rows equal to the number of columns in the last matrix (+1 row for the biases), and a number of columns equal to the number of "neurons" in the current layer.

```emacs
(defvar learn-layers)
(setq learn-layers (list (pb/bottom-points (pb/random-matrix 2 1))
                         (pb/bottom-points (pb/random-matrix 3 2))
                         (pb/bottom-points (pb/random-matrix 3 3))))
```

In `learner.el` W1 is 4x3 since their are 3 input columns, three neurons in the first layer, and a row of biases (pb/bottom-points) at the bottom. W2 is 4x2 since there are 3 neurons (columns) in the last layer, two neurons in the second layer, and a row of biases at the bottom. W3 is 3x1 since there are 2 neurons in the previous layer, a single neuron in this layer (the output), and a row of (a single) bias at the bottom.

```emacs
(pb/compute-guess (append learn-layers (list training-matrix)) 'pb/tanh-mat)
```

Using pb/compute-guess is as easy as appending the training data to the end of learn-layers and passing it an element-wise activation function.

