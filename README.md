# PEABRAIN
Extremely naive recursive implementation of feed forward (pb/compute-guess) and back-propagation (pb/back-prop) in emacs lisp.

Why would anyone do this? It's not clear. Will it probably break your max-specpdl-size and max-lisp-eval-depth? Yeah, I bet.

Credit for clear instruction on building these algorithms from scratch goes to Florian Courtial with this excellent tutorial: https://matrices.io/deep-neural-network-from-scratch/ . The `data.csv` file was also cribbed from `normalized_car_features.csv` from Florian's github here: https://github.com/theflofly/dnn_from_scratch_py/blob/master/normalized_car_features.csv . The math isn't bad but lining up matrix dimensions to do the math -- that's no fun. Agh.

An example using Peabrain is found in `learner.el`. This will read `data.csv` into a temporary buffer, construct a calc matrix from it (already whole seconds of work) and then run 1000 cycles training a neural network(10s of seconds of work) to estimate the last column given the first three (estimate car price based on some features). It prints out the Mean Absolute Error (output vs. training data) every 100 steps, just to indicate that it's "learning," like that's something that I expect it to do. 

Oh yeah, you should probably know, right now I'm using just 44 cars from the set of ~8000 because emacs chokes otherwise and I need to type words into the editor for MY JOB so I just can't have that happening intermittently.

I also skipped regularization because at this point I'm not concerned with practical considerations like "overfitting". Whatever that is.

Someone please tell me if any of the algorithms are straight up wrong because I absolutely cannot be bothered to validate my 44 car model.
