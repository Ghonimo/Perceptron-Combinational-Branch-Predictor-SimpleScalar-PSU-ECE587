# Perceptron-Branch-Predictor
An implementation and evaluation of a Perceptron-based Branch Prediction Algorithm in the SimpleScalar simulator for enhancing CPU performance.

## Table of Contents

- [Introduction](#introduction)
- [Installation](#installation)
- [Usage](#usage)
- [Files_Modified](#Files_Modified)
- [Contributing](#contributing)
- [References](#references)
- [License](#license)

## Introduction
This repository contains the SimpleScalar simulator with two additional branch predictors: "comb2lev" and "perceptron". These predictors are integrated into the existing SimpleScalar framework, extending its capabilities without altering the original functionality.


## Installation
1. Clone the repository:

2. Navigate to the directory:
cd ECE587-AdvancedBranchPrediction

3. Follow additional setup instructions as needed.

## Usage
To run the simulator with the new branch predictors, use the following commands:
For the Perceptron predictor:
./Run.pl -db ./bench.db -dir results/gcc1 -benchmark gcc -sim $HOME/your_directory/simulator/ss3/sim-outorder -args "-fastfwd 1000000 -max:inst 1000000 -bpred perceptron"
For the Combined Two-Level predictor (comb2lev), replace -bpred perceptron with -bpred comb2lev in the command.

## Files_Modified
The following files have been updated to include the new branch predictors:

1. bpred.c: Core logic for the branch predictors.
2. bpred.h: Header file with necessary definitions.
3. sim-outorder.c: Integration of new predictors with the out-of-order simulation model.
The original SimpleScalar implementation remains intact. The new predictors are added enhancements, and all existing functionality is preserved.

## Contributing
Contributions are welcome! Please read the [CONTRIBUTING](CONTRIBUTING.md) guidelines before submitting a pull request or opening an issue.

## License
This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.


