# Advanced Branch Prediction in SimpleScalar

## Overview
This repository contains our work for ECE 587: Advanced Computer Architecture at Portland State University, focusing on enhancing the branch prediction capabilities of the SimpleScalar simulator. We've implemented a Perceptron Branch Predictor and a Combinational Two-Level Adaptive Branch Predictor, demonstrating the application of sophisticated machine learning principles and adaptive strategies to improve prediction accuracy in processor simulation environments.

## Features
- **Perceptron Branch Predictor**: Utilizes a simple neural network model to dynamically predict branch behavior, considering longer branch histories and complex patterns for superior prediction accuracy.
- **Combinational Two-Level Adaptive Branch Predictor**: Integrates two adaptive predictors with a meta-predictor, dynamically selecting the most accurate prediction strategy based on historical performance.
- **Benchmark Analysis**: Detailed benchmarking using the `gcc` benchmarks to compare our advanced predictors against traditional methods, showcasing significant improvements in prediction accuracy.

## Getting Started
Clone this repository to review our implementation and experiments:
```bash
git clone https://github.com/Ghonimo/Perceptron-Combinational-Branch-Predictor-SimpleScalar-PSU-ECE587
```

Navigate through the directories to explore the implementation details, benchmark results, and documentation of our project.

## Project Structure
- src/: Source files modified in the SimpleScalar simulator to implement the branch predictors.
- docs/: Comprehensive documentation including the project report and presentation slides detailing our design rationale, implementation challenges, and benchmark analysis.
- testcases/: Test cases used for validating the functionality and accuracy of the implemented predictors.

## Benchmarks and Results
We conducted extensive benchmarking to evaluate the performance of our branch predictors. The perceptron predictor, notable for its use of machine learning principles, showed exceptional accuracy, especially in scenarios with complex branch patterns. The combinational predictor demonstrated versatility and high accuracy, benefiting from adaptive selection mechanisms.

For detailed results and analysis, refer to the docs/ directory.


## License
This project is licensed under the MIT License - see the LICENSE file for details.