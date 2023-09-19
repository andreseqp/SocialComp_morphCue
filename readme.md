# The role of learning in the evolution of status signalling: a modeling approach

Code, data and manuscript accompanying the publications

Quiñones, A.E. Bshary. R. Cadena, C.D.
The role of learning in the evolution of status signalling: a modeling approach

[![DOI](https://zenodo.org/badge/440585701.svg)](https://zenodo.org/badge/latestdoi/440585701)

## Reproducibility 

### Compiling 
Code for the simulation model is written in C++ language. 
The files contained at the project folder: 
*random.cpp*, *utils.cpp*, *utils.h*, *random.h* and *json.hpp*, 
contain useful functions and random number generators used in the
stochastic simulations of the learning model. The source file for the 
sumlations (*cue_comp.cpp*) can be found also in the project folder.
Executables get JSON files as parameter inputs. JSON files are
integrated into c++ using the library contained in *json.hpp* (for more info see:
https://github.com/nlohmann/json). This library is written with c++11 
standards. With all these files, executables can be compile for the simulation 
with standard c++ compilers. For example for compilation with g++ use command:

_g++ cue_comp.cpp random.cpp utils.cpp -o $PROGRAMNAME -std=c++11_

## Generating parameter Values for the simulation
Files containing parameter values can be generated using the 
R file: *parTOjson.R*. Comments inside the file indicate how to set the different 
parameters of the model.  

### Running Individual-based simulations
The executable files compiled in section 1, get the parameter files as input. 
With the settings used as default, the executable for the runs
one replicate for each parameter files that gets as input. The simulation
produces two types of files:
1) The evolutionary dynamics named with the prefix *evolLearn* and appended with
  parameters used for the simulations.
2) Information from individuals sampled from the population at regular intervals 
  along evolutionary time. 
These files are place in the directory specified in the parameter file. 

### Visualization
Description and analysis of the results are embedded in R chunks in the 
Rmd file *manuscript_1.0.Rmd*. These R chunks source other R files contained
in the repository. Bellow a short description of the files necessary for the 
analysis and their role.

### Description of relevant files
*cue_comp.cpp*: c++ code of the individual-based model.
*aesth*: Defines a set of aesthetic parameters to be used in 
  the visualizations.
*AccFunc.r*: Accesory functions to visualize the outcome of the simulations. 
*NbClust.R*: Function to systemtize k-means
*posPlots.R*: fucntion to control figure size and location
*Filled.contour3.R*: Tailored implementation of filled contour
*json.hpp*: Header file necessary to use json files in the c++ code.
*parTOjson.R*: generate different types of json files with the parameter values.
*random.cpp*, *random.h*, *utils.cpp*, *utils.h*: Headers and cpp files 
  necessary to compile the executables. They define convenient functions and
  random number generators. 
*manuscript_1.0/*: files necessary to compile the R markdown file that 
  renders the manuscript. Among those *Cleanerlearning.bib* the reference list.
