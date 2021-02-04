
## The shpanel code

shpanel is a Fortran based, Boundary Element Method code for solving the vorticity-free Euler equations around a two-dimensional wing section.

This is the GitHub repository of shpanel source code, including instructions for running and compiling shpanel.

### External Resources

The technical details of the Panel Method used by shpanel and validation against numerical and experimental data are illustrated in Chapter 3 of:

- [P. Olivucci (2016)](https://drive.google.com/file/d/1HxuUuOYdLmRaLohr6xwOQ2BDgNsrewLa/view?usp=sharing), A framework for the design by optimization of hydrofoils under cavitating conditions, Master Thesis, Universita' di Genova.

## Source Download and Compilation

First, make sure you have a copy of Matlab or Octave installed.

Then, acquire the source code by cloning the git repository:

    git clone git@github.com:polivucci/shpanel.git

Now run `make` to build the `shpanel` executable. 
The command will automatically check the existence of `Makefile` and use it if it exists.

Make sure the main script has execution permissions:

    chmod +x ./run_shpanel.sh


## Usage

### Input files

shpanel input consists of two text files: 

* `shpanel_parameters.dat` contains the main flow parameters,
* `geometry_input.dat` provides the wing section geometry.

The `example` directory contains two example input files.

### Running an example case

First, make sure to copy the two input files from the `example` directory to the main directory.
Now you can run the code by launching the script:

    ./run_shpanel.sh

If everything works correctly, you will see a shpanel banner and a summary of the input and the results printed to screen.

### Output files

The main result summary will be stored in `shpanel_output.dat`.
Complete output data will be stored in the sub-directory `output/`, including:

* `draw_chord.dat`, `draw_contrp.dat`, `draw_profile.dat`: wing section geometry,
* `draw_field_l.dat`: lower-surface pressure and velocity distributions,
* `draw_field_u.dat`: upper-surface pressure and velocity distributions.

## Platform-Specific Notes

### Linux

#### General

* Compilation has been tested with GCC version 4.7 or later.

#### Ubuntu

In order to compile and execute shpanel in the latest Ubuntu version please install the following packages:

    sudo apt install gfortran