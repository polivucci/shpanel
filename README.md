
<!-- [![Build Status](https://travis-ci.org/xcompact3d/shpanel.svg?branch=master)](https://travis-ci.org/xcompact3d/shpanel) -->
<!-- ![Build Status](https://github.com/xcompact3d/shpanel/workflows/Build/badge.svg) -->

## The shpanel code

shpanel is a Fortran based, Boundary Element Method code for solving the vorticity-free Euler equations around a two-dimensional wing section.

This is the GitHub repository of shpanel source code, including instructions for running and compiling shpanel.

### External Resources

The technical details of the Panel Method used by shpanel and validation against numerical and experimental data are illustrated in Chapter 3 of:

- [P. Olivucci (2016)](https://drive.google.com/file/d/1HxuUuOYdLmRaLohr6xwOQ2BDgNsrewLa/view?usp=sharing), A framework for the design by optimization of hydrofoils under cavitating conditions, Master Thesis, Universita' di Genova.

<!-- - [**Paper**](https://twitter.com/shpanel) -->

<!-- ### Benchmark test cases for code validation ### -->

<!-- Code validation against numerical and experimental data can be found in tha author's [Master Dissertation](https://www.shpanel.com/). -->

<!-- Benchmark datasets will be available soon. -->

<!-- The following case are set to match the parameters for cases of reference articles obtained with different codes.

|Code| Flow configuration             | BC File         | Reference | Dataset |
|:----------------:|:----------------:|:----------------:|:----------------:|:----------------:|
|1| Taylor-Green Vortices        | TGV              |[Beck et al. (2014)](https://link.springer.com/article/10.1007/s00162-011-0253-7) -->

## Source Download and Compilation

First, make sure you have all the required dependencies installed:

* Matlab or Octave.
* Gnuplot (optional).

Then, acquire the source code by cloning the git repository:

    git clone git@github.com:polivucci/shpanel.git

<!-- (If you are behind a firewall, you may need to use the `https` protocol instead of the `git` protocol:

    git config --global url."https://".insteadOf git@

Be sure to also configure your system to use the appropriate proxy settings, e.g. by setting the `https_proxy` and `http_proxy` variables.) -->

Now run `make` to build the `shpanel` executable. 
The command will automatically check the existence of `Makefile` and use it if it exists.

<!-- Once built, the `shpanel` executable can be run using its path in the directory created above (the `shpanel` directory). -->
Make sure the main script has execution permissions:

    chmod +x ./run_shpanel.sh

Now you should be able to run the code by launching the script:

    ./run_shpanel.sh

provided that the input file `shpanel_parameters.dat` is in the same directory.
If everything works correctly, you will see a shpanel banner and a summary of the input and the results printed on screen.
The results will be also stored in `shpanel_output.dat`.
Complete output data will be stored in the sub-directory `results/`.

### Example case

Coming soon.

## Platform-Specific Notes

### Linux

#### General

* Compilation has been tested with GCC version 4.7 or later.

#### Ubuntu

In order to compile and execute shpanel in the latest Ubuntu version please install the following packages:

    sudo apt install gfortran