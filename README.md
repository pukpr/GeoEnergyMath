# GeoEnergyMath
Software libraries for solving models described in Mathematical GeoEnergy (Wiley, 2018)

See the blog http://GeoEnergyMath.com and Azimuth Forum
https://forum.azimuthproject.org/discussion/comment/22266/#Comment_22266 for rationale

Requires an Ada compiler such as the GNAT IDE -- download community edition from https://www.adacore.com/download/ and select the install for your operating system

Load the project file (such as LTE.gpr) into GNATStudio and select build from the menu. Or run *gprbuild lte.gpr enso_opt.adb* from command line.
The compiled executable is located in an obj subdirectory.  The obj subdirectory may need to be created before compiling, run the executable from the root directory, i.e. 

> obj/enso_opt

More recently have created individual subbdirectories -- copy the executable in to a specific directory and run with e.g. *enso_opt r*, where *r* means to read from the par file associated with that exec, *enso_opt.exe.par*.  These are all preconfigured now to create optimal test cases to evaluate against.

I use DOS, Linux, or mingw to run the execs, but DOS has a responsive keyboard interrupt so I prefer to use DOS to run and keep a mingw shell to do other commands.  Pressing 'q' will stop and save the sim results, while pressing 'x' will exit the process without saving.

See the local Wiki (https://github.com/pukpr/GeoEnergyMath/wiki/Laplace's-Tidal-Equation-modeling) for examples of drivers and typical results of modeling the various data sets.
