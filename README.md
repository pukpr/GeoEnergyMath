# GeoEnergyMath
Software libraries for solving models described in Mathematical GeoEnergy (Wiley, 2018)

See the blog http://GeoEnergyMath.com and Azimuth Forum
https://forum.azimuthproject.org/discussion/comment/22266/#Comment_22266 for rationale. (Now the forum is deleted due to a crazy decision, so go to the Wayback Machine here https://web.archive.org/web/20200807074302/https://forum.azimuthproject.org/discussion/comment/22266/#Comment_22266).

Requires an Ada compiler such as the GNAT IDE -- download community edition from https://www.adacore.com/download/ and select the install for your operating system

Load the project file (such as LTE.gpr) into GNATStudio and select build from the menu. Or run *gprbuild lte.gpr enso_opt.adb* from the command line.
The compiled executable is located in an obj subdirectory.  The obj subdirectory may need to be created before compiling, run the executable from the root directory, i.e. 

> obj/enso_opt

More recently have created individual subbdirectories -- copy the executable in to a specific directory and run with e.g. *enso_opt*. This will automatically read the par file associated with that exec, *enso_opt.exe.par* and also the resp file  *enso_opt.exe.resp*  (parameter values and response configuration settings).  These are all preconfigured now to create optimal test cases to evaluate against. A commonly used command line parameter is *r*, which will calibrate the par values against LOD values.

I use DOS, Linux, or mingw to run the execs, but DOS has a responsive keyboard interrupt so I prefer to use DOS to run and keep a mingw shell to do other commands.  Pressing 'q' will stop and save the sim results, while pressing 'x' will exit the process without saving. Pressing '1' through '9' will stop at a correlation coefficent threshold, eg. 0.1 to 0.9.

See the local Wiki (https://github.com/pukpr/GeoEnergyMath/wiki/Laplace's-Tidal-Equation-modeling) for examples of drivers and typical results of modeling the various data sets.
