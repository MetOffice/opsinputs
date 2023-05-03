[![CI](https://github.com/MetOffice/opsinputs/actions/workflows/ci.yml/badge.svg)](https://github.com/MetOffice/opsinputs/actions/workflows/ci.yml)

&copy; Crown Copyright 2020, the Met Office. All rights reserved.

Refer to [COPYRIGHT](COPYRIGHT.txt) of this distribution for details.

Overview
========

This software makes it possible for NG-OPS to write observation data to files in the VarObs format and model data (geovals) to files in the Cx format.

Files in both formats are read as inputs into the VAR data assimilation system at the Met Office. The VarObs file format is described in [OTDP 16](https://www-nwp/~opsrc/OPS/view/ops-latest/doc/OTDP16.html) and the Cx file format is described in [OTDP 17](https://www-nwp/~opsrc/OPS/view/ops-latest/doc/OTDP17.html).

Dependencies
============

1. The `ufo` JEDI project and its dependencies, e.g. `oops` and `ioda`.

2. Unified Model Shared Library (SHUMlib).

Note that both of the above need to be built with the same compiler and use the same libraries (e.g. MPI, HDF5, NetCDF).

Building
========

1. Clone the `mo-bundle` into a folder of your choice.

       git clone https://github.com/MetOffice/mo-bundle.git
       cd mo-bundle

2. Open the `CMakeLists.txt` file in the `mo-bundle` directory and comment out the projects not required for opsinputs (i.e., lines beginning with ecbuild_bundle).
    Note that `opsinputs` depends on `oops`, `ioda`, `ufo` and `ropp-ufo`.


3. Run the following commands to set up your build environment (internal):

       module use ~jopa/modulefiles
       module load bb-env

4. Configure and build:

       cmake --workflow --preset=vdi_gnu

5. Optionally, run tests to verify that components of the `opsinputs` package work correctly:

       ctest -R opsinputs

Usage
=====

VarObs and Cx files are written by the `VarObs Writer` and `Cx Writer` observation filters, respectively. See the Doxygen documentation of these filters in the `src/opsinputs/VarObsWriter.h` and `src/opsinputs/CxWriter.h` files (and the accompanying `...Parameters.h` files) for more information. 

The following YAML snippet demonstrates the use of `VarObsWriter`. <ObsGroup> stands for the name of one of the observation groups known to OPS.

```yaml
- ObsSpace:
  name: <ObsGroup>
  ObsDataIn:
    obsfile: observations.nc4
  simulate:
    variables: [surface_pressure]
- Filter: VarObs Writer
  # The filter will output an <ObsGroup>.varobs file
  # in the directory specified in the output_directory option.
  output_directory: varobs
  # By default, the filter will produce a VarObs file containing
  # the varfields output by default by OPS for the observation
  # group <ObsGroup>. The list of varfields can be changed by
  # providing a namelist file <ObsGroup>.nl in the directory
  # specified in the namelist_directory option. This file should
  # contain a Fortran namelist in the format accepted by the
  # Ops_ReadVarobsControlNL function from OPS.
  namelist_directory: namelists
  # Output only observations that passed the quality check in all variables.
  reject_obs_with_any_variable_failing_qc: true
  # Values of the following options are written to the UM fixed header
  # embedded in the output VarObs file. In future (once the UM/LFRic JEDI
  # interfaces are ready) they will probably be taken directly from
  # the model. There are more options like this; the full list can be
  # found in `src/opsinputs/VarObsWriterParameters.h`.
  IC_XLen: 36
  IC_YLen: 18
  IC_PLevels: 8
  IC_WetLevels: 9
  RC_LongSpacing: 10
  RC_LatSpacing: 10
```

And here is a YAML snippet demonstrating the use of `CxWriter`.

```yaml
- ObsSpace:
  name: <ObsGroup>
  ObsDataIn:
    obsfile: observations.nc4
  simulate:
    variables: [surface_pressure]
- Filter: Cx Writer
  # The filter will output an <ObsGroup>.cx file
  # in the directory specified in the output_directory option.
  output_directory: cx
  # By default, the filter will produce a Cx file containing
  # the cxfields output by default by OPS for the observation
  # group <ObsGroup>. The list of cxfields can be changed by
  # providing a namelist file <ObsGroup>.nl in the directory
  # specified in the namelist_directory option. This file should
  # contain a Fortran namelist in the format accepted by the
  # Ops_ReadCXControlNL function from OPS.
  namelist_directory: namelists
  # Output only model columns corresponding to observations that
  # passed the quality check in all variables.
  reject_obs_with_any_variable_failing_qc: true
  # Values of the following options are written to the UM fixed header
  # embedded in the output Cx file. In future (once the UM/LFRic JEDI
  # interfaces are ready) they will probably be taken directly from
  # the model. There are more options like this; the full list can be
  # found in `src/opsinputs/VarObsWriterParameters.h`.
  IC_XLen: 36
  IC_YLen: 18
  IC_PLevels: 3
  IC_WetLevels: 4
  RC_LongSpacing: 10
  RC_LatSpacing: 10
  # New dynamics vertical coordinate theta (length: IC_PLevels + 1).
  eta_theta_levels: [300, 290, 280, 270]
  # New dynamics vertical coordinate rho (length: IC_PLevels).
  eta_rho_levels: [3, 2, 1]
```

Further YAML files illustrating the use of these filters can be found in the `test/testinput` folder.

Development
===========

VarObs
------

Only a subset of varfields recognised by OPS and VAR can currently be output. To add support for a new varfield:

1. Determine where the input data will come from (a variable stored in the `ObsSpace` object? a GeoVaL?).

2. Edit the `case` corresponding to the varfield in question in the `select` statement in the `opsinputs_varobswriter_populateobservations` subroutine in the `src/opsinputs/opsinputs_varobswriter_mod.F90` file. In the vast majority of cases, you will simply need to replace a commented-out call to `Ops_Alloc` with a call to an appropriate `opsinputs_fill_fill...` subroutine. For example, suppose that the `VarField_logvis` varfield should be filled with the data (observed values, observation errors, gross error probabilities and QC flags) stored in the `logarithmic_visibility` observation variable in the `ObsSpace`. The comment

        ! call Ops_Alloc(Ob % Header % logvis, "logvis", Ob % Header % NumObsLocal, Ob % logvis)

   should then be replaced by

        call opsinputs_fill_fillelementtypefromsimulatedvariable( &
          Ob % Header % logvis, "logvis", Ob % Header % NumObsLocal, Ob % logvis, &
          ObsSpace, Flags, ObsErrors, "logarithmic_visibility")

   If in doubt, look at similar varfields that have already been implemented or read the documentation of relevant subroutines from the `opsinputs_fill_mod.F90` module.

3. Add a unit test for the new varfield:

   a. Create an input YAML file and put it in the `test/testinput` folder. Typically, you can copy an existing YAML file used by the test of a varfield whose implementation calls the same subroutine as that of the new varfield, and simply adjust the observation group, variable and varfield name embedded in the YAML file.

   b. Create an input NetCDF file and put it in the same folder. You can use the `test/generate_unittest_netcdfs.py` script to create the file (add an appropriate function call in the `VarObs` section at the end of the script, again mimicking one generating input data for a previously implemented varfield, and run the script). You may need to load the `satools-py3` module before running the script to give it access to NumPy and SciPy.

   c. Add a call to the `ADD_WRITER_TEST` function in the `test/CMakeLists.txt` file, specifying the name of the test and its input YAML and data files.

4. Update the list of implemented varfields in `Varfields.md`. 

Cx
--

Only a subset of cxfields recognised by OPS and VAR can currently be output. To add support for a new cxfield:

1. Determine the name of the GeoVal from which the cxfield will be retrieved and the name of the component of the `CX_type` type in OPS holding the value of that cxfield.

2. Set the appropriate `opsinputs_cxfields_*` constant in the `opsinputs_cxfields_mod.F90` file to the name of the GeoVal identified in the previous step.

3. Add a unit test for the new cxfield:

   a. Create an input NetCDF file and put it in the `test/testinput` folder. You can use the `test/generate_unittest_netcdfs.py` script to create the file (add a call to `output_1d_geoval_to_netcdf` or `output_2d_geoval_to_netcdf` in the `Cx` section at the end of the script, mimicking one generating input data for a previously implemented cxfield, and run the script). You may need to load the `satools-py3` module before running the script to give it access to NumPy and SciPy.
   
   b. Create an input namelist file that will be read by OPS to determine which cxfield needs to be written, and put it in an appropriate subfolder of `test/testinput`. Typically, you can:
      
      - Copy the existing `CxWriterNamelists_007_SurfaceCxField_modelsurface` or `CxWriterNamelists_001_UpperAirCxField_theta` subfolder (for surface, i.e. 1D, and upper-air, i.e. 2D, cxfields, respectively) to a new subfolder. For consistency, name that folder so that the embedded number is the index of the newly implemented cxfield (defined in the `OpsMod_CXIndexes.f90` file in OPS) and the suffix is the cxfield name. 
      - Adjust the name of the `*.nl` file in the subfolder; the part before the `.nl` extension must be the name of an observation group defined in OPS. It's best to pick a group to which the cxfield is relevant.
      - Edit the `CxFields=...` line in that file so that the number following `CxFields=` is the stash/ancillary code corresponding to the cxfield. To determine it, find the name of the appropriate `StashItem_*`, `StashCode_*` or `AncilCode_` constant in the `select case` statement in the `opsinputs_cxwriter_addrequiredgeovars` subroutine in `opsinputs_cxwriter_mod.F90` and look up the numeric value of that constant in the `OpsMod_Stash.f90` or `OpsMod_Ancil.f90` file in OPS.

   c. Create an input YAML file and put it in the `test/testinput` folder. Typically, you can

      - Copy the existing `007_SurfaceCxField_modelsurface.yaml` or `001_UpperAirCxField_theta.yaml` file and rename it, following the pattern described above for the folder containing a namelist file.
      - Edit the file, setting the ObsSpace name to the name of the OPS observation group, the GeoVaLs filename to the name of the file created in step a, and the expected variable index in the `expected_surface_variables` or `expected_upper_air_variables` section to the index of the newly defined cxfield.
   
   d. Add a call to the `ADD_WRITER_TEST` function in the `test/CMakeLists.txt` file, specifying the name of the test and its input YAML and data files.

Working practices
=================

The JEDI working principles are detailed at https://jointcenterforsatellitedataassimilation-jedi-docs.readthedocs-hosted.com/en/latest/working-practices/index.html.
