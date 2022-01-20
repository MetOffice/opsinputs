#!/usr/bin/env python
"""Generate NetCDF files required by VarObsWriter and CxWriter tests.

This script must currently be run manually whenever these files need to be regenerated. 
In principle we could define these files as outputs of a custom CMake target; 
this script would then be run automatically as part of the build process whenever it changed."""

import numpy as np
import netCDF4 as nc4

# Defined as in oops/util/missingValues.cc 
missing_float = np.finfo(np.float32).min * 0.99
missing_int = np.iinfo(np.int32).min + 5

# NetCDF missing values
missing_float_nc = 9.969209968386869e+36

def output_1d_simulated_var_to_netcdf(var_name, file_name):
    f = nc4.Dataset(file_name, 'w', format="NETCDF4")  

    nlocs = 4
    f.createDimension('nlocs', nlocs)
    nstring = 9
    f.createDimension('nstring', nstring)

    var = f.createVariable('latitude@MetaData', 'f', ('nlocs',))
    var[:] = [21, 22, -23, 24]
    var = f.createVariable('longitude@MetaData', 'f', ('nlocs',))
    var[:] = [31, 32, 33, 34]
    var = f.createVariable('air_pressure@MetaData', 'f', ('nlocs',))
    var[:] = [100100, 100200, 100300, 100400]
    var = f.createVariable('time@MetaData', 'f', ('nlocs',))
    minute = 1/60.
    var[:] = [1*minute, 2*minute, 3*minute, 4*minute]
    var = f.createVariable('station_id@MetaData', 'c', ('nlocs','nstring'))
    var[:] = ["station_1", "station_2", "station_3", "station_4"]
    
    var = f.createVariable(var_name + '@ObsValue', 'f', ('nlocs',))
    var[:] = [1.1, missing_float, 1.3, 1.4]
    var = f.createVariable(var_name + '@ObsError', 'f', ('nlocs',))
    var[:] = [0.1, missing_float, 0.3, 0.4]
    var = f.createVariable(var_name + '@GrossErrorProbability', 'f', ('nlocs',))
    var[:] = [0.4215156, missing_float, 0.1660898, 0.238132]
    var = f.createVariable(var_name + '@PreQC', 'i', ('nlocs',))
    var[:] = [1, 1, 1, 1]

    f.date_time = 2018010100

    f.close()

def output_1d_simulated_vars_to_netcdf(var_name_1, var_name_2, file_name):
    f = nc4.Dataset(file_name, 'w', format="NETCDF4")

    nlocs = 4
    f.createDimension('nlocs', nlocs)
    nstring = 9
    f.createDimension('nstring', nstring)

    var = f.createVariable('latitude@MetaData', 'f', ('nlocs',))
    var[:] = [21, 22, -23, 24]
    var = f.createVariable('longitude@MetaData', 'f', ('nlocs',))
    var[:] = [31, 32, 33, 34]
    var = f.createVariable('air_pressure@MetaData', 'f', ('nlocs',))
    var[:] = [100100, 100200, 100300, 100400]
    var = f.createVariable('time@MetaData', 'f', ('nlocs',))
    minute = 1/60.
    var[:] = [1*minute, 2*minute, 3*minute, 4*minute]
    var = f.createVariable('station_id@MetaData', 'c', ('nlocs','nstring'))
    var[:] = ["station_1", "station_2", "station_3", "station_4"]

    var = f.createVariable(var_name_1 + '@ObsValue', 'f', ('nlocs',))
    var[:] = [1.1, missing_float, 1.3, 1.4]
    var = f.createVariable(var_name_1 + '@ObsError', 'f', ('nlocs',))
    var[:] = [0.1, missing_float, 0.3, 0.4]
    var = f.createVariable(var_name_1 + '@GrossErrorProbability', 'f', ('nlocs',))
    var[:] = [0.01, missing_float, 0.03, 0.04]
    var = f.createVariable(var_name_1 + '@PreQC', 'i', ('nlocs',))
    var[:] = [1, 1, 1, 1]

    var = f.createVariable(var_name_2 + '@ObsValue', 'f', ('nlocs',))
    var[:] = [2.1, missing_float, 2.3, 2.4]
    var = f.createVariable(var_name_2 + '@ObsError', 'f', ('nlocs',))
    var[:] = [1.1, missing_float, 1.3, 1.4]
    var = f.createVariable(var_name_2 + '@GrossErrorProbability', 'f', ('nlocs',))
    var[:] = [0.11, missing_float, 0.13, 0.14]
    var = f.createVariable(var_name_2 + '@PreQC', 'i', ('nlocs',))
    var[:] = [1, 1, 1, 1]

    f.date_time = 2018010100

    f.close()

def output_1d_multi_level_simulated_var_to_netcdf(var_name, file_name):
    f = nc4.Dataset(file_name, 'w', format="NETCDF4")

    # These synthethic obs represent two sonde profiles,
    # one with four levels and one with two levels.
    # The extended ObsSpace will be generated automatically when the
    # test is run. However, the HofX values must be provided as if
    # the ObsSpace has already been extended.

    nlocs = 6
    f.createDimension('nlocs', nlocs)
    nlocs_extended = 12
    f.createDimension('nlocs_extended', nlocs_extended)
    nstring = 9
    f.createDimension('nstring', nstring)

    # Dimension variables
    var = f.createVariable('nlocs', 'f', ('nlocs'))
    var[:] = 0
    var.suggested_chunk_dim = 100
    var = f.createVariable('nlocs_extended', 'f', ('nlocs_extended'))
    var[:] = 0
    var.suggested_chunk_dim = 100
    var = f.createVariable('nstring', 'f', ('nstring'))
    var[:] = 0
    var.suggested_chunk_dim = 100

    # Values in the original ObsSpace.
    var = f.createVariable('MetaData/latitude', 'f', ('nlocs',))
    var[:] = [21, 22, -23, 24, -25, 26]
    var = f.createVariable('MetaData/longitude', 'f', ('nlocs',))
    var[:] = [31, 32, 33, 34, -35, -36]
    var = f.createVariable('MetaData/air_pressure', 'f', ('nlocs',))
    var[:] = [100100, 100200, 100300, 100400, 100500, 100600]
    var = f.createVariable('MetaData/time', 'f', ('nlocs',))
    minute = 1/60.
    var[:] = [1*minute, 2*minute, 3*minute, 4*minute, 5*minute, 6*minute]
    var = f.createVariable('MetaData/station_id', str, ('nlocs'))
    var[:] = np.array(['station_1', 'station_1', 'station_1', 'station_1', 'station_2', 'station_2'], dtype=object)
    var = f.createVariable('ObsValue/' + var_name, 'f', ('nlocs',))
    var[:] = [1.1, missing_float_nc, 1.3, 1.4, 1.5, 1.6]
    var = f.createVariable('ObsError/' + var_name, 'f', ('nlocs',))
    var[:] = [0.1, missing_float_nc, 0.3, 0.4, 0.5, 0.6]
    var = f.createVariable('GrossErrorProbability/' + var_name, 'f', ('nlocs',))
    var[:] = [0.01, missing_float_nc, 0.03, 0.04, 0.05, 0.06]
    var = f.createVariable('PreQC/' + var_name, 'i', ('nlocs',))
    var[:] = [1, 1, 1, 1, 1, 1]

    # Values in the extended obs space.
    var = f.createVariable('HofX/' + var_name, 'f', ('nlocs_extended',))
    # The first six values correspond to the original profiles,
    # and the second six correspond to the averaged profiles.
    var[:] = [1.2, missing_float_nc, 1.4, 1.5, 1.6, 1.7,
              1.25, 1.35, 1.45, 1.55, 1.65, 1.75]

    f.date_time = 2018010100
    f._ioda_layout = "ObsGroup"
    f._ioda_layout_version = 0

    f.close()

def output_1d_normal_var_to_netcdf(var_name, var_group, file_name):
    f = nc4.Dataset(file_name, 'w', format="NETCDF4")

    nlocs = 4
    f.createDimension('nlocs', nlocs)
    nstring = 9
    f.createDimension('nstring', nstring)

    var = f.createVariable('latitude@MetaData', 'f', ('nlocs',))
    var[:] = [21, 22, -23, 24]
    var = f.createVariable('longitude@MetaData', 'f', ('nlocs',))
    var[:] = [31, 32, 33, 34]
    var = f.createVariable('air_pressure@MetaData', 'f', ('nlocs',))
    var[:] = [100100, 100200, 100300, 100400]
    var = f.createVariable('time@MetaData', 'f', ('nlocs',))
    minute = 1/60.
    var[:] = [1*minute, 2*minute, 3*minute, 4*minute]
    var = f.createVariable('station_id@MetaData', 'c', ('nlocs','nstring'))
    var[:] = ["station_1", "station_2", "station_3", "station_4"]

    # There must be at least one simulated variable
    var = f.createVariable('dummy@ObsValue', 'f', ('nlocs',))
    var[:] = [1.1, missing_float, 1.3, 1.4]
    var = f.createVariable('dummy@ObsError', 'f', ('nlocs',))
    var[:] = [0.1, missing_float, 0.3, 0.4]
    var = f.createVariable('dummy@GrossErrorProbability', 'f', ('nlocs',))
    var[:] = [0.01, missing_float, 0.03, 0.04]
    var = f.createVariable('dummy@PreQC', 'i', ('nlocs',))
    var[:] = [1, 1, 1, 1]

    var = f.createVariable(var_name + '@' + var_group, 'f', ('nlocs',))
    var[:] = [5.1, missing_float, 5.3, 5.4]

    f.date_time = 2018010100

    f.close()

def output_1d_normal_int_var_to_netcdf(var_name, var_group, file_name):
    f = nc4.Dataset(file_name, 'w', format="NETCDF4")

    nlocs = 4
    f.createDimension('nlocs', nlocs)
    nstring = 9
    f.createDimension('nstring', nstring)

    var = f.createVariable('latitude@MetaData', 'f', ('nlocs',))
    var[:] = [21, 22, -23, 24]
    var = f.createVariable('longitude@MetaData', 'f', ('nlocs',))
    var[:] = [31, 32, 33, 34]
    var = f.createVariable('air_pressure@MetaData', 'f', ('nlocs',))
    var[:] = [100100, 100200, 100300, 100400]
    var = f.createVariable('time@MetaData', 'f', ('nlocs',))
    minute = 1/60.
    var[:] = [1*minute, 2*minute, 3*minute, 4*minute]
    var = f.createVariable('station_id@MetaData', 'c', ('nlocs','nstring'))
    var[:] = ["station_1", "station_2", "station_3", "station_4"]

    # There must be at least one simulated variable
    var = f.createVariable('dummy@ObsValue', 'f', ('nlocs',))
    var[:] = [1.1, missing_float, 1.3, 1.4]
    var = f.createVariable('dummy@ObsError', 'f', ('nlocs',))
    var[:] = [0.1, missing_float, 0.3, 0.4]
    var = f.createVariable('dummy@GrossErrorProbability', 'f', ('nlocs',))
    var[:] = [0.01, missing_float, 0.03, 0.04]
    var = f.createVariable('dummy@PreQC', 'i', ('nlocs',))
    var[:] = [1, 1, 1, 1]

    var = f.createVariable(var_name + '@' + var_group, 'i', ('nlocs',))
    var[:] = [5, missing_int, 7, 8]

    f.date_time = 2018010100

    f.close()

def output_1d_geoval_to_netcdf(var_name, file_name, with_channels=False):
    f = nc4.Dataset(file_name, 'w', format="NETCDF4")

    nlocs = 4
    f.createDimension('nlocs', nlocs)

    if (with_channels):
        name = var_name + '_1'
        var = f.createVariable(name, 'f', ('nlocs',))
        var[:] = [7.1, missing_float, 7.3, 7.4]

        name = var_name + '_2'
        var = f.createVariable(name, 'f', ('nlocs',))
        var[:] = [3.3, 4.1, 7.6, 6.2]

        name = var_name + '_3'
        var = f.createVariable(name, 'f', ('nlocs',))
        var[:] = [missing_float, 13.1, 6.5, 8.7]
    else:
        var = f.createVariable(var_name, 'f', ('nlocs',))
        var[:] = [7.1, missing_float, 7.3, 7.4]

    f.date_time = 2018010100

    f.close()

def output_2d_simulated_var_to_netcdf(var_name, file_name, with_bias=False, with_radar_family=False):
    f = nc4.Dataset(file_name, 'w', format="NETCDF4")

    nlocs = 4
    f.createDimension('nlocs', nlocs)
    nstring = 9
    f.createDimension('nstring', nstring)

    var = f.createVariable('latitude@MetaData', 'f', ('nlocs',))
    var[:] = [21, 22, -23, 24]
    var = f.createVariable('longitude@MetaData', 'f', ('nlocs',))
    var[:] = [31, 32, 33, 34]
    var = f.createVariable('air_pressure@MetaData', 'f', ('nlocs',))
    var[:] = [100100, 100200, 100300, 100400]
    var = f.createVariable('time@MetaData', 'f', ('nlocs',))
    minute = 1/60.
    var[:] = [1*minute, 2*minute, 3*minute, 4*minute]
    var = f.createVariable('station_id@MetaData', 'c', ('nlocs','nstring'))
    var[:] = ["station_1", "station_2", "station_3", "station_4"]
    if with_radar_family:
        var = f.createVariable('radar_family@MetaData', 'i', ('nlocs',))
        var[:] = [11, 12, 13, 14]

    var = f.createVariable(var_name + '_1@ObsValue', 'f', ('nlocs',))
    var[:] = [1.1, missing_float, 1.3, 1.4]
    var = f.createVariable(var_name + '_1@ObsError', 'f', ('nlocs',))
    var[:] = [0.1, missing_float, 0.3, 0.4]
    var = f.createVariable(var_name + '_1@GrossErrorProbability', 'f', ('nlocs',))
    var[:] = [0.01, missing_float, 0.03, 0.04]
    var = f.createVariable(var_name + '_1@PreQC', 'i', ('nlocs',))
    var[:] = [1, 1, 1, 1]
    if with_bias:
        var = f.createVariable(var_name + '_1@ObsBias', 'f', ('nlocs',))
        var[:] = [-0.1, -0.2, -0.3, -0.4]
        # BiasCorrObsValue = ObsValue - ObsBias
        var = f.createVariable(var_name + '_1@BiasCorrObsValue', 'f', ('nlocs',))
        var[:] = [1.2, missing_float, 1.6, 1.8]

    var = f.createVariable(var_name + '_2@ObsValue', 'f', ('nlocs',))
    var[:] = [2.1, 2.2, 2.3, 2.4]
    var = f.createVariable(var_name + '_2@ObsError', 'f', ('nlocs',))
    var[:] = [1.1, 1.2, 1.3, 1.4]
    var = f.createVariable(var_name + '_2@GrossErrorProbability', 'f', ('nlocs',))
    var[:] = [0.11, 0.12, 0.13, 0.14]
    var = f.createVariable(var_name + '_2@PreQC', 'i', ('nlocs',))
    var[:] = [1, 1, 1, 1]
    if with_bias:
        var = f.createVariable(var_name + '_2@ObsBias', 'f', ('nlocs',))
        var[:] = [-0.5, -0.6, -0.7, -0.8]
        # BiasCorrObsValue = ObsValue - ObsBias
        var = f.createVariable(var_name + '_2@BiasCorrObsValue', 'f', ('nlocs',))
        var[:] = [2.6, 2.8, 3.0, 3.2]

    var = f.createVariable(var_name + '_3@ObsValue', 'f', ('nlocs',))
    var[:] = [3.1, 3.2, 3.3, 3.4]
    var = f.createVariable(var_name + '_3@ObsError', 'f', ('nlocs',))
    var[:] = [2.1, 2.2, 2.3, 2.4]
    var = f.createVariable(var_name + '_3@GrossErrorProbability', 'f', ('nlocs',))
    var[:] = [0.21, 0.22, 0.23, 0.24]
    var = f.createVariable(var_name + '_3@PreQC', 'i', ('nlocs',))
    var[:] = [1, 1, 1, 1]
    if with_bias:
        var = f.createVariable(var_name + '_3@ObsBias', 'f', ('nlocs',))
        var[:] = [-0.01, -0.02, -0.03, -0.04]
        # BiasCorrObsValue = ObsValue - ObsBias
        var = f.createVariable(var_name + '_3@BiasCorrObsValue', 'f', ('nlocs',))
        var[:] = [3.11, 3.22, 3.33, 3.44]

    f.date_time = 2018010100

    f.close()

def output_2d_normal_var_to_netcdf(var_name, var_group, file_name, 
                                   with_radar_family=False, predictors=False):
    f = nc4.Dataset(file_name, 'w', format="NETCDF4")

    nlocs = 4
    f.createDimension('nlocs', nlocs)
    nstring = 9
    f.createDimension('nstring', nstring)

    var = f.createVariable('latitude@MetaData', 'f', ('nlocs',))
    var[:] = [21, 22, -23, 24]
    var = f.createVariable('longitude@MetaData', 'f', ('nlocs',))
    var[:] = [31, 32, 33, 34]
    var = f.createVariable('air_pressure@MetaData', 'f', ('nlocs',))
    var[:] = [100100, 100200, 100300, 100400]
    var = f.createVariable('time@MetaData', 'f', ('nlocs',))
    minute = 1/60.
    var[:] = [1*minute, 2*minute, 3*minute, 4*minute]
    var = f.createVariable('station_id@MetaData', 'c', ('nlocs','nstring'))
    var[:] = ["station_1", "station_2", "station_3", "station_4"]
    if with_radar_family:
        var = f.createVariable('radar_family@MetaData', 'i', ('nlocs',))
        var[:] = [11, 12, 13, 14]

    # There must be at least one simulated variable

    var = f.createVariable('dummy_1@ObsValue', 'f', ('nlocs',))
    var[:] = [1.1, missing_float, 1.3, 1.4]
    var = f.createVariable('dummy_1@ObsError', 'f', ('nlocs',))
    var[:] = [0.1, missing_float, 0.3, 0.4]
    var = f.createVariable('dummy_1@GrossErrorProbability', 'f', ('nlocs',))
    var[:] = [0.01, missing_float, 0.03, 0.04]
    var = f.createVariable('dummy_1@PreQC', 'i', ('nlocs',))
    var[:] = [1, 1, 1, 1]

    var = f.createVariable('dummy_2@ObsValue', 'f', ('nlocs',))
    var[:] = [2.1, 2.2, 2.3, 2.4]
    var = f.createVariable('dummy_2@ObsError', 'f', ('nlocs',))
    var[:] = [1.1, 1.2, 1.3, 1.4]
    var = f.createVariable('dummy_2@GrossErrorProbability', 'f', ('nlocs',))
    var[:] = [0.11, 0.12, 0.13, 0.14]
    var = f.createVariable('dummy_2@PreQC', 'i', ('nlocs',))
    var[:] = [1, 1, 1, 1]

    var = f.createVariable('dummy_3@ObsValue', 'f', ('nlocs',))
    var[:] = [3.1, 3.2, 3.3, 3.4]
    var = f.createVariable('dummy_3@ObsError', 'f', ('nlocs',))
    var[:] = [2.1, 2.2, 2.3, 2.4]
    var = f.createVariable('dummy_3@GrossErrorProbability', 'f', ('nlocs',))
    var[:] = [0.21, 0.22, 0.23, 0.24]
    var = f.createVariable('dummy_3@PreQC', 'i', ('nlocs',))
    var[:] = [1, 1, 1, 1]

    if type(var_group) != list:
        var_group = [var_group]
    for grp in var_group:
        var = f.createVariable(var_name + '_1@' + grp, 'f', ('nlocs',))
        var[:] = [4.1, missing_float, 4.3, 4.4]
        if "5Predictor" in grp:
            var[:] = [4.1, missing_float, 4.3, 0.0]
        if "8Predictor" in grp:
            var[:] = [0.0, 0.0, 0.0, 4.4]

        var2 = f.createVariable(var_name + '_2@' + grp, 'f', ('nlocs',))
        var2[:] = [5.1, 5.2, 5.3, 5.4]
        if "5Predictor" in grp:
            var2[:] = [5.1, 5.2, 5.3, 0.0]
        if "8Predictor" in grp:
            var2[:] = [0.0, 0.0, 0.0, 5.4]

        var3 = f.createVariable(var_name + '_3@' + grp, 'f', ('nlocs',))
        var3[:] = [6.1, 6.2, 6.3, 6.4]
        if "5Predictor" in grp:
            var3[:] = [6.1, 6.2, 6.3, 0.0]
        if "8Predictor" in grp:
            var3[:] = [0.0, 0.0, 0.0, 6.4]


    if predictors:
        var = f.createVariable('satellite_id@MetaData', 'i', ('nlocs',))
        var[:] = [5, 5, 5, 8]

    f.date_time = 2018010100

    f.close()

def output_simulated_var_profiles_to_netcdf(var_name, file_name):
    f = nc4.Dataset(file_name, 'w', format="NETCDF4")

    nlocs = 14
    f.createDimension('nlocs', nlocs)

    # Observations taken by four stations. They are in "random" order (not sorted either by stations
    # or by datetimes); this makes it possible to verify that the record ordering specified in the
    # YAML file is respected by the filters writing VarObs and Cx files.

    var = f.createVariable('latitude@MetaData', 'f', ('nlocs',))
    var[:] = [-11, -23, -12, -22, -13, -21, -14, -31, -42, -32, -41, -33, -51, -52]
    var = f.createVariable('longitude@MetaData', 'f', ('nlocs',))
    var[:] = [11, 23, 12, 22, 13, 21, 14, 31, 42, 32, 41, 33, 51, 52]
    var = f.createVariable('air_pressure@MetaData', 'f', ('nlocs',))
    var[:] = [100100, 80200, 90100, 90200, 80100, 100200, 70100,
              100300, 90400, 90200, 100400, 80300, 100500, 90500]
    var = f.createVariable('datetime@MetaData', str, ('nlocs'))
    # The NetCDF4 module doesn't support assigning values to variable-length string variables
    # using the `var[:] = ...` syntax, so we do it using a loop
    for i, s in enumerate(["2018-01-01T00:01:01Z", "2018-01-01T00:02:03Z",
                           "2018-01-01T00:01:02Z", "2018-01-01T00:02:02Z", "2018-01-01T00:01:03Z",
                           "2018-01-01T00:02:01Z", "2018-01-01T00:01:04Z", "2018-01-01T00:03:01Z",
                           "2018-01-01T00:04:02Z", "2018-01-01T00:03:02Z", "2018-01-01T00:04:01Z",
                           "2018-01-01T00:03:03Z", "2018-01-01T00:05:01Z", "2018-01-01T00:05:02Z"]):
        var[i] = s
    var = f.createVariable('station_id@MetaData', str, ('nlocs'))
    for i, s in enumerate(["station_1", "station_2", "station_1", "station_2",
                           "station_1", "station_2", "station_1",
                           "station_3", "station_4", "station_3", "station_4",
                           "station_3", "station_5", "station_5"]):
        var[i] = s
    var = f.createVariable(var_name + '@ObsValue', 'f', ('nlocs',))
    var[:] = [0.11, 0.23, 0.12, 0.22, 0.13, 0.21, 0.14,
              0.31, 0.42, 0.32, missing_float, 0.33, 0.51, 0.52]
    var = f.createVariable(var_name + '@ObsError', 'f', ('nlocs',))
    var[:] = [0.011, 0.023, 0.012, 0.022, 0.013, missing_float, 0.014,
             0.031, 0.042, 0.032, 0.041, 0.033, 0.051, 0.052]
    var = f.createVariable(var_name + '@GrossErrorProbability', 'f', ('nlocs',))
    var[:] = [0.111, 0.123, 0.112, 0.122, 0.113, 0.121, 0.114,
              0.131, 0.142, 0.132, 0.141, 0.133, 0.151, 0.152]
    var = f.createVariable(var_name + '@PreQC', 'i', ('nlocs',))
    var[:] = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]

    f.close()

def output_2d_geoval_to_netcdf(var_name, file_name):
    return output_2d_geovals_to_netcdf([var_name], file_name)

def output_2d_geovals_to_netcdf(var_names, file_name):
    f = nc4.Dataset(file_name, 'w', format="NETCDF4")

    nlocs = 4
    nlevs = 3
    f.createDimension('nlocs', nlocs)
    f.createDimension('nlevs', nlevs)

    for var_index, var_name in enumerate(var_names):
      var = f.createVariable(var_name, 'f', ('nlocs','nlevs'))
      shift = 10 * var_index
      # Reversed now geovals are toptobottom
      var[:] = [[shift + 1.3, shift + 1.2, shift + 1.1],
                [shift + 2.3, missing_float, shift + 2.1],
                [shift + 3.3, shift + 3.2, shift + 3.1],
                [shift + 4.3, shift + 4.2, shift + 4.1]]

    f.date_time = 2018010100

    f.close()

def output_2d_geoval_for_multi_level_obs_to_netcdf(var_name, file_name):
    f = nc4.Dataset(file_name, 'w', format="NETCDF4")

    nlocs = 12
    nlevs = 3
    f.createDimension('nlocs', nlocs)
    f.createDimension('nlevs', nlevs)
    var = f.createVariable(var_name, 'f', ('nlocs','nlevs'))
    # These synthethic GeoVaLs represent two sonde profiles,
    # one with four levels and one with two levels.
    # Each profile has been averaged onto three model levels.
    var[:] = [[1.1, 1.2, 1.3],
              [2.1, missing_float, 2.3],
              [3.1, 3.2, 3.3],
              [4.1, 4.2, 4.3],
              [5.1, 5.2, 5.3],
              [6.1, 6.2, 6.3],
              [1.1, 1.2, 1.3],
              [1.1, 1.2, 1.3],
              [1.1, 1.2, 1.3],
              [5.1, 5.2, 5.3],
              [5.1, 5.2, 5.3],
              [5.1, 5.2, 5.3]]

    f.date_time = 2018010100

    f.close()

def output_full_cx_to_netcdf(oned_var_names, twod_var_names, file_name):
    f = nc4.Dataset(file_name, 'w', format="NETCDF4")

    nlocs = 4
    nlevs = 3
    f.createDimension('nlocs', nlocs)
    f.createDimension('nlevs', nlevs)

    for var_index, var_name in enumerate(twod_var_names):
      var = f.createVariable(var_name, 'f', ('nlocs','nlevs'))
      shift = 10 * var_index
      # Reversed now geovals are top to bottom
      var[:] = [[shift + 1.3, shift + 1.2, shift + 1.1],
                [shift + 2.3, missing_float, shift + 2.1],
                [shift + 3.3, shift + 3.2, shift + 3.1],
                [shift + 4.3, shift + 4.2, shift + 4.1]]

    for var_index, var_name in enumerate(oned_var_names):
      var = f.createVariable(var_name, 'f', ('nlocs',))
      shift = 10 * var_index
      var[:] = [shift + 7.1, missing_float, shift + 7.3, shift + 7.4]

    f.date_time = 2018010100

    f.close()

def output_full_varobs_to_netcdf(oned_float_varnames, twod_float_varnames, oned_int_varnames, file_name):
    f = nc4.Dataset(file_name, 'w', format="NETCDF4")

    nlocs = 4
    f.createDimension('nlocs', nlocs)
    nchans = 3
    f.createDimension('nchans', nchans)
    nstring = 9
    f.createDimension('nstring', nstring)

    var = f.createVariable('nchans', 'i', ('nchans',))
    var[:] = [1,2,3]

    for var_index, var_name in enumerate(oned_float_varnames):
      var = f.createVariable(var_name, 'f', ('nlocs',))
      shift = 10 * var_index
      var[:] = [shift + 7.1, missing_float, shift + 7.3, shift + 7.4]

    for var_index, var_name in enumerate(twod_float_varnames):
      var = f.createVariable(var_name, 'f', ('nlocs','nchans'))
      shift = 10 * var_index
      var[:] = [[shift + 1.1, shift + 1.2, shift + 1.3],
                [shift + 2.1, missing_float, shift + 2.3],
                [shift + 3.1, shift + 3.2, shift + 3.3],
                [shift + 4.1, shift + 4.2, shift + 4.3]]

    for var_index, var_name in enumerate(oned_int_varnames):
      var = f.createVariable(var_name, 'i', ('nlocs',))
      shift = 10 * var_index
      var[:] = [shift + 3, missing_int, shift + 7, shift + 9]

    var = f.createVariable('MetaData/datetime', str, ('nlocs'))
    for i, s in enumerate(["2018-01-01T00:01:01Z", "2018-01-01T00:02:03Z",
                           "2018-01-01T00:01:02Z", "2018-01-01T00:02:02Z"]):
        var[i] = s

    f.date_time = 2018010100

    f.close()


if __name__ == "__main__":
    # VarObs
    output_1d_simulated_var_to_netcdf('surface_pressure',            'testinput/001_VarField_pstar.nc4') # Surface
    output_1d_simulated_var_to_netcdf('air_temperature',             'testinput/002_VarField_temperature_Surface.nc4')
    output_2d_simulated_var_to_netcdf('air_temperature',             'testinput/002_VarField_temperature_RadarZ.nc4')
    output_1d_simulated_var_to_netcdf('relative_humidity',           'testinput/003_VarField_rh_Surface.nc4')
    output_simulated_var_profiles_to_netcdf('relative_humidity',           'testinput/003_VarField_rh_Sonde.nc4')
    output_1d_simulated_var_to_netcdf('eastward_wind',               'testinput/004_VarField_u_Surface.nc4')
    output_simulated_var_profiles_to_netcdf('eastward_wind',               'testinput/004_VarField_u_Sonde.nc4')
    output_1d_simulated_var_to_netcdf('northward_wind',              'testinput/005_VarField_v_Surface.nc4')
    output_simulated_var_profiles_to_netcdf('northward_wind',              'testinput/005_VarField_v_Sonde.nc4')
    output_2d_simulated_var_to_netcdf('brightness_temperature',      'testinput/010_VarField_britemp.nc4', with_bias=True)
    output_1d_normal_var_to_netcdf   ('skin_temperature', 'OneDVar', 'testinput/011_VarField_tskin.nc4')
    output_2d_normal_var_to_netcdf   ('surface_emissivity', 'Emiss', 'testinput/017_VarField_mwemiss.nc4')
    output_1d_normal_var_to_netcdf   ('sensor_zenith_angle', 'MetaData', 'testinput/019_VarField_satzenith.nc4')
    output_1d_normal_int_var_to_netcdf('surface_type', 'MetaData', 'testinput/021_VarField_surface.nc4')
    output_1d_geoval_to_netcdf       ('land_type_index',            'testinput/023_VarField_modelsurface_geoval.nc4')
    output_1d_normal_int_var_to_netcdf('satellite_id', 'MetaData',     'testinput/028_VarField_satid.nc4')
    output_1d_normal_var_to_netcdf   ('solar_zenith_angle', 'MetaData', 'testinput/031_VarField_solzenith.nc4')
    # 54 VarField_NumChans and 55 VarField_ChanNum: separate files not necessary
    output_2d_normal_var_to_netcdf   ('radar_azimuth', 'MetaData',  'testinput/066_VarField_radarobazim.nc4', with_radar_family=True)
    output_2d_normal_var_to_netcdf   ('brightness_temperature', ['constant_satid_5Predictor',            'constant_satid_8Predictor',
                                                                 'thickness_850_300hPa_satid_5Predictor','thickness_850_300hPa_satid_8Predictor',
                                                                 'thickness_200_50hPa_satid_5Predictor', 'thickness_200_50hPa_satid_8Predictor',
                                                                 'Legendre_order_1_satid_5Predictor',    'Legendre_order_1_satid_8Predictor'],
                                      'testinput/080_VarField_biaspredictors.nc4', predictors=True)
    output_2d_simulated_var_to_netcdf('bending_angle',              'testinput/071_VarField_bendingangle.nc4')
    output_2d_normal_var_to_netcdf('impact_parameter', 'MetaData', 'testinput/072_VarField_impactparam.nc4')
    output_1d_normal_var_to_netcdf('earth_radius_of_curvature', 'MetaData',  'testinput/073_VarField_ro_rad_curv.nc4')
    output_1d_normal_var_to_netcdf('geoid_height_above_reference_ellipsoid', 'MetaData', 'testinput/074_VarField_ro_geoid_und.nc4')
    output_2d_simulated_var_to_netcdf('aerosol_optical_depth', 'testinput/077_VarField_aod.nc4')
    output_simulated_var_profiles_to_netcdf('theta', 'testinput/078_VarField_theta.nc4') # Sonde
    output_1d_simulated_vars_to_netcdf('eastward_wind', 'northward_wind',
                                       'testinput/reject_obs_with_all_variables_failing_qc.nc4')

    # Varobs full output for an obsgroup testing
    # 1dfloats, 2dfloats, 1dints, filename
    
    # ATMS
    output_full_varobs_to_netcdf(['MetaData/latitude','MetaData/longitude','OneDVar/skin_temperature','MetaData/sensor_zenith_angle',
                                  'MetaData/solar_zenith_angle'],
                                 ['ObsValue/brightness_temperature','ObsError/brightness_temperature','Emiss/surface_emissivity',
                                  'BiasCorrObsValue/brightness_temperature','thickness_850_300hPa_satid_13Predictor/brightness_temperature',
                                  'thickness_850_300hPa_satid_17Predictor/brightness_temperature'],
                                 ['MetaData/surface_type','MetaData/satellite_id'],
                                  'testinput/varobs_globalnamelist_atms.nc4')
    # GNSS-RO
    output_full_varobs_to_netcdf(['MetaData/latitude',
                                  'MetaData/longitude',
                                  'MetaData/impact_parameter',
                                  'MetaData/earth_radius_of_curvature',
                                  'MetaData/geoid_height_above_reference_ellipsoid',
                                  'ObsValue/bending_angle',
                                  'ObsError/bending_angle'],
                                 [],
                                 ['MetaData/satellite_id'],
                                  'testinput/varobs_globalnamelist_gnssro.nc4')

    # Cx
    output_1d_simulated_var_to_netcdf('dummy',                      'testinput/dummy.nc4')
    output_1d_geoval_to_netcdf       ('surface_altitude',           'testinput/001_SurfaceCxField_Orog.nc4')
    output_1d_geoval_to_netcdf       ('surface_pressure',           'testinput/002_SurfaceCxField_pstar.nc4')
    output_1d_geoval_to_netcdf       ('surface_temperature',        'testinput/003_SurfaceCxField_t2.nc4')
    output_1d_geoval_to_netcdf       ('relative_humidity_2m',       'testinput/004_SurfaceCxField_rh2.nc4')
    output_1d_geoval_to_netcdf       ('uwind_at_10m',               'testinput/005_SurfaceCxField_u10.nc4')
    output_1d_geoval_to_netcdf       ('vwind_at_10m',               'testinput/006_SurfaceCxField_v10.nc4')
    output_1d_geoval_to_netcdf       ('skin_temperature',           'testinput/013_SurfaceCxField_TskinSea.nc4')
    output_1d_geoval_to_netcdf       ('surface_pressure_at_mean_sea_level', 'testinput/016_SurfaceCxField_pmsl.nc4')
    output_1d_geoval_to_netcdf       ('ice_area_fraction',          'testinput/017_SurfaceCxField_SeaIce.nc4')
    output_2d_geoval_to_netcdf       ('theta',                      'testinput/001_UpperAirCxField_theta.nc4')
    output_2d_geoval_to_netcdf       ('relative_humidity',          'testinput/002_UpperAirCxField_relative_humidity.nc4')
    output_2d_geoval_to_netcdf       ('eastward_wind',              'testinput/003_UpperAirCxField_u.nc4')
    output_2d_geoval_to_netcdf       ('northward_wind',             'testinput/004_UpperAirCxField_v.nc4')
    output_2d_geoval_to_netcdf       ('specific_humidity',          'testinput/005_UpperAirCxField_q.nc4')
    output_2d_geoval_to_netcdf       ('air_pressure',               'testinput/033_UpperAirCxField_p_bar.nc4')
    output_2d_geoval_to_netcdf       ('air_pressure_levels',        'testinput/011_UpperAirCxField_P.nc4')
    output_2d_geoval_to_netcdf       ('mass_content_of_cloud_ice_in_atmosphere_layer', 'testinput/029_UpperAirCxField_qcf.nc4')
    output_2d_geoval_to_netcdf       ('mass_content_of_cloud_liquid_water_in_atmosphere_layer', 'testinput/030_UpperAirCxField_qcl.nc4')
    output_2d_geoval_to_netcdf       ('cloud_area_fraction_in_atmosphere_layer', 'testinput/031_UpperAirCxField_cloud_bulk.nc4')
    output_2d_geoval_to_netcdf       ('frozen_cloud_fraction',      'testinput/034_UpperAirCxField_Cf.nc4')
    output_2d_geoval_to_netcdf       ('liquid_cloud_fraction',      'testinput/035_UpperAirCxField_Cl.nc4')
    output_2d_geovals_to_netcdf      (['dust%s' % i for i in range(1, 7)], 'testinput/041-046_UpperAirCxField_dust1-dust6.nc4')

    # Cx full output for an obsgroup testing
    # list of 1d-variables; list of 2d-variables; filename for output
    
    # ATMS
    output_full_cx_to_netcdf(['skin_temperature','ice_area_fraction','surface_altitude','surface_pressure','uwind_at_10m',
                              'vwind_at_10m','surface_temperature','relative_humidity_2m','surface_pressure_at_mean_sea_level'],
                             ['theta','specific_humidity','mass_content_of_cloud_ice_in_atmosphere_layer',
                              'mass_content_of_cloud_liquid_water_in_atmosphere_layer','air_pressure_levels',
                              'cloud_area_fraction_in_atmosphere_layer','liquid_cloud_fraction','frozen_cloud_fraction'],
                              'testinput/cx_globalnamelist_atms.nc4')

    # GNSS-RO
    output_full_cx_to_netcdf(['skin_temperature','ice_area_fraction','surface_altitude','surface_pressure','uwind_at_10m',
                              'vwind_at_10m','surface_temperature','relative_humidity_2m','surface_pressure_at_mean_sea_level'],
                             ['theta','specific_humidity','mass_content_of_cloud_ice_in_atmosphere_layer',
                              'mass_content_of_cloud_liquid_water_in_atmosphere_layer','air_pressure_levels',
                              'cloud_area_fraction_in_atmosphere_layer','liquid_cloud_fraction','frozen_cloud_fraction'],
                              'testinput/cx_globalnamelist_gnssro.nc4')

    output_1d_multi_level_simulated_var_to_netcdf('relative_humidity', 'testinput/relative_humidity_Sonde.nc4')
    output_2d_geoval_for_multi_level_obs_to_netcdf('relative_humidity', 'testinput/002_UpperAirCxFieldForMultiLevelObs_relative_humidity.nc4')
