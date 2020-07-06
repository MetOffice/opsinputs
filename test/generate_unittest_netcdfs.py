#!/usr/bin/env python
"""Generate NetCDF files required by VarObsWriter tests.

This script must currently be run manually whenever these files need to be regenerated. 
In principle we could define these files as outputs of a custom CMake target; 
this script would then be run automatically as part of the build process whenever it changed."""

import numpy as np
from scipy.io import netcdf

# Defined as in oops/util/missingValues.cc 
missing_float = np.finfo(np.float32).min * 0.99
missing_int = np.iinfo(np.int32).min + 5

def output_1d_simulated_var_to_netcdf(var_name, file_name):
    f = netcdf.netcdf_file(file_name, 'w')

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
    var[:] = [0.01, missing_float, 0.03, 0.04]
    var = f.createVariable(var_name + '@PreQC', 'i', ('nlocs',))
    var[:] = [1, 1, 1, 1]

    f.date_time = 2018010100

    f.close()

def output_1d_simulated_vars_to_netcdf(var_name_1, var_name_2, file_name):
    f = netcdf.netcdf_file(file_name, 'w')

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

def output_1d_normal_var_to_netcdf(var_name, var_group, file_name):
    f = netcdf.netcdf_file(file_name, 'w')

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
    f = netcdf.netcdf_file(file_name, 'w')

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

def output_1d_geoval_to_netcdf(var_name, file_name):
    f = netcdf.netcdf_file(file_name, 'w')

    nlocs = 4
    f.createDimension('nlocs', nlocs)

    var = f.createVariable(var_name, 'f', ('nlocs',))
    var[:] = [7.1, missing_float, 7.3, 7.4]

    f.date_time = 2018010100

    f.close()

def output_2d_simulated_var_to_netcdf(var_name, file_name, with_bias=False, with_radar_family=False):
    f = netcdf.netcdf_file(file_name, 'w')

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

    f.date_time = 2018010100

    f.close()

def output_2d_normal_var_to_netcdf(var_name, var_group, file_name, with_radar_family=False):
    f = netcdf.netcdf_file(file_name, 'w')

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

    var = f.createVariable(var_name + '_1@' + var_group, 'f', ('nlocs',))
    var[:] = [4.1, missing_float, 4.3, 4.4]

    var = f.createVariable(var_name + '_2@' + var_group, 'f', ('nlocs',))
    var[:] = [5.1, 5.2, 5.3, 5.4]

    var = f.createVariable(var_name + '_3@' + var_group, 'f', ('nlocs',))
    var[:] = [6.1, 6.2, 6.3, 6.4]

    f.date_time = 2018010100

    f.close()

def output_2d_geoval_to_netcdf(var_name, file_name):
    f = netcdf.netcdf_file(file_name, 'w')

    nlocs = 4
    nlevs = 3
    f.createDimension('nlocs', nlocs)
    f.createDimension('nlevs', nlevs)

    var = f.createVariable(var_name, 'f', ('nlocs','nlevs'))
    var[:] = [[1.1, 1.2, 1.3], [2.1, missing_float, 2.3], [3.1, 3.2, 3.3], [4.1, 4.2, 4.3]]

    f.date_time = 2018010100

    f.close()

if __name__ == "__main__":
    # VarObs
    output_1d_simulated_var_to_netcdf('surface_pressure',           'testinput/001_VarField_pstar.nc4') # Surface
    output_1d_simulated_var_to_netcdf('air_temperature',            'testinput/002_VarField_temperature_Surface.nc4')
    output_2d_simulated_var_to_netcdf('air_temperature',            'testinput/002_VarField_temperature_RadarZ.nc4')
    output_1d_simulated_var_to_netcdf('relative_humidity',          'testinput/003_VarField_rh_Surface.nc4')
    output_2d_simulated_var_to_netcdf('relative_humidity',          'testinput/003_VarField_rh_Sonde.nc4')
    output_1d_simulated_var_to_netcdf('eastward_wind',              'testinput/004_VarField_u_Surface.nc4')
    output_2d_simulated_var_to_netcdf('eastward_wind',              'testinput/004_VarField_u_Sonde.nc4')
    output_1d_simulated_var_to_netcdf('northward_wind',             'testinput/005_VarField_v_Surface.nc4')
    output_2d_simulated_var_to_netcdf('northward_wind',             'testinput/005_VarField_v_Sonde.nc4')
    output_2d_simulated_var_to_netcdf('brightness_temperature',     'testinput/010_VarField_britemp.nc4', with_bias=True)
    output_1d_normal_var_to_netcdf   ('sensor_zenith_angle', 'MetaData', 'testinput/019_VarField_satzenith.nc4')
    output_1d_geoval_to_netcdf       ('land_type_index',            'testinput/023_VarField_modelsurface_geoval.nc4')
    output_1d_normal_int_var_to_netcdf('satellite_id', 'MetaData',     'testinput/028_VarField_satid.nc4')
    output_1d_normal_var_to_netcdf   ('solar_zenith_angle', 'MetaData', 'testinput/031_VarField_solzenith.nc4')
    # 54 VarField_NumChans and 55 VarField_ChanNum: separate files not necessary
    output_2d_normal_var_to_netcdf   ('radar_azimuth', 'MetaData',  'testinput/066_VarField_radarobazim.nc4', with_radar_family=True)
    output_2d_simulated_var_to_netcdf('bending_angle',              'testinput/071_VarField_bendingangle.nc4')
    output_2d_normal_var_to_netcdf('impact_parameter', 'MetaData', 'testinput/072_VarField_impactparam.nc4')
    output_1d_normal_var_to_netcdf('earth_radius_of_curvature', 'MetaData',  'testinput/073_VarField_ro_rad_curv.nc4')
    output_1d_normal_var_to_netcdf('geoid_height_above_reference_ellipsoid', 'MetaData', 'testinput/074_VarField_ro_geoid_und.nc4')
    output_2d_simulated_var_to_netcdf('aerosol_optical_depth', 'testinput/077_VarField_aod.nc4')
    output_2d_simulated_var_to_netcdf('air_potential_temperature',  'testinput/078_VarField_theta.nc4') # Sonde
    output_1d_simulated_vars_to_netcdf('eastward_wind', 'northward_wind', 
                                       'testinput/reject_obs_with_all_variables_failing_qc.nc4')

    # Cx
    output_1d_simulated_var_to_netcdf('dummy',                      'testinput/dummy.nc4')
    output_1d_geoval_to_netcdf       ('surface_pressure',           'testinput/002_SurfaceCxField_pstar.nc4')
    output_2d_geoval_to_netcdf       ('air_potential_temperature',  'testinput/001_UpperAirCxField_theta.nc4')
    output_2d_geoval_to_netcdf       ('eastward_wind',              'testinput/003_UpperAirCxField_u.nc4')
    output_2d_geoval_to_netcdf       ('northward_wind',             'testinput/004_UpperAirCxField_v.nc4')
