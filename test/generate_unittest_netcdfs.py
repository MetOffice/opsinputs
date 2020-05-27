import numpy as np
from scipy.io import netcdf

missing_float = np.finfo(np.float32).min * 0.99

def output_1d_var_to_netcdf(var_name, file_name):
    f = netcdf.netcdf_file(file_name, 'w')

    nlocs = 4
    f.createDimension('nlocs', nlocs)

    var = f.createVariable('latitude@MetaData', 'f', ('nlocs',))
    var[:] = [21, 22, -23, 24]
    var = f.createVariable('longitude@MetaData', 'f', ('nlocs',))
    var[:] = [31, 32, 33, 34]
    air = f.createVariable('air_pressure@MetaData', 'f', ('nlocs',))
    air[:] = [100100, 100200, 100300, 100400]
    var = f.createVariable('time@MetaData', 'f', ('nlocs',))
    minute = 1/60.
    var[:] = [1*minute, 2*minute, 3*minute, 4*minute]

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

output_1d_var_to_netcdf('air_temperature', 'testinput/002_VarField_temperature_Surface.nc4')
