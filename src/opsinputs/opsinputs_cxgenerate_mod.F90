! (C) Copyright 2020 Met Office UK
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.

!> Subset of the OpsMod_CxGenerate module from OPS. That module can't be used directly because it
!> depends on libodb (via the OpsMod_Extract module), which in turn is linked to a different
!> version of eckit than the JEDI libraries.

module opsinputs_cxgenerate_mod

implicit none

integer            :: CxLevels = 30
integer, parameter :: MaxModelCodes = 100

contains

include 'Ops_GetDefaultBgerrFields.inc'
include 'Ops_GetDefaultCxFields.inc'
include 'Ops_ReadCXControlNL.inc'

end module opsinputs_cxgenerate_mod
