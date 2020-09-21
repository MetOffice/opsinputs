! GC.interface
!   Checks the interface has been well defined

#undef GC__INTERFACE_ID
#undef GC__INTERFACE
#undef GC__ERROR

#if defined(MPI_SRC)
#if defined(GC__INTERFACE)
#define GC__ERROR
#else
#define GC__INTERFACE_ID 1
#define GC__INTERFACE 'BUFFERED MPI'
#endif
#endif

#if defined(SERIAL_SRC)
#if defined(GC__INTERFACE)
#define GC_ERROR
#else
#define GC__INTERFACE_ID 0
#define GC__INTERFACE 'SERIAL'
#endif
#endif

#if !defined(GC__INTERFACE)
#define GC__INTERFACE_ID 0
#define GC__INTERFACE 'SERIAL'
#define SERIAL_SRC
#endif

#if defined(GC__ERROR)
ERROR gc: multiple interfaces defined.
#else
! GCOM Interface type: GC__INTERFACE
#endif
!============================================================
