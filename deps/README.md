The `gcom`, `odb` and `ops` folders contain fragments of the source
code of the Gcom, ODB and OPS software packages. These are accompanied
by a `CMakeLists.txt` file used to build that code and, for `odb` and
`ops`, a `stubs` folder containing modified versions of the original
files (typically removing code unused by `opsinputs`, but introducing
extra dependencies). (In fact, for `odb` all that is needed is heavily
cut-down versions of two original files that no longer contain any
executable code.)

Use the `update_*.sh` scripts to bring the original source files in sync
with a more recent version of Gcom and OPS. For example, run

    ./update_ops_sources.sh /path/to/working/copy/of/ops

to copy files from a working copy of OPS into the `ops` folder.

If you need to modify any Gcom or OPS files, do not edit them
in-place; instead, move them into an appropriate `stubs` folder (or
another folder of your choice), at the same time adjusting the path to
these files in the corresponding `CMakeLists.txt` file and excluding
them from the list of files copied by the corresponding
`update_*_sources.sh` script. Then edit the moved files. This will
ensure modified files can be easily distinguished from the original
ones and prevent your modifications from being overwritten the next
time the `update_*_sources.sh` script is run.
