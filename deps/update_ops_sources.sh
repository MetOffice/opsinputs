# Usage:
#
#   update_ops_sources.sh <ops_dir>
#
# Clear the ops/code and ops/public subdirectories. Then copy the OPS
# source files needed to build opsinputs from <ops_dir> (which should
# be a directory containing a working copy of the OPS FCM repository)
# into these subdirectories.

MY_DIR="`dirname "$0"`"
MY_DIR="`( cd "$MY_DIR" && pwd )`"

DEST_DIR="$MY_DIR/ops"
SRC_DIR="$1"

echo "Clearing $DEST_DIR/code..."
rm -rf $DEST_DIR/code

echo "Clearing $DEST_DIR/public..."
rm -rf $DEST_DIR/public

echo "Copying files from $PWD to $DEST_DIR..."

cd "$SRC_DIR/src"
cp --parents --recursive code/GenMod_ModelIO $DEST_DIR
cp --parents --recursive code/OpsMod_AirQuality $DEST_DIR
cp --parents --recursive code/OpsMod_Argument $DEST_DIR
cp --parents --recursive code/OpsMod_CXGenerate $DEST_DIR
cp --parents --recursive code/OpsMod_CXInfo $DEST_DIR
cp --parents --recursive code/OpsMod_Control $DEST_DIR
cp --parents --recursive code/OpsMod_DateTime $DEST_DIR
cp --parents --recursive code/OpsMod_GatherSpread $DEST_DIR
cp --parents --recursive code/OpsMod_HorizontalInterp $DEST_DIR
cp --parents --recursive code/OpsMod_ModelColumnIO $DEST_DIR
cp --parents --recursive code/OpsMod_ModelIO $DEST_DIR
cp --parents --recursive code/OpsMod_ModelObInfo $DEST_DIR
cp --parents --recursive code/OpsMod_ObTypeUtils $DEST_DIR
cp --parents --recursive code/OpsMod_ObsInfo $DEST_DIR
cp --parents --recursive code/OpsMod_ODB $DEST_DIR
cp --parents --recursive code/OpsMod_Process $DEST_DIR
cp --parents --recursive code/OpsMod_QC $DEST_DIR
cp --parents --recursive code/OpsMod_UMImport $DEST_DIR
cp --parents --recursive code/OpsMod_Utilities $DEST_DIR
cp --parents --recursive code/OpsMod_Varobs $DEST_DIR
cp --parents --recursive code/OpsMod_VarobsLib $DEST_DIR
cp --parents --recursive public/GenMod_Control $DEST_DIR
cp --parents --recursive public/GenMod_Core $DEST_DIR
cp --parents --recursive public/GenMod_Platform $DEST_DIR
cp --parents --recursive public/GenMod_Sleep $DEST_DIR
cp --parents --recursive public/GenMod_Utilities $DEST_DIR
cp --parents --recursive public/OpsMod_CharUtils $DEST_DIR
cp --parents --recursive public/OpsMod_EnvUtils $DEST_DIR
cp --parents --recursive public/OpsMod_IOUtils $DEST_DIR
cp --parents --recursive public/OpsMod_MPIInterface $DEST_DIR
cp --parents --recursive public/OpsMod_NetCDF $DEST_DIR
cp --parents --recursive public/OpsMod_Random $DEST_DIR
cp --parents --recursive public/OpsMod_Sort $DEST_DIR
cp --parents --recursive public/OpsMod_UMInterface $DEST_DIR
cp --parents --recursive public/OpsMod_VerticalInterp $DEST_DIR
cp --parents --recursive public/Ops_Constants $DEST_DIR

cp --parents code/OpsProg_Utils/OpsMod_PrintCX.f90 $DEST_DIR
cp --parents code/OpsProg_Utils/OpsMod_PrintVarobs.f90 $DEST_DIR
cp --parents code/OpsProg_Utils/OpsMod_UtilsProgs.f90 $DEST_DIR
cp --parents code/OpsProg_Utils/OpsProg_PrintCXFile.F90 $DEST_DIR
cp --parents code/OpsProg_Utils/OpsProg_PrintVarobs.F90 $DEST_DIR
cp --parents code/OpsProg_Utils/Ops_PrintBatchFieldByField.inc $DEST_DIR
cp --parents code/OpsProg_Utils/Ops_PrintBatchObByOb.inc $DEST_DIR
cp --parents code/OpsProg_Utils/Ops_PrintCXBatches.inc $DEST_DIR
cp --parents code/OpsProg_Utils/Ops_PrintCXHeader.inc $DEST_DIR
cp --parents code/OpsProg_Utils/Ops_PrintFixedHeader.inc $DEST_DIR
cp --parents code/OpsProg_Utils/Ops_PrintVarobsHeader.inc $DEST_DIR
cp --parents code/OpsProg_Utils/Ops_PrintVarobsObByOb.inc $DEST_DIR
cp --parents code/OpsProg_Utils/Ops_PrintVarobsSummary.inc $DEST_DIR
cp --parents code/OpsProg_Utils/Ops_ReadCXBatch.inc $DEST_DIR

echo "Done"
