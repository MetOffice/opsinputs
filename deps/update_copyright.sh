#!/bin/bash -e
#shellcheck disable=2086
#
# Update copyright statements removing unnecessary text from file headers
# in gcom and ops codes.
#
MY_DIR="$(dirname "$0")"

echo "1. pdating gcom headers..."
# Delete 2 lines from line matching gcom_patss
gcom_pats=("Please refer to Copyright file in top level GCOM directory"
           "For further details please refer to the file Copyright")
if flist=$(grep -Rl "${gcom_pats[0]}\|${gcom_pats[1]}" "${MY_DIR}/gcom"); then
  sed -i "/${gcom_pats[0]}/,+1d;/${gcom_pats[1]}/,+1d" $flist
else :;
fi

echo "2. Updating ops headers..."
# Delete single/two lines matching ops_pat1/ops_pat2 respectively
ops_pat1="Refer to COPYRIGHT.txt of this distribution for details."
if flist=$(grep -Rl "$ops_pat1" "${MY_DIR}/ops"); then
  sed -i "/$ops_pat1/d" $flist
else :;
fi

ops_pat2="For further details please refer to the file COPYRIGHT.txt"
if flist=$(grep -Rl "$ops_pat2" "${MY_DIR}/ops"); then
  sed -i "/$ops_pat2/,+1d" $flist
else :;
fi

# --- using find (little slower)
# find "${MY_DIR}/gcom" -type f -exec \
#   sed -i "/${gcom_pats[0]}/,+1d;/${gcom_pats[1]}/,+1d" {} \;
# find "${MY_DIR}/ops" -type f -exec sed -i "/$ops_pat1/d" {} \;
# find "${MY_DIR}/ops" -type f -exec sed -i "/$ops_pat2/,+1d" {} \;
# ---
