#!/bin/sh

# This is probably a bit harsh because it also matches windows endings
# and they should be OK.

# Run as
#   check_line_endings.sh csv
# to search all csv files in the working directory.  Or run without
# any argument to search the index (what will be commited) but
# checking *all* files.

if [[ $1 == "csv" ]]
then
    # echo "checking all csv files"
    BAD_ENDINGS=$(find data -name '*.csv' -print0 | xargs -0 grep -l $'\r$')
else
    # echo "checking index"
    BAD_ENDINGS=$(git diff-index --cached -S$'\r$' --name-only HEAD)
fi

if test -z "$BAD_ENDINGS"
then
    exit 0
else
    echo "## Files with bad line endings:"
    echo "$BAD_ENDINGS"
    exit 1
fi
