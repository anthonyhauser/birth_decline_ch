#!/bin/bash
# Generates parameters_mod8.txt with one line per model job (1-5):
# 1: all ctz,     all parity    (main, 2000:last_year)
# 2: all ctz,     first parity  (2005:last_year)
# 3: all ctz,     second parity (2005:last_year)
# 4: swiss only,  all parity    (2005:last_year)
# 5: non-swiss,   all parity    (2005:last_year)

filename="parameters_mod8.txt"
rm -f ${filename}

for ra in 1 2 3 4 5; do
    echo "$ra" >> ${filename}
done

Nlines=$(grep -c "" < ${filename})
echo "Generated $Nlines job configurations in ${filename}"
