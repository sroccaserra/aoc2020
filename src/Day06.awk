#!/usr/bin/env gawk -f

/^$/ { print l ; l = "" }
/.+/ { l = l" "$0 }
END { print l }
