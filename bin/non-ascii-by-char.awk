# Expect lines like
#    papers/00-140r1.txt x93\223:1 x94\224:1
# Create the inversion with lines like
#    x93\223:59 papers/00-140r1.txt
BEGIN {
    FS=" "
}

#NR > 10 { next}

{
    #printf("   file '%s'\n", $1)
    for (f = 2; f <= NF; f++) {
        char = gensub(":.*", "", "g", $f)
        count = gensub(".*:", "", "g", $f)
        if (char in counts) {
            counts[char] = counts[char] + count
        } else {
            counts[char] = count
        }

        places[char] = places[char] " " $1
    }
}

END {
    for (char in counts) {
        printf("%s %d %s\n", char, counts[char], places[char])
    }
}
