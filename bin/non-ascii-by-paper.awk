{
    for (i = 2; i <= NF; i++) {
        histo[$i + 0]++;
    }
}

END {
    printed_any = 0;
    for (i = 0; i <= 255; i++) {
        if (i in histo) {
            if (printed_any) printf " ";
            printf "x%02x\\%o%c:%d", i, i, i, histo[i]
            ++printed_any
        }
    }
    if (printed_any) printf "\n"
}
