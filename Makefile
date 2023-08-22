all:	non-ascii-by-paper.txt non-ascii-by-char.txt

non-ascii-by-paper.txt: papers/*.txt \
	bin/non-ascii-by-paper bin/non-ascii-by-paper.awk
	bin/non-ascii-by-paper papers/*.txt >"$@"

non-ascii-by-char.txt:	non-ascii-by-paper.txt \
	bin/non-ascii-by-char bin/non-ascii-by-char.awk
	# LC_ALL=C cat -v  "non-ascii-by-paper.txt" | LC_ALL=C bin/non-ascii-by-char >"$@"
	LC_ALL=C bin/non-ascii-by-char <"non-ascii-by-paper.txt" >"$@"
