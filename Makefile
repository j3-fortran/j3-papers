all:	meeting-docs index file-types

MEETINGS=232
meeting-docs:
	bin/get-docs-for-meeting ${MEETINGS}

index: papers
	bin/update-index "$@" papers meetings years

file-types:	papers
	bin/update-file-types "$@" papers

# non-ascii-by-paper.txt: papers/*.txt \
# 	bin/non-ascii-by-paper bin/non-ascii-by-paper.awk
# 	bin/non-ascii-by-paper papers/*.txt >"$@"

# non-ascii-by-char.txt:	non-ascii-by-paper.txt \
# 	bin/non-ascii-by-char bin/non-ascii-by-char.awk
# 	LC_ALL=C bin/non-ascii-by-char <"non-ascii-by-paper.txt" >"$@"
