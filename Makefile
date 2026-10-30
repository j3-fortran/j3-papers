all:	meeting-docs index file-types.txt

# Fetch notes for three current meetings and one future meeting.
MEETINGS=$$(/bin/ls meetings | tail -3 | awk '{ print; n=$$1; } END {printf "%d\n", n+1; }')

meeting-docs:
	bin/get-docs-for-meeting ${MEETINGS}

index: papers
	bin/update-index "$@" papers meetings years

file-types.txt:	papers
	bin/update-file-types "$@" papers

non-ascii-by-paper.txt: papers/*.txt \
	    bin/non-ascii-by-paper bin/non-ascii-by-paper.awk
	bin/non-ascii-by-paper papers/*.txt >"$@"

non-ascii-by-char.txt:	non-ascii-by-paper.txt \
	    bin/non-ascii-by-char bin/non-ascii-by-char.awk
	LC_ALL=C bin/non-ascii-by-char <"non-ascii-by-paper.txt" >"$@"
