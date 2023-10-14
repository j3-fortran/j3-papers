all:	meeting-docs INDEX file-types

MEETING=231
meeting-docs:
	bin/get-docs-for-meeting -c ${MEETING}

INDEX: papers
	bin/make-index "$@" papers meetings years

file-types:	papers
	file papers/* >"$@"

# non-ascii-by-paper.txt: papers/*.txt \
# 	bin/non-ascii-by-paper bin/non-ascii-by-paper.awk
# 	bin/non-ascii-by-paper papers/*.txt >"$@"

# non-ascii-by-char.txt:	non-ascii-by-paper.txt \
# 	bin/non-ascii-by-char bin/non-ascii-by-char.awk
# 	LC_ALL=C bin/non-ascii-by-char <"non-ascii-by-paper.txt" >"$@"

# Steve Lionel was curious about why so many files
# were changed since he became convenor. He wonders
# if he could improve the upload process.
updated-lionel-files:
	echo "$$(echo papers/{17,18,19,20,21,22,23}-*.txt | wc -w) files"
	CHANGES=0; TL=0; TW=0; CRLF=0; \
	for F in papers/{17,18,19,20,21,22,23}-*.txt; do \
		LOG=`git log "$$F"`; \
		C=; \
		if [[ "$$LOG" =~ "blank lines" ]]; then C="$$C trail-lines"; TL=$$((TL+1)); fi; \
		if [[ "$$LOG" =~ "Remove white" ]]; then C="$$C trail-white"; TW=$$((TW+1)); fi; \
		if [[ "$$LOG" =~ "CRLF" ]]; then C="$$C CRLF"; CRLF=$$((CRLF+1)); fi; \
		if [[ "$$C" != "" ]]; then echo "$$F: $$C"; CHANGES=$$((CHANGES+1)); fi; \
	done; \
	echo "$$CHANGES changed: $$TL lines; $$TW whitespace; $$CRLF CRLF."
