#!/bin/bash
OUTDIR="AbstractOutput"
DATE=`date +%Y-%m-%d`
mkdir "$OUTDIR"
echo "Pubmed access date: $DATE\n\n" > "$OUTDIR/out.log"
cat eutils_queries.sh >> "$OUTDIR/out.log"
for YEAR in {2015..2014}
do
	for MONTH in {1..12}
	do
		echo "Year $YEAR Month $MONTH"
		esearch -db pubmed -query "JAMA [journal] AND $YEAR/$MONTH [dp]" | \
		tee >(efetch -format xml | xtract -pattern Article -element ArticleTitle > "$OUTDIR/${YEAR}_${MONTH}_TitleText.txt") | \
		efetch -format xml | xtract -pattern Abstract -element AbstractText > \
		"$OUTDIR/${YEAR}_${MONTH}_AbstractText.txt"
		
		sleep 5
	done
done