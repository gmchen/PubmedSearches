#!/bin/bash
OUTDIR="Pubmed_NEJM"
DATE=`date +%Y-%m-%d`
mkdir "$OUTDIR"
mkdir "$OUTDIR/xml"
echo "Pubmed access date: $DATE\n\n" > "$OUTDIR/out.log"
cat eutils_queries_nejm.sh >> "$OUTDIR/out.log"
for YEAR in {1975..1966}
do
	for MONTH in {1..12}
	do
		echo "Year $YEAR Month $MONTH"
		esearch -db pubmed -query "NEJM [journal] AND $YEAR/$MONTH [dp]" | \
		efetch -format xml | \
		tee >(xtract -pattern Article -element ArticleTitle > "$OUTDIR/${YEAR}_${MONTH}_TitleText.txt") | \
		tee >(xtract -pattern MeshHeading -element DescriptorName > "$OUTDIR/${YEAR}_${MONTH}_KeywordText.txt") | \
		tee >(xtract -pattern Abstract -element AbstractText > "$OUTDIR/${YEAR}_${MONTH}_AbstractText.txt") > \
		"$OUTDIR/xml/${YEAR}_${MONTH}.xml"
		
		sleep 5
	done
done
