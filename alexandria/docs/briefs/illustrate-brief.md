# Illustration wave brief: from the ingested originals to the notebooks

Pedro's goal (2026-10-05): download the original works, ingest them with hive-ingestor, then
"use swarm of agents to illustrate them in notebooks".

Corpus: $HOME/Documents/feed/math-history/ (manifest.edn lists every file, its text, its source and
licence basis). Ingested into hive memory under project scope `math-history`: query it with
`mcp__hive__memory command="search" query="..." scope="math-history"` (and `ingest search`), which
returns chunks with their source path and position. Read the text files directly for exact wording
(they are on disk, plain UTF-8).

## Your job (one era per ling)
1. Read your work's ORIGINAL and its translation through the corpus. Make an inventory of the
   propositions, constructions, problems, tables and figures the work contains (book, proposition,
   page), and mark which ones the series' notebook already illustrates.
2. Choose the next ones worth illustrating: the propositions the author's argument depends on, the
   famous figures, the tables the author computed. Prefer depth over breadth: a proposition fully
   drawn, stepped, and graded beats five mentioned.
3. Add them to alexandria: shelf entries (with the original-language statement quoted short and the
   PD English translation, both pointing at the corpus file and page), proofs.edn steps, Emmy
   identities graded :grade/proved, numbers from alexandria.raster (quadgk, brent, RK4, kernels),
   and moving figures as raster kernels (KernelFigure; medium.* owned by hom-raster-mechanics until it
   merges, then shared). Teach the pattern, not repeats.
4. Extend the era's history-of-math notebook to mount them; add a 'Reading the original' section per
   proposition: the short original quote, the translation, the page reference, and what the figure in
   the original edition shows next to our moving figure.
5. Verify: alexandria core and media suites cold; the history-of-math build for your notebook with no
   FAILS badge; and a Chromium check of the page (no page errors).

House rules, tools and finishing steps: the common brief (common-brief.md) and the raster brief
(raster-brief.md), both in the same directory as this file, apply. Never print long passages of a
book in one output; quote one or two sentences at a time.
