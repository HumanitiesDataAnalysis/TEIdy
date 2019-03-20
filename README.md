# TEIdy
An R package to load TEI and other XML texts as a set of tag interactions.

The [tidytext](http://tidytextmining.com) package by Julia Silge and David Robinson presents a compelling mechanism for bringing
exploratory data analysis to unstructure or lightly structured texts. 

But it doesn't work that well with the extraordinary metadata built into many TEI and XML-encoded texts. 

This is too bad, because the one-word-per-row and grouping-related philosophy of tidytext
actually works extremely well in TEI formats.

If you want to look at usage of country words in the Foreign Relations of the United States by author, or if you want to 
topic model Shakespeare's plays one speech at a time, the combination of TEI and tidytext analysis provides a 
perfect way to do that.

This package begins to remedy this problem by providing ways to parse out any XML text (of which TEI is a subset) into a set of 
textual elements with dataframe columns describing the tagged context it appears in. Divs are reduced to their type, if present,
and id and n fields are cast to the type. 

So the following XML:

```xml
<div type="act" n="1">
Act 1
<div type="scene" n="1">
<stage>
Enter Hamlet
</stage>
<speech speaker="Ghost">
Hi Hamlet, I'm your dad!
</speech>
</div>
</div>
```

Might be, ideally, rendered something like this:

```
.txt                    .hierarchy                 act       scene    speech   speaker
Act 1                   div.act                    1         NA       NA       NA
Enter Hamlet            div.act>div.scene>stage    1         1        NA       NA
Hi Hamlet, I'm your dad!div.act>div.scene>speech   1         1        TRUE     Ghost
```

This lets you quickly use filter, group_by, and other tidyverse operations
to explore text. 

This is all still a bit experimental, and--despite a few efforts on my part--quite slow.
And of course it doesn't do everything that a full XML query language should do, and while
some information can be extracted from the teiHeader through join operations, just 
how you'd do that is probably obscure to anyone but full tidyverse acolytes.

It's even possible it would make more sense to write a translation language between dplyr operations 
and xpath operations. But something along these lines is quite useful. Thoughts welcome.

