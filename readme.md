# Introduction
This module provides a filter for [pandoc](https://www.pandoc.org) that can be
used to create an interactive HTML page for the [Snowflake
Method](https://www.advancedfictionwriting.com/articles/snowflake-method) of novel writing.

The input document (e.g. a markdown file) will be used as a database that
contains the various items that you need to write according to the Snowflake
Method. You can combine this filter with other filters, e.g.
[pandoc-vimwiki](https://github.com/LarsEKrueger/pandoc-vimwiki) or
[pandoc-narrative-charts](https://github.com/LarsEKrueger/pandoc-narrative-charts).
The input document needs to be structured in a certain way (as explained in
the next sections). The output document, when exported as HTML and combined
with an appropriate Javascript function and appropriate CSS, will be
an interactive, single-page application that will guide you through the steps
according to the Snowflake Method.

The app is supposed to show the result of the relevant previous steps (e.g.
steps 2 and 3) of the Snowflake in a browser, while you type the text for the
current step (e.g. step 4) in a text editor. This ensures even slow computers
(e.g. subnotebooks/tablets) can provide a pleasant working environment.

This document assumes that you installed pandoc and this filter such that both
executables can be found in the program search path. Use [stack for
Windows/Linux/macOS](https://docs.haskellstack.org/en/stable/GUIDE/) or
[cabal](https://wiki.haskell.org/Cabal/How_to_install_a_Cabal_package) to
install either program.

# Setup of the Export Pipeline
This sections explains how to call pandoc to create the HTML app. We assume
that the input file is named `design.markdown` and we generate a file named
`design.html`.

Use the following command line arguments for pandoc:

```
pandoc --from markdown --to html --filter pandoc-snowflake -s -H design_header.html -o design.html design.markdown
```

The file [`design_header.html`](example/design_header.html) contains the Javascript code and the CSS
definitions to make the app work.

It is recommended to put the commands above in a [Windows
Batch](https://en.wikibooks.org/wiki/Windows_Batch_Scripting) or a [Shell
Script](https://www.shellscript.sh/) file.

If you generate multiple formats (e.g. `design.html`, the EPUB file,
and a PDF) from your novel, you can generate all those with one command.

If you use multiple filters, it is recommended to put `pandoc-snowflake` last.

# Structure of the Input Document
The input document as read by `pandoc-snowflake` is to be structured in the
following way. If you use other filters, ensure that they output the structure
as illustrated below.

## General Structure

The document is expected to contain three top-level headings (level 1):

```markdown
# Design

# Timeline

# Chapters
```

The header *Design* will contain the Snowflake database. All other headers at
this level (i.e. *Timeline* and *Chapters*) are copied to the output in the
same order they appear in. Any meta data (i.e. title and author) are
preserved. Any matter before the first header will be discarded.

## The Snowflake Database

### Basic Structure

The basic layout is structured according to the steps of the Snowflake Method.
However, as long as the nesting of the headers is reproduced as described
below, the order of the headers inside their higher-level header is not
important.

The *Design* header is expected to contain the following level-2 headers. The
text below the header explains the meaning of this section.

In addition to the Snowflake Method, there are three fields (*Internal
Conflict*, *External Conflict*, *Twist*) that I find helpful. [See
here](https://www.reddit.com/r/WritingPrompts/comments/6aqqdu/ot_friday_a_novel_idea_the_internal_and_the/)
for an explanation on them.

```markdown

# Design
## Premise
The one-sentence summary of the novel (step 1).

## Internal Conflict
This is not part of the Snowflake Method as explained in the link above.
However, I find it helpful to sketch the essential (emotional) conflict of the
hero at the start.

If you don't want to use this field, leave it empty.

## External Conflict
Again not part of the Method per se, this section is supposed to show a sketch
of what outer forces oppose the hero. If you don't want to use it, leave it
empty.

## Twist
Since I firmly believe a good (fiction) story requires a twist (or better, a
whole bunch of them), here's the place to list the twist you want to include.

## One Paragraph Summary
The one-paragraph summary of the novel (step 2).

## Characters
This section contains basic information about all your important characters,
respective step 3 of the Snowflake. How to structure this section will be
explained below (Step 3: Characters).

## One Page Summary
Step 4 of the Snowflake. Text here will be displayed as is, incl. formatting.

## Character Synopsis
Step 5 of the Snowflake. Text here will be displayed as is, incl. formatting.

## Four Page Summary
Step 6. See below.

## Character Details
Step 7. See below.

## Scenes
Step 8. The list of scenes. See below.
```

The filter does not provide a mechanism for step 9 (scene details) and step 10
(first draft). It is recommended to use the linking mechanism of
`pandoc-vimwiki` or the soon-to-be-released `vimwiki` parser of `pandoc` to
link the scenes to individual files. Putting each scene in a separate file
allows you to change the order of the scenes later.

### Step 3: Characters
The section `## Characters` is supposed to contain one level-3 header per
character. The title of the header should be the (short) name of the
character and the subordinate headers must be structured like this:

```markdown
## Characters
### Little Girl
#### Role
Protagonist

#### One-sentence summary
Send on a humanitarian mission by her mother, a little girl becomes an
accomplice in a cold-blooded murder of an sentient canine.

#### Goal
Bring cake and wine to her grandmother.

#### Motivation
Provide help for those in need.

#### Internal Conflict
An evil presence may have taken over her grandmother. Should the girl kill her
apparently senile relative?

#### Epiphany
It pays to be suspicious of strangers.

#### One-paragraph summary
A small girl is sent to bring her sick grandma food. On her voyage, she meets
a mysterious stranger. Once she arrives at her grandma's house, she notices
something strange about the old lady. Together with a hunter, she kills the
murderous stranger and frees her grandmother.
```

If you name the character *Character*, the section will be ignored. That
allows you to keep prepared copies of the section until they are ready to be
filled.

## Step 6: Four Page Summary

Each level-3 header in this section is matched with the respective paragraph
from step 4 and rendered as a table.

## Step 7: Character Details

Again, provide one level-3 section per character and name them appropriately.
Also, naming them *Character* lets you keep a prepared template as the filter
ignores it.

Each non-empty header with a non-empty body text will be converted into a
row of a table. The left column will contain the headline text, the right the
body text incl. formatting.

The following example show this:

```markdown
## Character Details
### Little Girl

#### Full Name
Little Red Riding Hood

#### Nickname
Red

#### Place of birth
A small town in the forest.

#### Current address

#### Favorite color
red
```

No row will be included in the table for *Current address* as the body is
empty.

The [template](example/design.md) contains the fields listed
[here](https://www.epiguide.com/ep101/writing/charchart.html).

## Step 8: Scenes

For each level-3 header of step 6, provide the respective level-3 header here.
For each scene in the act represented by the level-3 header, provide a level-4
header. The title of the header should be the character from who's point of
view, this scene is shown.

```markdown
## Scenes

### Act 1

#### Little Girl
Little Girl's mother packs a basket of cake and wine, before she sends Little
Girl to her grandmother.

#### Hunter
The hunter sees a little girl with a basket in the forest. He approaches her
to warn her of the Big Bad Wolf.
```

If you write a complicated story and need to keep track of where characters
are at which place in which scene, you can use the `pandoc-narrative-charts`
filter to generate such a chart. Add the chart events to the scene sections
and add the plot in a top-level section.

You might also want to link to the intermediate or final scenes (steps 9 and
10) using `pandoc-vimwiki` until the full vimwiki parser becomes part of
pandoc. Another option is to use `pandoc-include` and maintain the links
manually.

<!--
vim: tw=78
-->
