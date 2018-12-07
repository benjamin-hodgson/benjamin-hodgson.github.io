#!/bin/bash
if [ -z "$2" ]
then echo "No title"; exit 1
fi

basename="$1-$2"

touch "posts/$basename.md"
mkdir "comments/$basename"
echo "---
author: Benjamin
website: http://www.benjamin.pizza
published: $1
---

To join the discussion, <a href=\"https://github.com/benjamin-hodgson/benjamin-hodgson.github.io/blob/live/comments/$basename/example.md\">send me a pull request</a>.
" > "comments/$basename/example.md"
