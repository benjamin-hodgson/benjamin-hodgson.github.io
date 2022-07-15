#!/usr/bin/env python3

import os
import sys
from datetime import date

for f in os.listdir("posts"):
    if date.fromisoformat(sys.argv[1][0:10]) <= date.fromisoformat(f[0:10]) <= date.today():
        print("::set-output name=result::true")
        exit()
print("::set-output name=result::false")

