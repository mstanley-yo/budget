#!/bin/bash

export PATH="/opt/homebrew/bin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin"

cd /Users/stanleyyo/Python/budget/ || exit 1
/opt/homebrew/bin/python3 get_budget.py "$@"
