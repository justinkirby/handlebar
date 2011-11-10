#!/bin/sh

if test ! -x "handlebar"; then
    echo "compile handlebar via make!"
    exit 1
fi


mkdir tmp

./handlebar examples/one.src examples/one.vars > tmp/foobar
./handlebar examples/ -d tmp/foo
./handlebar examples/ -o tmp/bar
./handlebar -e fdsa -E asdf examples -o tmp/baz
