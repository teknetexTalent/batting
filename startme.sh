#!/usr/bin/env bash

erlc *.erl
erl -noshell -s baseball start -s init stop
