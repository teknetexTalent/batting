#!/usr/bin/env bash

export SQL_LITE="/usr/lib/erlang/lib/sqlite-1.0.0"
export PATH=$SQL_LITE/priv:$PATH
erlc *.erl && erl -pa $SQL_LITE/ebin -noshell -noinput -s batmachine start -s init stop
