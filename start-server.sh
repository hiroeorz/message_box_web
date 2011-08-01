#!/bin/sh
cd `dirname $0`

exec erl -pa $PWD/ebin $PWD/deps/*/ebin -boot start_sasl -s reloader -s message_box_web -name message_box_web@127.0.0.1 -setcookie message_box_cookie
