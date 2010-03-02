#!/bin/sh

if [ -x MyTronBot ]; then
    java -jar engine/Tron.jar maps/huge-room.txt "./MyTronBot" "java -jar example_bots/RunAway.jar" 0;
else
    java -jar engine/Tron.jar maps/huge.txt "sbcl --noinform --noprint --no-userinit --no-sysinit --disable-debugger --load MyTronBot.lisp --eval \"(my-tron-bot::main)\"" "java -jar example_bots/RunAway.jar";
fi
