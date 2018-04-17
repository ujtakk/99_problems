#!/bin/zsh

OCAML=utop

for script in `ls -v *.ml`; do
  $OCAML $script > /dev/null 2>&1
  if [ $? -eq 0 ]; then
    echo "\e[32mPASS\e[m: $OCAML $script"
  else
    echo "\e[31mFAIL\e[m: $OCAML $script"
  fi
done

exit 0

