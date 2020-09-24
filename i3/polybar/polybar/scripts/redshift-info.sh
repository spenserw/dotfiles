#!/bin/bash

PROC_COUNT=$(pgrep --exact -c redshift)

if [[ $PROC_COUNT -gt 0 ]]; then
  echo "%{F#fc7703}[RS]%{F-}"
else
  echo "%{F#dfdfdf}[RS]%{F-}"
fi
