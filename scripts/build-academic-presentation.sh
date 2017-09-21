#!/bin/bash

stack install :thesis \
  --exec='thesis build academic-presentation' \
  --exec='thesis build presenter-academic-presentation'
