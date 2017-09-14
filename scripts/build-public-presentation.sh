#!/bin/bash

stack install :thesis \
  --exec='thesis build public-presentation' \
  --exec='thesis build presenter-public-presentation'
