#!/bin/bash

stack install :thesis \
  --exec='thesis build ademic-presentation' \
  --exec='thesis build presenter-academic presentation'
