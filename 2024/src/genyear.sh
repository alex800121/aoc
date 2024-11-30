#!/usr/bin/env bash

for i in {1..25}; do
  touch Day$i.hs
  echo 'module Day'$i' where' >> Day$i.hs
  echo '' >> Day$i.hs
  echo 'day'$i' :: IO ()' >> Day$i.hs
  echo 'day'$i' = return ()' >> Day$i.hs
done
