#!/usr/bin/env bash

for i in {1..25}; do
  touch Day$i.hs
  echo 'module Day'$i' where' >> Day$i.hs
  echo '' >> Day$i.hs
  echo 'import Paths_AOC'$1 >> Day$i.hs
  echo '' >> Day$i.hs
  echo 'day'$i' :: IO ()' >> Day$i.hs
  echo 'day'$i' = do' >> Day$i.hs
  echo '  -- input <- (getDataDir >>= readFile . (++ "/input/input'$i'.txt"))' >> Day$i.hs
  echo '  return ()' >> Day$i.hs
done
