{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

module HeftiaParser where

import Control.Applicative (Alternative (..), optional)
import Control.Arrow
import Control.Monad (msum, unless)
import Control.Monad.Hefty
import Control.Monad.Hefty.NonDet hiding (empty)
import Control.Monad.Hefty.State
import Data.Char (isNumber)
import Data.Effect.NonDet qualified as NonDet
import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy)

runNonDetMaybe :: forall ef a. Eff '[] (Choose ': Empty ': ef) a -> Eff '[] ef (Maybe a)
runNonDetMaybe =
  bundleN @2
    >>> interpretBy
      (pure . pure)
      ( (\Choose k -> maybe (k True) (pure . pure) =<< k False)
          !+ (\Empty _ -> pure Nothing)
          !+ nil
      )

-- msplit :: forall ef eh a. (Choose <| ef, ChooseH <<| eh, Empty <| ef) => Eff eh ef a -> Eff eh ef (Maybe (a, Eff eh ef a))
-- msplit = loop []
--   where
--     loop jq (Val a) = pure (Just (a, msum jq))
--     loop jq (Op u q) = case u of
--       Left uh -> case prjH @ChooseH uh of
--         Nothing -> Op u k
--         Just (ChooseH a b) -> undefined
--       Right uf -> case prj @Empty uf of
--         Nothing -> undefined
--         Just Empty -> undefined
--       where
--         k = _f q (loop jq)

-- "try" may not be needed, because proper backtracking is default
-- try :: forall a s eh ef. (Empty <| ef, State String <| ef) => Eff eh ef a -> Eff eh ef a
-- try = id

satisfy :: forall a eh ef. (Empty <| ef, State String <| ef) => (Char -> Bool) -> Eff eh ef Char
satisfy p = do
  s <- get
  case s of
    c : r | p c -> put r >> pure c
    _ -> NonDet.empty

anySingle :: (Empty <| ef, State String <| ef) => Eff eh ef Char
anySingle = satisfy (const True)

char :: forall a eh ef. (Empty <| ef, State String <| ef) => Char -> Eff eh ef Char
char c = satisfy (== c)

string :: forall eh ef. (Empty <| ef, State String <| ef) => String -> Eff eh ef String
string s = do
  i <- get
  case stripPrefix s i of
    Nothing -> NonDet.empty
    Just i' -> put i' >> pure s

takeWhileP :: forall a eh ef. (Empty <| ef, State String <| ef) => (Char -> Bool) -> Eff eh ef String
takeWhileP p = do
  (a, s) <- span p <$> get
  put s
  pure a

takeWhile1P :: forall a eh ef. (Empty <| ef, State String <| ef) => (Char -> Bool) -> Eff eh ef String
takeWhile1P p = (:) <$> satisfy p <*> takeWhileP p

takeRest :: forall a eh ef. (Empty <| ef, State String <| ef) => Eff eh ef String
takeRest = do
  s <- get @String
  put @String []
  pure s

takeP :: forall a eh ef. (Empty <| ef, State String <| ef) => Int -> Eff eh ef String
takeP n = do
  s <- get
  if length s >= n then let (a, b) = splitAt n s in put b >> pure a else NonDet.empty

signedInteger :: forall a eh ef. (Choose <| ef, Empty <| ef, State String <| ef) => Eff eh ef Int
signedInteger = do
  sign <- (char '-' >> pure negate) `branch` ((char '+' `branch` pure '+') >> pure id)
  n <- read @Int <$> takeWhile1P isNumber
  pure $ sign n

eof :: forall a eh ef. (Empty <| ef, State String <| ef) => Eff eh ef ()
eof = do
  s <- get @String
  unless (null s) NonDet.empty

parseMaybe p input = runPure $ evalState input $ runNonDet (p <* eof)
