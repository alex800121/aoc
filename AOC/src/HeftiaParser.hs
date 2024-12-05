{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

module HeftiaParser where

import Control.Applicative (Alternative (..), optional)
import Control.Monad (unless)
import Control.Monad.Hefty
import Control.Monad.Hefty.NonDet hiding (empty)
import Control.Monad.Hefty.State
import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy)
import Data.Char (isNumber)
import Control.Arrow

runNonDetMaybe :: forall ef a. Eff '[] (Choose ': Empty ': ef) a -> Eff '[] ef (Maybe a)
runNonDetMaybe =
  bundleN @2
    >>> interpretBy
      (pure . pure)
      ( ( \Choose k -> do
            x <- k False
            case x of
              Just y -> pure (pure y)
              Nothing -> k True
        )
          !+ (\Empty _ -> pure Nothing)
          !+ nil
      )

try :: forall a s eh ef. (ChooseH <<| eh, Empty <| ef, State String <| ef) => Eff eh ef a -> Eff eh ef a
try m = do
  s <- get @String
  m <|> (put s >> empty)

satisfy :: forall a eh ef. (ChooseH <<| eh, Empty <| ef, State String <| ef) => (Char -> Bool) -> Eff eh ef Char
satisfy p = do
  s <- get
  case s of
    c : r | p c -> put r >> pure c
    _ -> empty

anySingle :: (ChooseH <<| eh, Empty <| ef, State String <| ef) => Eff eh ef Char
anySingle = satisfy (const True)

char :: forall a eh ef. (ChooseH <<| eh, Empty <| ef, State String <| ef) => Char -> Eff eh ef Char
char c = satisfy (== c)

string :: forall eh ef. (ChooseH <<| eh, Empty <| ef, State String <| ef) => String -> Eff eh ef String
string s = do
  i <- get
  case stripPrefix s i of
    Nothing -> empty
    Just i' -> put i' >> pure s

takeWhileP :: forall a eh ef. (ChooseH <<| eh, Empty <| ef, State String <| ef) => (Char -> Bool) -> Eff eh ef String
takeWhileP p = do
  (a, s) <- span p <$> get
  put s
  pure a

takeWhile1P :: forall a eh ef. (ChooseH <<| eh, Empty <| ef, State String <| ef) => (Char -> Bool) -> Eff eh ef String
takeWhile1P p = (:) <$> satisfy p <*> takeWhileP p

takeRest :: forall a eh ef. (ChooseH <<| eh, Empty <| ef, State String <| ef) => Eff eh ef String
takeRest = do
  s <- get @String
  put @String []
  pure s

takeP :: forall a eh ef. (ChooseH <<| eh, Empty <| ef, State String <| ef) => Int -> Eff eh ef String
takeP n = do
  s <- get
  if length s >= n then let (a, b) = splitAt n s in put b >> pure a else empty

signedInteger :: forall a eh ef. (ChooseH <<| eh, Empty <| ef, State String <| ef) => Eff eh ef Int
signedInteger = try @Int @String do
  sign <- (char '-' >> pure negate) <|> (optional (char '+') >> pure id)
  n <- read @Int <$> takeWhile1P isNumber
  pure $ sign n

eof :: forall a eh ef. (ChooseH <<| eh, Empty <| ef, State String <| ef) => Eff eh ef ()
eof = do
  s <- get @String
  unless (null s) empty

parseMaybe p input = runPure $ evalState input $ runNonDet $ runChooseH (p <* eof)
