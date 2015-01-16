{-# LANGUAGE RankNTypes #-}
-- | Syntactic sugar that improves the usability of 'Control.Exception.assert'
-- and 'error'.
--
-- This is actually a bunch of hacks wrapping the original @assert@ function,
-- which is, as of GHC 7.8, the only simple way of obtaining source positions.
-- The original @assert@ function is here re-exported for convenience.
--
-- Make sure to enable assertions for your cabal package, e.g., by setting
--
-- > ghc-options: -fno-ignore-asserts
--
-- in your .cabal file. Otherwise, some of the functions will have
-- no effect at all.
module Control.Exception.Assert.Sugar
  ( assert, blame, failure, twith, swith, allB, forceEither
  ) where

import Control.Exception (assert)
import Data.Text (Text)
import Debug.Trace (trace)
import qualified Text.Show.Pretty as Show.Pretty (ppShow)

infix 1 `blame`
-- | If the condition fails, display the value blamed for the failure.
-- Used as in
--
-- > assert (age < 120 `blame` age) $ savings / (120 - age)
blame :: Show a => Bool -> a -> Bool
{-# INLINE blame #-}
blame True _ = True
blame False blamed = trace (blameMessage blamed) False

blameMessage :: Show a => a -> String
{-# NOINLINE blameMessage #-}
blameMessage blamed = "Contract failed and the following is to blame:\n  "
                      ++ Show.Pretty.ppShow blamed

infix 1 `failure`
-- | Like 'error', but shows the source position and also
-- the value to blame for the failure. To be used as in
--
-- > case xs of
-- >   0 : _ -> assert `failure` (xs, "has an insignificant zero")
failure :: Show a => (forall x. Bool -> x -> x) -> a -> b
{-# NOINLINE failure #-}
failure asrt blamed =
  let s = "Internal failure occurred and the following is to blame:\n  "
          ++ Show.Pretty.ppShow blamed
  in trace s
     $ asrt False
     $ error "Control.Exception.Assert.Sugar.failure"
         -- Lack of no-ignore-asserts or GHC < 7.4.

infix 2 `twith`
-- | Syntactic sugar for the pair operation, to be used in 'blame'
-- and 'failure' as in
--
-- > assert (age < 120 `blame` "age too high" `twith` age) $ savings / (120 - age)
--
-- or
--
-- > case xs of
-- >   0 : _ -> assert `failure` "insignificant zero" `twith` xs
--
-- Fixing the first component of the pair to @Text@ prevents warnings
-- about defaulting.
twith :: Text -> b -> (Text, b)
{-# INLINE twith #-}
twith t b = (t, b)

infix 2 `swith`
-- | The same as 'twith', but for 'String', not 'Text'.
--
-- Syntactic sugar for the pair operation, to be used in 'blame'
-- and 'failure' as in
--
-- > assert (age < 120 `blame` "age too high" `swith` age) $ savings / (120 - age)
--
-- or
--
-- > case xs of
-- >   0 : _ -> assert `failure` "insignificant zero" `swith` xs
--
-- Fixing the first component of the pair to @String@ prevents warnings
-- about defaulting.
swith :: String -> b -> (String, b)
{-# INLINE swith #-}
swith t b = (t, b)

-- | Like 'List.all', but if the predicate fails, blame all the list elements
-- and especially those for which it fails. To be used as in
--
-- > assert (allB (<= height) [yf, y1, y2])
allB :: Show a => (a -> Bool) -> [a] -> Bool
{-# INLINE allB #-}
allB predicate l = blame (all predicate l) $ allBMessage predicate l

allBMessage :: Show a => (a -> Bool) -> [a] -> String
{-# NOINLINE allBMessage #-}
allBMessage predicate l = Show.Pretty.ppShow (filter (not . predicate) l)
                          ++ " in the context of "
                          ++ Show.Pretty.ppShow l

infix 1 `forceEither`
-- | Assuming that @Left@ signifies an error condition,
-- check the @Either@ value and, if @Left@ is encountered,
-- fail outright and show the error message. Used as in
--
-- > assert `forceEither` parseOrFailWithMessage code
forceEither :: Show a => (forall x. Bool -> x -> x) -> Either a b -> b
{-# NOINLINE forceEither #-}
forceEither asrt (Left a)  = asrt `failure` a
forceEither _    (Right b) = b
