module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Symbol (class IsSymbol, SProxy)
import Data.Variant (Variant, inj)

class IsSymbol sym <= Rel parent child sym | sym → child where
  rel ∷ ∀ r1 r2
      . RowCons sym child r1 r2
      ⇒ SProxy sym
      → parent
      → Variant r2

instance relInt ∷ Rel Int Int "int" where
  rel sym p = inj sym p

rel' ∷ ∀ r1 r2 parent child sym
     . IsSymbol sym
     ⇒ RowCons sym child r1 r2
     ⇒ SProxy sym
     → parent
     → Variant r2
rel' sym p = inj sym p