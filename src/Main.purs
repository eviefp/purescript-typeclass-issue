module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Symbol (class IsSymbol, SProxy)
import Data.Variant (Variant, inj)

-- | This compiles just fine.
class WithoutSym t where
  wrel ∷ ∀ sym r1 r2
       . IsSymbol sym
       ⇒ RowCons sym t r1 r2
       ⇒ SProxy sym
       → t
       → Variant r2

instance wsInt ∷ WithoutSym Int where
  wrel sym p = inj sym p

-- This does not work.
class IsSymbol sym <= Rel t sym where
  rel ∷ ∀ r1 r2
      . RowCons sym t r1 r2
      ⇒ SProxy sym
      → t
      → Variant r2

instance relInt ∷ Rel Int "test" where
-- | Compile error is here, at inj.
  rel sym p = inj sym p

-- | This works just fine as well.
rel' ∷ ∀ r1 r2 t sym
     . IsSymbol sym
     ⇒ RowCons sym t r1 r2
     ⇒ SProxy sym
     → t
     → Variant r2
rel' sym p = inj sym p