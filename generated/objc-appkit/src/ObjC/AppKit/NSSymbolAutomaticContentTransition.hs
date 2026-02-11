{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | The default symbol transition, resolves to a particular transition in a context-sensitive manner.
--
-- Generated bindings for @NSSymbolAutomaticContentTransition@.
module ObjC.AppKit.NSSymbolAutomaticContentTransition
  ( NSSymbolAutomaticContentTransition
  , IsNSSymbolAutomaticContentTransition(..)
  , transition
  , transitionSelector


  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | The default automatic transition, determined by the system.
--
-- ObjC selector: @+ transition@
transition :: IO (Id NSSymbolAutomaticContentTransition)
transition  =
  do
    cls' <- getRequiredClass "NSSymbolAutomaticContentTransition"
    sendClassMsg cls' (mkSelector "transition") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @transition@
transitionSelector :: Selector
transitionSelector = mkSelector "transition"

