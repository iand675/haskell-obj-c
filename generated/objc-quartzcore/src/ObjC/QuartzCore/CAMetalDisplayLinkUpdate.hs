{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CAMetalDisplayLinkUpdate@.
module ObjC.QuartzCore.CAMetalDisplayLinkUpdate
  ( CAMetalDisplayLinkUpdate
  , IsCAMetalDisplayLinkUpdate(..)
  , targetTimestamp
  , targetPresentationTimestamp
  , targetTimestampSelector
  , targetPresentationTimestampSelector


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

import ObjC.QuartzCore.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- targetTimestamp@
targetTimestamp :: IsCAMetalDisplayLinkUpdate caMetalDisplayLinkUpdate => caMetalDisplayLinkUpdate -> IO CDouble
targetTimestamp caMetalDisplayLinkUpdate  =
  sendMsg caMetalDisplayLinkUpdate (mkSelector "targetTimestamp") retCDouble []

-- | @- targetPresentationTimestamp@
targetPresentationTimestamp :: IsCAMetalDisplayLinkUpdate caMetalDisplayLinkUpdate => caMetalDisplayLinkUpdate -> IO CDouble
targetPresentationTimestamp caMetalDisplayLinkUpdate  =
  sendMsg caMetalDisplayLinkUpdate (mkSelector "targetPresentationTimestamp") retCDouble []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @targetTimestamp@
targetTimestampSelector :: Selector
targetTimestampSelector = mkSelector "targetTimestamp"

-- | @Selector@ for @targetPresentationTimestamp@
targetPresentationTimestampSelector :: Selector
targetPresentationTimestampSelector = mkSelector "targetPresentationTimestamp"

