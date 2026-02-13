{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CAMetalDisplayLinkUpdate@.
module ObjC.QuartzCore.CAMetalDisplayLinkUpdate
  ( CAMetalDisplayLinkUpdate
  , IsCAMetalDisplayLinkUpdate(..)
  , drawable
  , targetTimestamp
  , targetPresentationTimestamp
  , drawableSelector
  , targetPresentationTimestampSelector
  , targetTimestampSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.QuartzCore.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- drawable@
drawable :: IsCAMetalDisplayLinkUpdate caMetalDisplayLinkUpdate => caMetalDisplayLinkUpdate -> IO RawId
drawable caMetalDisplayLinkUpdate =
  sendMessage caMetalDisplayLinkUpdate drawableSelector

-- | @- targetTimestamp@
targetTimestamp :: IsCAMetalDisplayLinkUpdate caMetalDisplayLinkUpdate => caMetalDisplayLinkUpdate -> IO CDouble
targetTimestamp caMetalDisplayLinkUpdate =
  sendMessage caMetalDisplayLinkUpdate targetTimestampSelector

-- | @- targetPresentationTimestamp@
targetPresentationTimestamp :: IsCAMetalDisplayLinkUpdate caMetalDisplayLinkUpdate => caMetalDisplayLinkUpdate -> IO CDouble
targetPresentationTimestamp caMetalDisplayLinkUpdate =
  sendMessage caMetalDisplayLinkUpdate targetPresentationTimestampSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @drawable@
drawableSelector :: Selector '[] RawId
drawableSelector = mkSelector "drawable"

-- | @Selector@ for @targetTimestamp@
targetTimestampSelector :: Selector '[] CDouble
targetTimestampSelector = mkSelector "targetTimestamp"

-- | @Selector@ for @targetPresentationTimestamp@
targetPresentationTimestampSelector :: Selector '[] CDouble
targetPresentationTimestampSelector = mkSelector "targetPresentationTimestamp"

