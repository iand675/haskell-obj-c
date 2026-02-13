{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | BEAccessibilityTextMarkerRange holds the start and end markers for a text range.
--
-- Generated bindings for @BEAccessibilityTextMarkerRange@.
module ObjC.BrowserEngineKit.BEAccessibilityTextMarkerRange
  ( BEAccessibilityTextMarkerRange
  , IsBEAccessibilityTextMarkerRange(..)
  , startMarker
  , setStartMarker
  , endMarker
  , setEndMarker
  , endMarkerSelector
  , setEndMarkerSelector
  , setStartMarkerSelector
  , startMarkerSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.BrowserEngineKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- startMarker@
startMarker :: IsBEAccessibilityTextMarkerRange beAccessibilityTextMarkerRange => beAccessibilityTextMarkerRange -> IO (Id BEAccessibilityTextMarker)
startMarker beAccessibilityTextMarkerRange =
  sendMessage beAccessibilityTextMarkerRange startMarkerSelector

-- | @- setStartMarker:@
setStartMarker :: (IsBEAccessibilityTextMarkerRange beAccessibilityTextMarkerRange, IsBEAccessibilityTextMarker value) => beAccessibilityTextMarkerRange -> value -> IO ()
setStartMarker beAccessibilityTextMarkerRange value =
  sendMessage beAccessibilityTextMarkerRange setStartMarkerSelector (toBEAccessibilityTextMarker value)

-- | @- endMarker@
endMarker :: IsBEAccessibilityTextMarkerRange beAccessibilityTextMarkerRange => beAccessibilityTextMarkerRange -> IO (Id BEAccessibilityTextMarker)
endMarker beAccessibilityTextMarkerRange =
  sendMessage beAccessibilityTextMarkerRange endMarkerSelector

-- | @- setEndMarker:@
setEndMarker :: (IsBEAccessibilityTextMarkerRange beAccessibilityTextMarkerRange, IsBEAccessibilityTextMarker value) => beAccessibilityTextMarkerRange -> value -> IO ()
setEndMarker beAccessibilityTextMarkerRange value =
  sendMessage beAccessibilityTextMarkerRange setEndMarkerSelector (toBEAccessibilityTextMarker value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @startMarker@
startMarkerSelector :: Selector '[] (Id BEAccessibilityTextMarker)
startMarkerSelector = mkSelector "startMarker"

-- | @Selector@ for @setStartMarker:@
setStartMarkerSelector :: Selector '[Id BEAccessibilityTextMarker] ()
setStartMarkerSelector = mkSelector "setStartMarker:"

-- | @Selector@ for @endMarker@
endMarkerSelector :: Selector '[] (Id BEAccessibilityTextMarker)
endMarkerSelector = mkSelector "endMarker"

-- | @Selector@ for @setEndMarker:@
setEndMarkerSelector :: Selector '[Id BEAccessibilityTextMarker] ()
setEndMarkerSelector = mkSelector "setEndMarker:"

