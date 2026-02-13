{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @AVMetricMediaRendition@.
module ObjC.AVFoundation.AVMetricMediaRendition
  ( AVMetricMediaRendition
  , IsAVMetricMediaRendition(..)
  , init_
  , new
  , stableID
  , url
  , initSelector
  , newSelector
  , stableIDSelector
  , urlSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsAVMetricMediaRendition avMetricMediaRendition => avMetricMediaRendition -> IO (Id AVMetricMediaRendition)
init_ avMetricMediaRendition =
  sendOwnedMessage avMetricMediaRendition initSelector

-- | @+ new@
new :: IO (Id AVMetricMediaRendition)
new  =
  do
    cls' <- getRequiredClass "AVMetricMediaRendition"
    sendOwnedClassMessage cls' newSelector

-- | stableID
--
-- Provides ID corresponding to the rendition. This is equivalent to the				STABLE-RENDITION-ID in the HLS playlist. If not available, value is nil.
--
-- ObjC selector: @- stableID@
stableID :: IsAVMetricMediaRendition avMetricMediaRendition => avMetricMediaRendition -> IO (Id NSString)
stableID avMetricMediaRendition =
  sendMessage avMetricMediaRendition stableIDSelector

-- | URL
--
-- Provides URL corresponding to the rendition's HLS playlist. If not available,				value is nil.
--
-- ObjC selector: @- URL@
url :: IsAVMetricMediaRendition avMetricMediaRendition => avMetricMediaRendition -> IO (Id NSURL)
url avMetricMediaRendition =
  sendMessage avMetricMediaRendition urlSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id AVMetricMediaRendition)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id AVMetricMediaRendition)
newSelector = mkSelector "new"

-- | @Selector@ for @stableID@
stableIDSelector :: Selector '[] (Id NSString)
stableIDSelector = mkSelector "stableID"

-- | @Selector@ for @URL@
urlSelector :: Selector '[] (Id NSURL)
urlSelector = mkSelector "URL"

