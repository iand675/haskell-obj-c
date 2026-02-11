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

import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsAVMetricMediaRendition avMetricMediaRendition => avMetricMediaRendition -> IO (Id AVMetricMediaRendition)
init_ avMetricMediaRendition  =
  sendMsg avMetricMediaRendition (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id AVMetricMediaRendition)
new  =
  do
    cls' <- getRequiredClass "AVMetricMediaRendition"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | stableID
--
-- Provides ID corresponding to the rendition. This is equivalent to the				STABLE-RENDITION-ID in the HLS playlist. If not available, value is nil.
--
-- ObjC selector: @- stableID@
stableID :: IsAVMetricMediaRendition avMetricMediaRendition => avMetricMediaRendition -> IO (Id NSString)
stableID avMetricMediaRendition  =
  sendMsg avMetricMediaRendition (mkSelector "stableID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | URL
--
-- Provides URL corresponding to the rendition's HLS playlist. If not available,				value is nil.
--
-- ObjC selector: @- URL@
url :: IsAVMetricMediaRendition avMetricMediaRendition => avMetricMediaRendition -> IO (Id NSURL)
url avMetricMediaRendition  =
  sendMsg avMetricMediaRendition (mkSelector "URL") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @stableID@
stableIDSelector :: Selector
stableIDSelector = mkSelector "stableID"

-- | @Selector@ for @URL@
urlSelector :: Selector
urlSelector = mkSelector "URL"

