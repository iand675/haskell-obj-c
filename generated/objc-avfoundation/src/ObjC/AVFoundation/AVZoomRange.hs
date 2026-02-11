{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVZoomRange
--
-- An AVZoomRange expresses an inclusive range of supported zoom factors.
--
-- This is used by features that have requirements on zoom factors falling within certain ranges.
--
-- Generated bindings for @AVZoomRange@.
module ObjC.AVFoundation.AVZoomRange
  ( AVZoomRange
  , IsAVZoomRange(..)
  , init_
  , new
  , containsZoomFactor
  , minZoomFactor
  , maxZoomFactor
  , initSelector
  , newSelector
  , containsZoomFactorSelector
  , minZoomFactorSelector
  , maxZoomFactorSelector


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
init_ :: IsAVZoomRange avZoomRange => avZoomRange -> IO (Id AVZoomRange)
init_ avZoomRange  =
  sendMsg avZoomRange (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id AVZoomRange)
new  =
  do
    cls' <- getRequiredClass "AVZoomRange"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | containsZoomFactor:
--
-- Tests if a given zoom factor is within the zoom range.
--
-- @zoomFactor@ — The zoom factor to test.
--
-- Returns: Returns YES if the given zoom factor is within the zoom range, NO otherwise.
--
-- Note that the zoom ranges are inclusive.
--
-- ObjC selector: @- containsZoomFactor:@
containsZoomFactor :: IsAVZoomRange avZoomRange => avZoomRange -> CDouble -> IO Bool
containsZoomFactor avZoomRange  zoomFactor =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avZoomRange (mkSelector "containsZoomFactor:") retCULong [argCDouble (fromIntegral zoomFactor)]

-- | minZoomFactor
--
-- A CGFloat indicating the minimum zoom factor supported by this range.
--
-- ObjC selector: @- minZoomFactor@
minZoomFactor :: IsAVZoomRange avZoomRange => avZoomRange -> IO CDouble
minZoomFactor avZoomRange  =
  sendMsg avZoomRange (mkSelector "minZoomFactor") retCDouble []

-- | maxZoomFactor
--
-- A CGFloat indicating the maximum zoom factor supported by this range.
--
-- ObjC selector: @- maxZoomFactor@
maxZoomFactor :: IsAVZoomRange avZoomRange => avZoomRange -> IO CDouble
maxZoomFactor avZoomRange  =
  sendMsg avZoomRange (mkSelector "maxZoomFactor") retCDouble []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @containsZoomFactor:@
containsZoomFactorSelector :: Selector
containsZoomFactorSelector = mkSelector "containsZoomFactor:"

-- | @Selector@ for @minZoomFactor@
minZoomFactorSelector :: Selector
minZoomFactorSelector = mkSelector "minZoomFactor"

-- | @Selector@ for @maxZoomFactor@
maxZoomFactorSelector :: Selector
maxZoomFactorSelector = mkSelector "maxZoomFactor"

