{-# LANGUAGE DataKinds #-}
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
  , containsZoomFactorSelector
  , initSelector
  , maxZoomFactorSelector
  , minZoomFactorSelector
  , newSelector


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
init_ :: IsAVZoomRange avZoomRange => avZoomRange -> IO (Id AVZoomRange)
init_ avZoomRange =
  sendOwnedMessage avZoomRange initSelector

-- | @+ new@
new :: IO (Id AVZoomRange)
new  =
  do
    cls' <- getRequiredClass "AVZoomRange"
    sendOwnedClassMessage cls' newSelector

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
containsZoomFactor avZoomRange zoomFactor =
  sendMessage avZoomRange containsZoomFactorSelector zoomFactor

-- | minZoomFactor
--
-- A CGFloat indicating the minimum zoom factor supported by this range.
--
-- ObjC selector: @- minZoomFactor@
minZoomFactor :: IsAVZoomRange avZoomRange => avZoomRange -> IO CDouble
minZoomFactor avZoomRange =
  sendMessage avZoomRange minZoomFactorSelector

-- | maxZoomFactor
--
-- A CGFloat indicating the maximum zoom factor supported by this range.
--
-- ObjC selector: @- maxZoomFactor@
maxZoomFactor :: IsAVZoomRange avZoomRange => avZoomRange -> IO CDouble
maxZoomFactor avZoomRange =
  sendMessage avZoomRange maxZoomFactorSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id AVZoomRange)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id AVZoomRange)
newSelector = mkSelector "new"

-- | @Selector@ for @containsZoomFactor:@
containsZoomFactorSelector :: Selector '[CDouble] Bool
containsZoomFactorSelector = mkSelector "containsZoomFactor:"

-- | @Selector@ for @minZoomFactor@
minZoomFactorSelector :: Selector '[] CDouble
minZoomFactorSelector = mkSelector "minZoomFactor"

-- | @Selector@ for @maxZoomFactor@
maxZoomFactorSelector :: Selector '[] CDouble
maxZoomFactorSelector = mkSelector "maxZoomFactor"

