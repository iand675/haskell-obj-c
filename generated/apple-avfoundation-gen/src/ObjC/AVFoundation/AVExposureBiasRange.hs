{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVExposureBiasRange
--
-- An AVExposureBiasRange expresses an inclusive range of supported exposure bias values, in EV units.
--
-- This is used by AVCaptureSystemExposureBiasSlider for the range the slider uses.
--
-- Generated bindings for @AVExposureBiasRange@.
module ObjC.AVFoundation.AVExposureBiasRange
  ( AVExposureBiasRange
  , IsAVExposureBiasRange(..)
  , init_
  , new
  , containsExposureBias
  , minExposureBias
  , maxExposureBias
  , containsExposureBiasSelector
  , initSelector
  , maxExposureBiasSelector
  , minExposureBiasSelector
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
init_ :: IsAVExposureBiasRange avExposureBiasRange => avExposureBiasRange -> IO (Id AVExposureBiasRange)
init_ avExposureBiasRange =
  sendOwnedMessage avExposureBiasRange initSelector

-- | @+ new@
new :: IO (Id AVExposureBiasRange)
new  =
  do
    cls' <- getRequiredClass "AVExposureBiasRange"
    sendOwnedClassMessage cls' newSelector

-- | containsExposureBias:
--
-- Tests if a given exposure bias in EV units is within the exposure bias range.
--
-- @exposureBias@ — The exposure bias to test.
--
-- Returns: Returns YES if the given exposure bias is within the exposure bias, NO otherwise.
--
-- Note that the exposure bias ranges are inclusive.
--
-- ObjC selector: @- containsExposureBias:@
containsExposureBias :: IsAVExposureBiasRange avExposureBiasRange => avExposureBiasRange -> CFloat -> IO Bool
containsExposureBias avExposureBiasRange exposureBias =
  sendMessage avExposureBiasRange containsExposureBiasSelector exposureBias

-- | minExposureBias
--
-- A float indicating the minimum exposure bias in EV units supported by this range.
--
-- ObjC selector: @- minExposureBias@
minExposureBias :: IsAVExposureBiasRange avExposureBiasRange => avExposureBiasRange -> IO CFloat
minExposureBias avExposureBiasRange =
  sendMessage avExposureBiasRange minExposureBiasSelector

-- | maxExposureBias
--
-- A float indicating the maximum exposure bias in EV units supported by this range.
--
-- ObjC selector: @- maxExposureBias@
maxExposureBias :: IsAVExposureBiasRange avExposureBiasRange => avExposureBiasRange -> IO CFloat
maxExposureBias avExposureBiasRange =
  sendMessage avExposureBiasRange maxExposureBiasSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id AVExposureBiasRange)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id AVExposureBiasRange)
newSelector = mkSelector "new"

-- | @Selector@ for @containsExposureBias:@
containsExposureBiasSelector :: Selector '[CFloat] Bool
containsExposureBiasSelector = mkSelector "containsExposureBias:"

-- | @Selector@ for @minExposureBias@
minExposureBiasSelector :: Selector '[] CFloat
minExposureBiasSelector = mkSelector "minExposureBias"

-- | @Selector@ for @maxExposureBias@
maxExposureBiasSelector :: Selector '[] CFloat
maxExposureBiasSelector = mkSelector "maxExposureBias"

