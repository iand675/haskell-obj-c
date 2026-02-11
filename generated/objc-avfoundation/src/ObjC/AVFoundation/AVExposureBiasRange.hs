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
  , initSelector
  , newSelector
  , containsExposureBiasSelector
  , minExposureBiasSelector
  , maxExposureBiasSelector


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
init_ :: IsAVExposureBiasRange avExposureBiasRange => avExposureBiasRange -> IO (Id AVExposureBiasRange)
init_ avExposureBiasRange  =
  sendMsg avExposureBiasRange (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id AVExposureBiasRange)
new  =
  do
    cls' <- getRequiredClass "AVExposureBiasRange"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

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
containsExposureBias avExposureBiasRange  exposureBias =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avExposureBiasRange (mkSelector "containsExposureBias:") retCULong [argCFloat (fromIntegral exposureBias)]

-- | minExposureBias
--
-- A float indicating the minimum exposure bias in EV units supported by this range.
--
-- ObjC selector: @- minExposureBias@
minExposureBias :: IsAVExposureBiasRange avExposureBiasRange => avExposureBiasRange -> IO CFloat
minExposureBias avExposureBiasRange  =
  sendMsg avExposureBiasRange (mkSelector "minExposureBias") retCFloat []

-- | maxExposureBias
--
-- A float indicating the maximum exposure bias in EV units supported by this range.
--
-- ObjC selector: @- maxExposureBias@
maxExposureBias :: IsAVExposureBiasRange avExposureBiasRange => avExposureBiasRange -> IO CFloat
maxExposureBias avExposureBiasRange  =
  sendMsg avExposureBiasRange (mkSelector "maxExposureBias") retCFloat []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @containsExposureBias:@
containsExposureBiasSelector :: Selector
containsExposureBiasSelector = mkSelector "containsExposureBias:"

-- | @Selector@ for @minExposureBias@
minExposureBiasSelector :: Selector
minExposureBiasSelector = mkSelector "minExposureBias"

-- | @Selector@ for @maxExposureBias@
maxExposureBiasSelector :: Selector
maxExposureBiasSelector = mkSelector "maxExposureBias"

