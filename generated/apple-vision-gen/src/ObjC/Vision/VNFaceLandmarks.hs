{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | VNFaceLandmarks
--
-- VNFaceLandmarks2D is the result of a face landmarks request. It is an abstract base class.
--
-- Generated bindings for @VNFaceLandmarks@.
module ObjC.Vision.VNFaceLandmarks
  ( VNFaceLandmarks
  , IsVNFaceLandmarks(..)
  , init_
  , confidence
  , confidenceSelector
  , initSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Vision.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsVNFaceLandmarks vnFaceLandmarks => vnFaceLandmarks -> IO (Id VNFaceLandmarks)
init_ vnFaceLandmarks =
  sendOwnedMessage vnFaceLandmarks initSelector

-- | a confidence estimate for the returned landmarks.
--
-- ObjC selector: @- confidence@
confidence :: IsVNFaceLandmarks vnFaceLandmarks => vnFaceLandmarks -> IO CFloat
confidence vnFaceLandmarks =
  sendMessage vnFaceLandmarks confidenceSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id VNFaceLandmarks)
initSelector = mkSelector "init"

-- | @Selector@ for @confidence@
confidenceSelector :: Selector '[] CFloat
confidenceSelector = mkSelector "confidence"

