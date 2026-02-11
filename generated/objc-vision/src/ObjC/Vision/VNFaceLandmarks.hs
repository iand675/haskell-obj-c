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
  , initSelector
  , confidenceSelector


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

import ObjC.Vision.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsVNFaceLandmarks vnFaceLandmarks => vnFaceLandmarks -> IO (Id VNFaceLandmarks)
init_ vnFaceLandmarks  =
  sendMsg vnFaceLandmarks (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | a confidence estimate for the returned landmarks.
--
-- ObjC selector: @- confidence@
confidence :: IsVNFaceLandmarks vnFaceLandmarks => vnFaceLandmarks -> IO CFloat
confidence vnFaceLandmarks  =
  sendMsg vnFaceLandmarks (mkSelector "confidence") retCFloat []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @confidence@
confidenceSelector :: Selector
confidenceSelector = mkSelector "confidence"

