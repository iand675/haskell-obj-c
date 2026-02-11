{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A request that generates an instance mask of individual people found in the image.
--
-- Generated bindings for @VNGeneratePersonInstanceMaskRequest@.
module ObjC.Vision.VNGeneratePersonInstanceMaskRequest
  ( VNGeneratePersonInstanceMaskRequest
  , IsVNGeneratePersonInstanceMaskRequest(..)
  , results
  , resultsSelector


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

-- | VNObservation results.
--
-- ObjC selector: @- results@
results :: IsVNGeneratePersonInstanceMaskRequest vnGeneratePersonInstanceMaskRequest => vnGeneratePersonInstanceMaskRequest -> IO (Id NSArray)
results vnGeneratePersonInstanceMaskRequest  =
  sendMsg vnGeneratePersonInstanceMaskRequest (mkSelector "results") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @results@
resultsSelector :: Selector
resultsSelector = mkSelector "results"

