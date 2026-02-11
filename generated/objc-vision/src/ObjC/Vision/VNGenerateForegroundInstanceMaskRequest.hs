{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A request that generates an instance mask of salient objects that can be separated from the background.
--
-- Generated bindings for @VNGenerateForegroundInstanceMaskRequest@.
module ObjC.Vision.VNGenerateForegroundInstanceMaskRequest
  ( VNGenerateForegroundInstanceMaskRequest
  , IsVNGenerateForegroundInstanceMaskRequest(..)
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
results :: IsVNGenerateForegroundInstanceMaskRequest vnGenerateForegroundInstanceMaskRequest => vnGenerateForegroundInstanceMaskRequest -> IO (Id NSArray)
results vnGenerateForegroundInstanceMaskRequest  =
  sendMsg vnGenerateForegroundInstanceMaskRequest (mkSelector "results") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @results@
resultsSelector :: Selector
resultsSelector = mkSelector "results"

