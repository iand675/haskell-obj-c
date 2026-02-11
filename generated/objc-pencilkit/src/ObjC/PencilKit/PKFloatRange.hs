{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @PKFloatRange@.
module ObjC.PencilKit.PKFloatRange
  ( PKFloatRange
  , IsPKFloatRange(..)
  , initWithLowerBound_upperBound
  , lowerBound
  , upperBound
  , initWithLowerBound_upperBoundSelector
  , lowerBoundSelector
  , upperBoundSelector


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

import ObjC.PencilKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithLowerBound:upperBound:@
initWithLowerBound_upperBound :: IsPKFloatRange pkFloatRange => pkFloatRange -> CDouble -> CDouble -> IO (Id PKFloatRange)
initWithLowerBound_upperBound pkFloatRange  lowerBound upperBound =
  sendMsg pkFloatRange (mkSelector "initWithLowerBound:upperBound:") (retPtr retVoid) [argCDouble (fromIntegral lowerBound), argCDouble (fromIntegral upperBound)] >>= ownedObject . castPtr

-- | @- lowerBound@
lowerBound :: IsPKFloatRange pkFloatRange => pkFloatRange -> IO CDouble
lowerBound pkFloatRange  =
  sendMsg pkFloatRange (mkSelector "lowerBound") retCDouble []

-- | @- upperBound@
upperBound :: IsPKFloatRange pkFloatRange => pkFloatRange -> IO CDouble
upperBound pkFloatRange  =
  sendMsg pkFloatRange (mkSelector "upperBound") retCDouble []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithLowerBound:upperBound:@
initWithLowerBound_upperBoundSelector :: Selector
initWithLowerBound_upperBoundSelector = mkSelector "initWithLowerBound:upperBound:"

-- | @Selector@ for @lowerBound@
lowerBoundSelector :: Selector
lowerBoundSelector = mkSelector "lowerBound"

-- | @Selector@ for @upperBound@
upperBoundSelector :: Selector
upperBoundSelector = mkSelector "upperBound"

