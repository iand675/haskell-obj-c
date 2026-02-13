{-# LANGUAGE DataKinds #-}
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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.PencilKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithLowerBound:upperBound:@
initWithLowerBound_upperBound :: IsPKFloatRange pkFloatRange => pkFloatRange -> CDouble -> CDouble -> IO (Id PKFloatRange)
initWithLowerBound_upperBound pkFloatRange lowerBound upperBound =
  sendOwnedMessage pkFloatRange initWithLowerBound_upperBoundSelector lowerBound upperBound

-- | @- lowerBound@
lowerBound :: IsPKFloatRange pkFloatRange => pkFloatRange -> IO CDouble
lowerBound pkFloatRange =
  sendMessage pkFloatRange lowerBoundSelector

-- | @- upperBound@
upperBound :: IsPKFloatRange pkFloatRange => pkFloatRange -> IO CDouble
upperBound pkFloatRange =
  sendMessage pkFloatRange upperBoundSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithLowerBound:upperBound:@
initWithLowerBound_upperBoundSelector :: Selector '[CDouble, CDouble] (Id PKFloatRange)
initWithLowerBound_upperBoundSelector = mkSelector "initWithLowerBound:upperBound:"

-- | @Selector@ for @lowerBound@
lowerBoundSelector :: Selector '[] CDouble
lowerBoundSelector = mkSelector "lowerBound"

-- | @Selector@ for @upperBound@
upperBoundSelector :: Selector '[] CDouble
upperBoundSelector = mkSelector "upperBound"

