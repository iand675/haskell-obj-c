{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INTemporalEventTrigger@.
module ObjC.Intents.INTemporalEventTrigger
  ( INTemporalEventTrigger
  , IsINTemporalEventTrigger(..)
  , initWithDateComponentsRange
  , dateComponentsRange
  , dateComponentsRangeSelector
  , initWithDateComponentsRangeSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Intents.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithDateComponentsRange:@
initWithDateComponentsRange :: (IsINTemporalEventTrigger inTemporalEventTrigger, IsINDateComponentsRange dateComponentsRange) => inTemporalEventTrigger -> dateComponentsRange -> IO (Id INTemporalEventTrigger)
initWithDateComponentsRange inTemporalEventTrigger dateComponentsRange =
  sendOwnedMessage inTemporalEventTrigger initWithDateComponentsRangeSelector (toINDateComponentsRange dateComponentsRange)

-- | @- dateComponentsRange@
dateComponentsRange :: IsINTemporalEventTrigger inTemporalEventTrigger => inTemporalEventTrigger -> IO (Id INDateComponentsRange)
dateComponentsRange inTemporalEventTrigger =
  sendMessage inTemporalEventTrigger dateComponentsRangeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDateComponentsRange:@
initWithDateComponentsRangeSelector :: Selector '[Id INDateComponentsRange] (Id INTemporalEventTrigger)
initWithDateComponentsRangeSelector = mkSelector "initWithDateComponentsRange:"

-- | @Selector@ for @dateComponentsRange@
dateComponentsRangeSelector :: Selector '[] (Id INDateComponentsRange)
dateComponentsRangeSelector = mkSelector "dateComponentsRange"

