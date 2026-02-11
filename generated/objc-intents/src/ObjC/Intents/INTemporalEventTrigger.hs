{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INTemporalEventTrigger@.
module ObjC.Intents.INTemporalEventTrigger
  ( INTemporalEventTrigger
  , IsINTemporalEventTrigger(..)
  , initWithDateComponentsRange
  , dateComponentsRange
  , initWithDateComponentsRangeSelector
  , dateComponentsRangeSelector


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

import ObjC.Intents.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithDateComponentsRange:@
initWithDateComponentsRange :: (IsINTemporalEventTrigger inTemporalEventTrigger, IsINDateComponentsRange dateComponentsRange) => inTemporalEventTrigger -> dateComponentsRange -> IO (Id INTemporalEventTrigger)
initWithDateComponentsRange inTemporalEventTrigger  dateComponentsRange =
withObjCPtr dateComponentsRange $ \raw_dateComponentsRange ->
    sendMsg inTemporalEventTrigger (mkSelector "initWithDateComponentsRange:") (retPtr retVoid) [argPtr (castPtr raw_dateComponentsRange :: Ptr ())] >>= ownedObject . castPtr

-- | @- dateComponentsRange@
dateComponentsRange :: IsINTemporalEventTrigger inTemporalEventTrigger => inTemporalEventTrigger -> IO (Id INDateComponentsRange)
dateComponentsRange inTemporalEventTrigger  =
  sendMsg inTemporalEventTrigger (mkSelector "dateComponentsRange") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDateComponentsRange:@
initWithDateComponentsRangeSelector :: Selector
initWithDateComponentsRangeSelector = mkSelector "initWithDateComponentsRange:"

-- | @Selector@ for @dateComponentsRange@
dateComponentsRangeSelector :: Selector
dateComponentsRangeSelector = mkSelector "dateComponentsRange"

