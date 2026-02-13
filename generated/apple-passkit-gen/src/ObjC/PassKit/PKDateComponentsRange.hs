{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @PKDateComponentsRange@.
module ObjC.PassKit.PKDateComponentsRange
  ( PKDateComponentsRange
  , IsPKDateComponentsRange(..)
  , init_
  , initWithStartDateComponents_endDateComponents
  , startDateComponents
  , endDateComponents
  , endDateComponentsSelector
  , initSelector
  , initWithStartDateComponents_endDateComponentsSelector
  , startDateComponentsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.PassKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsPKDateComponentsRange pkDateComponentsRange => pkDateComponentsRange -> IO (Id PKDateComponentsRange)
init_ pkDateComponentsRange =
  sendOwnedMessage pkDateComponentsRange initSelector

-- | @- initWithStartDateComponents:endDateComponents:@
initWithStartDateComponents_endDateComponents :: (IsPKDateComponentsRange pkDateComponentsRange, IsNSDateComponents startDateComponents, IsNSDateComponents endDateComponents) => pkDateComponentsRange -> startDateComponents -> endDateComponents -> IO (Id PKDateComponentsRange)
initWithStartDateComponents_endDateComponents pkDateComponentsRange startDateComponents endDateComponents =
  sendOwnedMessage pkDateComponentsRange initWithStartDateComponents_endDateComponentsSelector (toNSDateComponents startDateComponents) (toNSDateComponents endDateComponents)

-- | @- startDateComponents@
startDateComponents :: IsPKDateComponentsRange pkDateComponentsRange => pkDateComponentsRange -> IO (Id NSDateComponents)
startDateComponents pkDateComponentsRange =
  sendMessage pkDateComponentsRange startDateComponentsSelector

-- | @- endDateComponents@
endDateComponents :: IsPKDateComponentsRange pkDateComponentsRange => pkDateComponentsRange -> IO (Id NSDateComponents)
endDateComponents pkDateComponentsRange =
  sendMessage pkDateComponentsRange endDateComponentsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id PKDateComponentsRange)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithStartDateComponents:endDateComponents:@
initWithStartDateComponents_endDateComponentsSelector :: Selector '[Id NSDateComponents, Id NSDateComponents] (Id PKDateComponentsRange)
initWithStartDateComponents_endDateComponentsSelector = mkSelector "initWithStartDateComponents:endDateComponents:"

-- | @Selector@ for @startDateComponents@
startDateComponentsSelector :: Selector '[] (Id NSDateComponents)
startDateComponentsSelector = mkSelector "startDateComponents"

-- | @Selector@ for @endDateComponents@
endDateComponentsSelector :: Selector '[] (Id NSDateComponents)
endDateComponentsSelector = mkSelector "endDateComponents"

