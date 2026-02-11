{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A relevance provider to indicate relevance at a date or date interval.
--
-- Generated bindings for @INDateRelevanceProvider@.
module ObjC.Intents.INDateRelevanceProvider
  ( INDateRelevanceProvider
  , IsINDateRelevanceProvider(..)
  , initWithStartDate_endDate
  , startDate
  , endDate
  , initWithStartDate_endDateSelector
  , startDateSelector
  , endDateSelector


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

-- | Initializes a date relevance provider with the specified relevant date interval.
--
-- ObjC selector: @- initWithStartDate:endDate:@
initWithStartDate_endDate :: (IsINDateRelevanceProvider inDateRelevanceProvider, IsNSDate startDate, IsNSDate endDate) => inDateRelevanceProvider -> startDate -> endDate -> IO (Id INDateRelevanceProvider)
initWithStartDate_endDate inDateRelevanceProvider  startDate endDate =
withObjCPtr startDate $ \raw_startDate ->
  withObjCPtr endDate $ \raw_endDate ->
      sendMsg inDateRelevanceProvider (mkSelector "initWithStartDate:endDate:") (retPtr retVoid) [argPtr (castPtr raw_startDate :: Ptr ()), argPtr (castPtr raw_endDate :: Ptr ())] >>= ownedObject . castPtr

-- | The start date of the relevant time interval.
--
-- ObjC selector: @- startDate@
startDate :: IsINDateRelevanceProvider inDateRelevanceProvider => inDateRelevanceProvider -> IO (Id NSDate)
startDate inDateRelevanceProvider  =
  sendMsg inDateRelevanceProvider (mkSelector "startDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The end date of the relevant time interval.
--
-- Note: If @endDate@ is @nil,@ the relevant time interval will be assumed to represent a single point in time instead of a time interval.
--
-- ObjC selector: @- endDate@
endDate :: IsINDateRelevanceProvider inDateRelevanceProvider => inDateRelevanceProvider -> IO (Id NSDate)
endDate inDateRelevanceProvider  =
  sendMsg inDateRelevanceProvider (mkSelector "endDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithStartDate:endDate:@
initWithStartDate_endDateSelector :: Selector
initWithStartDate_endDateSelector = mkSelector "initWithStartDate:endDate:"

-- | @Selector@ for @startDate@
startDateSelector :: Selector
startDateSelector = mkSelector "startDate"

-- | @Selector@ for @endDate@
endDateSelector :: Selector
endDateSelector = mkSelector "endDate"

