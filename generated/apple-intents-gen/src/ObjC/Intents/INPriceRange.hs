{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INPriceRange@.
module ObjC.Intents.INPriceRange
  ( INPriceRange
  , IsINPriceRange(..)
  , init_
  , initWithRangeBetweenPrice_andPrice_currencyCode
  , initWithMaximumPrice_currencyCode
  , initWithMinimumPrice_currencyCode
  , initWithPrice_currencyCode
  , minimumPrice
  , maximumPrice
  , currencyCode
  , currencyCodeSelector
  , initSelector
  , initWithMaximumPrice_currencyCodeSelector
  , initWithMinimumPrice_currencyCodeSelector
  , initWithPrice_currencyCodeSelector
  , initWithRangeBetweenPrice_andPrice_currencyCodeSelector
  , maximumPriceSelector
  , minimumPriceSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Intents.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsINPriceRange inPriceRange => inPriceRange -> IO (Id INPriceRange)
init_ inPriceRange =
  sendOwnedMessage inPriceRange initSelector

-- | @- initWithRangeBetweenPrice:andPrice:currencyCode:@
initWithRangeBetweenPrice_andPrice_currencyCode :: (IsINPriceRange inPriceRange, IsNSDecimalNumber firstPrice, IsNSDecimalNumber secondPrice, IsNSString currencyCode) => inPriceRange -> firstPrice -> secondPrice -> currencyCode -> IO (Id INPriceRange)
initWithRangeBetweenPrice_andPrice_currencyCode inPriceRange firstPrice secondPrice currencyCode =
  sendOwnedMessage inPriceRange initWithRangeBetweenPrice_andPrice_currencyCodeSelector (toNSDecimalNumber firstPrice) (toNSDecimalNumber secondPrice) (toNSString currencyCode)

-- | @- initWithMaximumPrice:currencyCode:@
initWithMaximumPrice_currencyCode :: (IsINPriceRange inPriceRange, IsNSDecimalNumber maximumPrice, IsNSString currencyCode) => inPriceRange -> maximumPrice -> currencyCode -> IO (Id INPriceRange)
initWithMaximumPrice_currencyCode inPriceRange maximumPrice currencyCode =
  sendOwnedMessage inPriceRange initWithMaximumPrice_currencyCodeSelector (toNSDecimalNumber maximumPrice) (toNSString currencyCode)

-- | @- initWithMinimumPrice:currencyCode:@
initWithMinimumPrice_currencyCode :: (IsINPriceRange inPriceRange, IsNSDecimalNumber minimumPrice, IsNSString currencyCode) => inPriceRange -> minimumPrice -> currencyCode -> IO (Id INPriceRange)
initWithMinimumPrice_currencyCode inPriceRange minimumPrice currencyCode =
  sendOwnedMessage inPriceRange initWithMinimumPrice_currencyCodeSelector (toNSDecimalNumber minimumPrice) (toNSString currencyCode)

-- | @- initWithPrice:currencyCode:@
initWithPrice_currencyCode :: (IsINPriceRange inPriceRange, IsNSDecimalNumber price, IsNSString currencyCode) => inPriceRange -> price -> currencyCode -> IO (Id INPriceRange)
initWithPrice_currencyCode inPriceRange price currencyCode =
  sendOwnedMessage inPriceRange initWithPrice_currencyCodeSelector (toNSDecimalNumber price) (toNSString currencyCode)

-- | @- minimumPrice@
minimumPrice :: IsINPriceRange inPriceRange => inPriceRange -> IO (Id NSDecimalNumber)
minimumPrice inPriceRange =
  sendMessage inPriceRange minimumPriceSelector

-- | @- maximumPrice@
maximumPrice :: IsINPriceRange inPriceRange => inPriceRange -> IO (Id NSDecimalNumber)
maximumPrice inPriceRange =
  sendMessage inPriceRange maximumPriceSelector

-- | @- currencyCode@
currencyCode :: IsINPriceRange inPriceRange => inPriceRange -> IO (Id NSString)
currencyCode inPriceRange =
  sendMessage inPriceRange currencyCodeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id INPriceRange)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithRangeBetweenPrice:andPrice:currencyCode:@
initWithRangeBetweenPrice_andPrice_currencyCodeSelector :: Selector '[Id NSDecimalNumber, Id NSDecimalNumber, Id NSString] (Id INPriceRange)
initWithRangeBetweenPrice_andPrice_currencyCodeSelector = mkSelector "initWithRangeBetweenPrice:andPrice:currencyCode:"

-- | @Selector@ for @initWithMaximumPrice:currencyCode:@
initWithMaximumPrice_currencyCodeSelector :: Selector '[Id NSDecimalNumber, Id NSString] (Id INPriceRange)
initWithMaximumPrice_currencyCodeSelector = mkSelector "initWithMaximumPrice:currencyCode:"

-- | @Selector@ for @initWithMinimumPrice:currencyCode:@
initWithMinimumPrice_currencyCodeSelector :: Selector '[Id NSDecimalNumber, Id NSString] (Id INPriceRange)
initWithMinimumPrice_currencyCodeSelector = mkSelector "initWithMinimumPrice:currencyCode:"

-- | @Selector@ for @initWithPrice:currencyCode:@
initWithPrice_currencyCodeSelector :: Selector '[Id NSDecimalNumber, Id NSString] (Id INPriceRange)
initWithPrice_currencyCodeSelector = mkSelector "initWithPrice:currencyCode:"

-- | @Selector@ for @minimumPrice@
minimumPriceSelector :: Selector '[] (Id NSDecimalNumber)
minimumPriceSelector = mkSelector "minimumPrice"

-- | @Selector@ for @maximumPrice@
maximumPriceSelector :: Selector '[] (Id NSDecimalNumber)
maximumPriceSelector = mkSelector "maximumPrice"

-- | @Selector@ for @currencyCode@
currencyCodeSelector :: Selector '[] (Id NSString)
currencyCodeSelector = mkSelector "currencyCode"

