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
  , initSelector
  , initWithRangeBetweenPrice_andPrice_currencyCodeSelector
  , initWithMaximumPrice_currencyCodeSelector
  , initWithMinimumPrice_currencyCodeSelector
  , initWithPrice_currencyCodeSelector
  , minimumPriceSelector
  , maximumPriceSelector
  , currencyCodeSelector


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

-- | @- init@
init_ :: IsINPriceRange inPriceRange => inPriceRange -> IO (Id INPriceRange)
init_ inPriceRange  =
  sendMsg inPriceRange (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithRangeBetweenPrice:andPrice:currencyCode:@
initWithRangeBetweenPrice_andPrice_currencyCode :: (IsINPriceRange inPriceRange, IsNSDecimalNumber firstPrice, IsNSDecimalNumber secondPrice, IsNSString currencyCode) => inPriceRange -> firstPrice -> secondPrice -> currencyCode -> IO (Id INPriceRange)
initWithRangeBetweenPrice_andPrice_currencyCode inPriceRange  firstPrice secondPrice currencyCode =
withObjCPtr firstPrice $ \raw_firstPrice ->
  withObjCPtr secondPrice $ \raw_secondPrice ->
    withObjCPtr currencyCode $ \raw_currencyCode ->
        sendMsg inPriceRange (mkSelector "initWithRangeBetweenPrice:andPrice:currencyCode:") (retPtr retVoid) [argPtr (castPtr raw_firstPrice :: Ptr ()), argPtr (castPtr raw_secondPrice :: Ptr ()), argPtr (castPtr raw_currencyCode :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithMaximumPrice:currencyCode:@
initWithMaximumPrice_currencyCode :: (IsINPriceRange inPriceRange, IsNSDecimalNumber maximumPrice, IsNSString currencyCode) => inPriceRange -> maximumPrice -> currencyCode -> IO (Id INPriceRange)
initWithMaximumPrice_currencyCode inPriceRange  maximumPrice currencyCode =
withObjCPtr maximumPrice $ \raw_maximumPrice ->
  withObjCPtr currencyCode $ \raw_currencyCode ->
      sendMsg inPriceRange (mkSelector "initWithMaximumPrice:currencyCode:") (retPtr retVoid) [argPtr (castPtr raw_maximumPrice :: Ptr ()), argPtr (castPtr raw_currencyCode :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithMinimumPrice:currencyCode:@
initWithMinimumPrice_currencyCode :: (IsINPriceRange inPriceRange, IsNSDecimalNumber minimumPrice, IsNSString currencyCode) => inPriceRange -> minimumPrice -> currencyCode -> IO (Id INPriceRange)
initWithMinimumPrice_currencyCode inPriceRange  minimumPrice currencyCode =
withObjCPtr minimumPrice $ \raw_minimumPrice ->
  withObjCPtr currencyCode $ \raw_currencyCode ->
      sendMsg inPriceRange (mkSelector "initWithMinimumPrice:currencyCode:") (retPtr retVoid) [argPtr (castPtr raw_minimumPrice :: Ptr ()), argPtr (castPtr raw_currencyCode :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithPrice:currencyCode:@
initWithPrice_currencyCode :: (IsINPriceRange inPriceRange, IsNSDecimalNumber price, IsNSString currencyCode) => inPriceRange -> price -> currencyCode -> IO (Id INPriceRange)
initWithPrice_currencyCode inPriceRange  price currencyCode =
withObjCPtr price $ \raw_price ->
  withObjCPtr currencyCode $ \raw_currencyCode ->
      sendMsg inPriceRange (mkSelector "initWithPrice:currencyCode:") (retPtr retVoid) [argPtr (castPtr raw_price :: Ptr ()), argPtr (castPtr raw_currencyCode :: Ptr ())] >>= ownedObject . castPtr

-- | @- minimumPrice@
minimumPrice :: IsINPriceRange inPriceRange => inPriceRange -> IO (Id NSDecimalNumber)
minimumPrice inPriceRange  =
  sendMsg inPriceRange (mkSelector "minimumPrice") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- maximumPrice@
maximumPrice :: IsINPriceRange inPriceRange => inPriceRange -> IO (Id NSDecimalNumber)
maximumPrice inPriceRange  =
  sendMsg inPriceRange (mkSelector "maximumPrice") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- currencyCode@
currencyCode :: IsINPriceRange inPriceRange => inPriceRange -> IO (Id NSString)
currencyCode inPriceRange  =
  sendMsg inPriceRange (mkSelector "currencyCode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithRangeBetweenPrice:andPrice:currencyCode:@
initWithRangeBetweenPrice_andPrice_currencyCodeSelector :: Selector
initWithRangeBetweenPrice_andPrice_currencyCodeSelector = mkSelector "initWithRangeBetweenPrice:andPrice:currencyCode:"

-- | @Selector@ for @initWithMaximumPrice:currencyCode:@
initWithMaximumPrice_currencyCodeSelector :: Selector
initWithMaximumPrice_currencyCodeSelector = mkSelector "initWithMaximumPrice:currencyCode:"

-- | @Selector@ for @initWithMinimumPrice:currencyCode:@
initWithMinimumPrice_currencyCodeSelector :: Selector
initWithMinimumPrice_currencyCodeSelector = mkSelector "initWithMinimumPrice:currencyCode:"

-- | @Selector@ for @initWithPrice:currencyCode:@
initWithPrice_currencyCodeSelector :: Selector
initWithPrice_currencyCodeSelector = mkSelector "initWithPrice:currencyCode:"

-- | @Selector@ for @minimumPrice@
minimumPriceSelector :: Selector
minimumPriceSelector = mkSelector "minimumPrice"

-- | @Selector@ for @maximumPrice@
maximumPriceSelector :: Selector
maximumPriceSelector = mkSelector "maximumPrice"

-- | @Selector@ for @currencyCode@
currencyCodeSelector :: Selector
currencyCodeSelector = mkSelector "currencyCode"

