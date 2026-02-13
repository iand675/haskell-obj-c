{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INRideFareLineItem@.
module ObjC.Intents.INRideFareLineItem
  ( INRideFareLineItem
  , IsINRideFareLineItem(..)
  , initWithTitle_price_currencyCode
  , init_
  , title
  , price
  , currencyCode
  , currencyCodeSelector
  , initSelector
  , initWithTitle_price_currencyCodeSelector
  , priceSelector
  , titleSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Intents.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithTitle:price:currencyCode:@
initWithTitle_price_currencyCode :: (IsINRideFareLineItem inRideFareLineItem, IsNSString title, IsNSDecimalNumber price, IsNSString currencyCode) => inRideFareLineItem -> title -> price -> currencyCode -> IO (Id INRideFareLineItem)
initWithTitle_price_currencyCode inRideFareLineItem title price currencyCode =
  sendOwnedMessage inRideFareLineItem initWithTitle_price_currencyCodeSelector (toNSString title) (toNSDecimalNumber price) (toNSString currencyCode)

-- | @- init@
init_ :: IsINRideFareLineItem inRideFareLineItem => inRideFareLineItem -> IO (Id INRideFareLineItem)
init_ inRideFareLineItem =
  sendOwnedMessage inRideFareLineItem initSelector

-- | @- title@
title :: IsINRideFareLineItem inRideFareLineItem => inRideFareLineItem -> IO (Id NSString)
title inRideFareLineItem =
  sendMessage inRideFareLineItem titleSelector

-- | @- price@
price :: IsINRideFareLineItem inRideFareLineItem => inRideFareLineItem -> IO (Id NSDecimalNumber)
price inRideFareLineItem =
  sendMessage inRideFareLineItem priceSelector

-- | @- currencyCode@
currencyCode :: IsINRideFareLineItem inRideFareLineItem => inRideFareLineItem -> IO (Id NSString)
currencyCode inRideFareLineItem =
  sendMessage inRideFareLineItem currencyCodeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithTitle:price:currencyCode:@
initWithTitle_price_currencyCodeSelector :: Selector '[Id NSString, Id NSDecimalNumber, Id NSString] (Id INRideFareLineItem)
initWithTitle_price_currencyCodeSelector = mkSelector "initWithTitle:price:currencyCode:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id INRideFareLineItem)
initSelector = mkSelector "init"

-- | @Selector@ for @title@
titleSelector :: Selector '[] (Id NSString)
titleSelector = mkSelector "title"

-- | @Selector@ for @price@
priceSelector :: Selector '[] (Id NSDecimalNumber)
priceSelector = mkSelector "price"

-- | @Selector@ for @currencyCode@
currencyCodeSelector :: Selector '[] (Id NSString)
currencyCodeSelector = mkSelector "currencyCode"

