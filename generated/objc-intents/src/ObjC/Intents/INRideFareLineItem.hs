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
  , initWithTitle_price_currencyCodeSelector
  , initSelector
  , titleSelector
  , priceSelector
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

-- | @- initWithTitle:price:currencyCode:@
initWithTitle_price_currencyCode :: (IsINRideFareLineItem inRideFareLineItem, IsNSString title, IsNSDecimalNumber price, IsNSString currencyCode) => inRideFareLineItem -> title -> price -> currencyCode -> IO (Id INRideFareLineItem)
initWithTitle_price_currencyCode inRideFareLineItem  title price currencyCode =
withObjCPtr title $ \raw_title ->
  withObjCPtr price $ \raw_price ->
    withObjCPtr currencyCode $ \raw_currencyCode ->
        sendMsg inRideFareLineItem (mkSelector "initWithTitle:price:currencyCode:") (retPtr retVoid) [argPtr (castPtr raw_title :: Ptr ()), argPtr (castPtr raw_price :: Ptr ()), argPtr (castPtr raw_currencyCode :: Ptr ())] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsINRideFareLineItem inRideFareLineItem => inRideFareLineItem -> IO (Id INRideFareLineItem)
init_ inRideFareLineItem  =
  sendMsg inRideFareLineItem (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- title@
title :: IsINRideFareLineItem inRideFareLineItem => inRideFareLineItem -> IO (Id NSString)
title inRideFareLineItem  =
  sendMsg inRideFareLineItem (mkSelector "title") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- price@
price :: IsINRideFareLineItem inRideFareLineItem => inRideFareLineItem -> IO (Id NSDecimalNumber)
price inRideFareLineItem  =
  sendMsg inRideFareLineItem (mkSelector "price") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- currencyCode@
currencyCode :: IsINRideFareLineItem inRideFareLineItem => inRideFareLineItem -> IO (Id NSString)
currencyCode inRideFareLineItem  =
  sendMsg inRideFareLineItem (mkSelector "currencyCode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithTitle:price:currencyCode:@
initWithTitle_price_currencyCodeSelector :: Selector
initWithTitle_price_currencyCodeSelector = mkSelector "initWithTitle:price:currencyCode:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @title@
titleSelector :: Selector
titleSelector = mkSelector "title"

-- | @Selector@ for @price@
priceSelector :: Selector
priceSelector = mkSelector "price"

-- | @Selector@ for @currencyCode@
currencyCodeSelector :: Selector
currencyCodeSelector = mkSelector "currencyCode"

