{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @PKShippingMethod@.
module ObjC.PassKit.PKShippingMethod
  ( PKShippingMethod
  , IsPKShippingMethod(..)
  , identifier
  , setIdentifier
  , detail
  , setDetail
  , dateComponentsRange
  , setDateComponentsRange
  , dateComponentsRangeSelector
  , detailSelector
  , identifierSelector
  , setDateComponentsRangeSelector
  , setDetailSelector
  , setIdentifierSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.PassKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- identifier@
identifier :: IsPKShippingMethod pkShippingMethod => pkShippingMethod -> IO (Id NSString)
identifier pkShippingMethod =
  sendMessage pkShippingMethod identifierSelector

-- | @- setIdentifier:@
setIdentifier :: (IsPKShippingMethod pkShippingMethod, IsNSString value) => pkShippingMethod -> value -> IO ()
setIdentifier pkShippingMethod value =
  sendMessage pkShippingMethod setIdentifierSelector (toNSString value)

-- | @- detail@
detail :: IsPKShippingMethod pkShippingMethod => pkShippingMethod -> IO (Id NSString)
detail pkShippingMethod =
  sendMessage pkShippingMethod detailSelector

-- | @- setDetail:@
setDetail :: (IsPKShippingMethod pkShippingMethod, IsNSString value) => pkShippingMethod -> value -> IO ()
setDetail pkShippingMethod value =
  sendMessage pkShippingMethod setDetailSelector (toNSString value)

-- | @- dateComponentsRange@
dateComponentsRange :: IsPKShippingMethod pkShippingMethod => pkShippingMethod -> IO (Id PKDateComponentsRange)
dateComponentsRange pkShippingMethod =
  sendMessage pkShippingMethod dateComponentsRangeSelector

-- | @- setDateComponentsRange:@
setDateComponentsRange :: (IsPKShippingMethod pkShippingMethod, IsPKDateComponentsRange value) => pkShippingMethod -> value -> IO ()
setDateComponentsRange pkShippingMethod value =
  sendMessage pkShippingMethod setDateComponentsRangeSelector (toPKDateComponentsRange value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @identifier@
identifierSelector :: Selector '[] (Id NSString)
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @setIdentifier:@
setIdentifierSelector :: Selector '[Id NSString] ()
setIdentifierSelector = mkSelector "setIdentifier:"

-- | @Selector@ for @detail@
detailSelector :: Selector '[] (Id NSString)
detailSelector = mkSelector "detail"

-- | @Selector@ for @setDetail:@
setDetailSelector :: Selector '[Id NSString] ()
setDetailSelector = mkSelector "setDetail:"

-- | @Selector@ for @dateComponentsRange@
dateComponentsRangeSelector :: Selector '[] (Id PKDateComponentsRange)
dateComponentsRangeSelector = mkSelector "dateComponentsRange"

-- | @Selector@ for @setDateComponentsRange:@
setDateComponentsRangeSelector :: Selector '[Id PKDateComponentsRange] ()
setDateComponentsRangeSelector = mkSelector "setDateComponentsRange:"

