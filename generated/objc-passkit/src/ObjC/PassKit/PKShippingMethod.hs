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
  , identifierSelector
  , setIdentifierSelector
  , detailSelector
  , setDetailSelector
  , dateComponentsRangeSelector
  , setDateComponentsRangeSelector


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

import ObjC.PassKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- identifier@
identifier :: IsPKShippingMethod pkShippingMethod => pkShippingMethod -> IO (Id NSString)
identifier pkShippingMethod  =
  sendMsg pkShippingMethod (mkSelector "identifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setIdentifier:@
setIdentifier :: (IsPKShippingMethod pkShippingMethod, IsNSString value) => pkShippingMethod -> value -> IO ()
setIdentifier pkShippingMethod  value =
withObjCPtr value $ \raw_value ->
    sendMsg pkShippingMethod (mkSelector "setIdentifier:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- detail@
detail :: IsPKShippingMethod pkShippingMethod => pkShippingMethod -> IO (Id NSString)
detail pkShippingMethod  =
  sendMsg pkShippingMethod (mkSelector "detail") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setDetail:@
setDetail :: (IsPKShippingMethod pkShippingMethod, IsNSString value) => pkShippingMethod -> value -> IO ()
setDetail pkShippingMethod  value =
withObjCPtr value $ \raw_value ->
    sendMsg pkShippingMethod (mkSelector "setDetail:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- dateComponentsRange@
dateComponentsRange :: IsPKShippingMethod pkShippingMethod => pkShippingMethod -> IO (Id PKDateComponentsRange)
dateComponentsRange pkShippingMethod  =
  sendMsg pkShippingMethod (mkSelector "dateComponentsRange") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setDateComponentsRange:@
setDateComponentsRange :: (IsPKShippingMethod pkShippingMethod, IsPKDateComponentsRange value) => pkShippingMethod -> value -> IO ()
setDateComponentsRange pkShippingMethod  value =
withObjCPtr value $ \raw_value ->
    sendMsg pkShippingMethod (mkSelector "setDateComponentsRange:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @identifier@
identifierSelector :: Selector
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @setIdentifier:@
setIdentifierSelector :: Selector
setIdentifierSelector = mkSelector "setIdentifier:"

-- | @Selector@ for @detail@
detailSelector :: Selector
detailSelector = mkSelector "detail"

-- | @Selector@ for @setDetail:@
setDetailSelector :: Selector
setDetailSelector = mkSelector "setDetail:"

-- | @Selector@ for @dateComponentsRange@
dateComponentsRangeSelector :: Selector
dateComponentsRangeSelector = mkSelector "dateComponentsRange"

-- | @Selector@ for @setDateComponentsRange:@
setDateComponentsRangeSelector :: Selector
setDateComponentsRangeSelector = mkSelector "setDateComponentsRange:"

