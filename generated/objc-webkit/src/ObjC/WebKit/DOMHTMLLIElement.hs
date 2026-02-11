{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @DOMHTMLLIElement@.
module ObjC.WebKit.DOMHTMLLIElement
  ( DOMHTMLLIElement
  , IsDOMHTMLLIElement(..)
  , type_
  , setType
  , value
  , setValue
  , typeSelector
  , setTypeSelector
  , valueSelector
  , setValueSelector


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

import ObjC.WebKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- type@
type_ :: IsDOMHTMLLIElement domhtmlliElement => domhtmlliElement -> IO (Id NSString)
type_ domhtmlliElement  =
  sendMsg domhtmlliElement (mkSelector "type") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setType:@
setType :: (IsDOMHTMLLIElement domhtmlliElement, IsNSString value) => domhtmlliElement -> value -> IO ()
setType domhtmlliElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlliElement (mkSelector "setType:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- value@
value :: IsDOMHTMLLIElement domhtmlliElement => domhtmlliElement -> IO CInt
value domhtmlliElement  =
  sendMsg domhtmlliElement (mkSelector "value") retCInt []

-- | @- setValue:@
setValue :: IsDOMHTMLLIElement domhtmlliElement => domhtmlliElement -> CInt -> IO ()
setValue domhtmlliElement  value =
  sendMsg domhtmlliElement (mkSelector "setValue:") retVoid [argCInt (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @type@
typeSelector :: Selector
typeSelector = mkSelector "type"

-- | @Selector@ for @setType:@
setTypeSelector :: Selector
setTypeSelector = mkSelector "setType:"

-- | @Selector@ for @value@
valueSelector :: Selector
valueSelector = mkSelector "value"

-- | @Selector@ for @setValue:@
setValueSelector :: Selector
setValueSelector = mkSelector "setValue:"

