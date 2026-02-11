{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @DOMAttr@.
module ObjC.WebKit.DOMAttr
  ( DOMAttr
  , IsDOMAttr(..)
  , name
  , specified
  , value
  , setValue
  , ownerElement
  , style
  , nameSelector
  , specifiedSelector
  , valueSelector
  , setValueSelector
  , ownerElementSelector
  , styleSelector


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

-- | @- name@
name :: IsDOMAttr domAttr => domAttr -> IO (Id NSString)
name domAttr  =
  sendMsg domAttr (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- specified@
specified :: IsDOMAttr domAttr => domAttr -> IO Bool
specified domAttr  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg domAttr (mkSelector "specified") retCULong []

-- | @- value@
value :: IsDOMAttr domAttr => domAttr -> IO (Id NSString)
value domAttr  =
  sendMsg domAttr (mkSelector "value") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setValue:@
setValue :: (IsDOMAttr domAttr, IsNSString value) => domAttr -> value -> IO ()
setValue domAttr  value =
withObjCPtr value $ \raw_value ->
    sendMsg domAttr (mkSelector "setValue:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- ownerElement@
ownerElement :: IsDOMAttr domAttr => domAttr -> IO (Id DOMElement)
ownerElement domAttr  =
  sendMsg domAttr (mkSelector "ownerElement") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- style@
style :: IsDOMAttr domAttr => domAttr -> IO (Id DOMCSSStyleDeclaration)
style domAttr  =
  sendMsg domAttr (mkSelector "style") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

-- | @Selector@ for @specified@
specifiedSelector :: Selector
specifiedSelector = mkSelector "specified"

-- | @Selector@ for @value@
valueSelector :: Selector
valueSelector = mkSelector "value"

-- | @Selector@ for @setValue:@
setValueSelector :: Selector
setValueSelector = mkSelector "setValue:"

-- | @Selector@ for @ownerElement@
ownerElementSelector :: Selector
ownerElementSelector = mkSelector "ownerElement"

-- | @Selector@ for @style@
styleSelector :: Selector
styleSelector = mkSelector "style"

