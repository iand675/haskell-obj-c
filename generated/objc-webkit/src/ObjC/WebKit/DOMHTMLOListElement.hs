{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @DOMHTMLOListElement@.
module ObjC.WebKit.DOMHTMLOListElement
  ( DOMHTMLOListElement
  , IsDOMHTMLOListElement(..)
  , compact
  , setCompact
  , start
  , setStart
  , type_
  , setType
  , compactSelector
  , setCompactSelector
  , startSelector
  , setStartSelector
  , typeSelector
  , setTypeSelector


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

-- | @- compact@
compact :: IsDOMHTMLOListElement domhtmloListElement => domhtmloListElement -> IO Bool
compact domhtmloListElement  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg domhtmloListElement (mkSelector "compact") retCULong []

-- | @- setCompact:@
setCompact :: IsDOMHTMLOListElement domhtmloListElement => domhtmloListElement -> Bool -> IO ()
setCompact domhtmloListElement  value =
  sendMsg domhtmloListElement (mkSelector "setCompact:") retVoid [argCULong (if value then 1 else 0)]

-- | @- start@
start :: IsDOMHTMLOListElement domhtmloListElement => domhtmloListElement -> IO CInt
start domhtmloListElement  =
  sendMsg domhtmloListElement (mkSelector "start") retCInt []

-- | @- setStart:@
setStart :: IsDOMHTMLOListElement domhtmloListElement => domhtmloListElement -> CInt -> IO ()
setStart domhtmloListElement  value =
  sendMsg domhtmloListElement (mkSelector "setStart:") retVoid [argCInt (fromIntegral value)]

-- | @- type@
type_ :: IsDOMHTMLOListElement domhtmloListElement => domhtmloListElement -> IO (Id NSString)
type_ domhtmloListElement  =
  sendMsg domhtmloListElement (mkSelector "type") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setType:@
setType :: (IsDOMHTMLOListElement domhtmloListElement, IsNSString value) => domhtmloListElement -> value -> IO ()
setType domhtmloListElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmloListElement (mkSelector "setType:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @compact@
compactSelector :: Selector
compactSelector = mkSelector "compact"

-- | @Selector@ for @setCompact:@
setCompactSelector :: Selector
setCompactSelector = mkSelector "setCompact:"

-- | @Selector@ for @start@
startSelector :: Selector
startSelector = mkSelector "start"

-- | @Selector@ for @setStart:@
setStartSelector :: Selector
setStartSelector = mkSelector "setStart:"

-- | @Selector@ for @type@
typeSelector :: Selector
typeSelector = mkSelector "type"

-- | @Selector@ for @setType:@
setTypeSelector :: Selector
setTypeSelector = mkSelector "setType:"

