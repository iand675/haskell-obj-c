{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @DOMHTMLUListElement@.
module ObjC.WebKit.DOMHTMLUListElement
  ( DOMHTMLUListElement
  , IsDOMHTMLUListElement(..)
  , compact
  , setCompact
  , type_
  , setType
  , compactSelector
  , setCompactSelector
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
compact :: IsDOMHTMLUListElement domhtmluListElement => domhtmluListElement -> IO Bool
compact domhtmluListElement  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg domhtmluListElement (mkSelector "compact") retCULong []

-- | @- setCompact:@
setCompact :: IsDOMHTMLUListElement domhtmluListElement => domhtmluListElement -> Bool -> IO ()
setCompact domhtmluListElement  value =
  sendMsg domhtmluListElement (mkSelector "setCompact:") retVoid [argCULong (if value then 1 else 0)]

-- | @- type@
type_ :: IsDOMHTMLUListElement domhtmluListElement => domhtmluListElement -> IO (Id NSString)
type_ domhtmluListElement  =
  sendMsg domhtmluListElement (mkSelector "type") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setType:@
setType :: (IsDOMHTMLUListElement domhtmluListElement, IsNSString value) => domhtmluListElement -> value -> IO ()
setType domhtmluListElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmluListElement (mkSelector "setType:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @compact@
compactSelector :: Selector
compactSelector = mkSelector "compact"

-- | @Selector@ for @setCompact:@
setCompactSelector :: Selector
setCompactSelector = mkSelector "setCompact:"

-- | @Selector@ for @type@
typeSelector :: Selector
typeSelector = mkSelector "type"

-- | @Selector@ for @setType:@
setTypeSelector :: Selector
setTypeSelector = mkSelector "setType:"

