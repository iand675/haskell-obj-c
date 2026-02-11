{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @DOMHTMLStyleElement@.
module ObjC.WebKit.DOMHTMLStyleElement
  ( DOMHTMLStyleElement
  , IsDOMHTMLStyleElement(..)
  , disabled
  , setDisabled
  , media
  , setMedia
  , type_
  , setType
  , sheet
  , disabledSelector
  , setDisabledSelector
  , mediaSelector
  , setMediaSelector
  , typeSelector
  , setTypeSelector
  , sheetSelector


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

-- | @- disabled@
disabled :: IsDOMHTMLStyleElement domhtmlStyleElement => domhtmlStyleElement -> IO Bool
disabled domhtmlStyleElement  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg domhtmlStyleElement (mkSelector "disabled") retCULong []

-- | @- setDisabled:@
setDisabled :: IsDOMHTMLStyleElement domhtmlStyleElement => domhtmlStyleElement -> Bool -> IO ()
setDisabled domhtmlStyleElement  value =
  sendMsg domhtmlStyleElement (mkSelector "setDisabled:") retVoid [argCULong (if value then 1 else 0)]

-- | @- media@
media :: IsDOMHTMLStyleElement domhtmlStyleElement => domhtmlStyleElement -> IO (Id NSString)
media domhtmlStyleElement  =
  sendMsg domhtmlStyleElement (mkSelector "media") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMedia:@
setMedia :: (IsDOMHTMLStyleElement domhtmlStyleElement, IsNSString value) => domhtmlStyleElement -> value -> IO ()
setMedia domhtmlStyleElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlStyleElement (mkSelector "setMedia:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- type@
type_ :: IsDOMHTMLStyleElement domhtmlStyleElement => domhtmlStyleElement -> IO (Id NSString)
type_ domhtmlStyleElement  =
  sendMsg domhtmlStyleElement (mkSelector "type") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setType:@
setType :: (IsDOMHTMLStyleElement domhtmlStyleElement, IsNSString value) => domhtmlStyleElement -> value -> IO ()
setType domhtmlStyleElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlStyleElement (mkSelector "setType:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- sheet@
sheet :: IsDOMHTMLStyleElement domhtmlStyleElement => domhtmlStyleElement -> IO (Id DOMStyleSheet)
sheet domhtmlStyleElement  =
  sendMsg domhtmlStyleElement (mkSelector "sheet") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @disabled@
disabledSelector :: Selector
disabledSelector = mkSelector "disabled"

-- | @Selector@ for @setDisabled:@
setDisabledSelector :: Selector
setDisabledSelector = mkSelector "setDisabled:"

-- | @Selector@ for @media@
mediaSelector :: Selector
mediaSelector = mkSelector "media"

-- | @Selector@ for @setMedia:@
setMediaSelector :: Selector
setMediaSelector = mkSelector "setMedia:"

-- | @Selector@ for @type@
typeSelector :: Selector
typeSelector = mkSelector "type"

-- | @Selector@ for @setType:@
setTypeSelector :: Selector
setTypeSelector = mkSelector "setType:"

-- | @Selector@ for @sheet@
sheetSelector :: Selector
sheetSelector = mkSelector "sheet"

