{-# LANGUAGE DataKinds #-}
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
  , mediaSelector
  , setDisabledSelector
  , setMediaSelector
  , setTypeSelector
  , sheetSelector
  , typeSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.WebKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- disabled@
disabled :: IsDOMHTMLStyleElement domhtmlStyleElement => domhtmlStyleElement -> IO Bool
disabled domhtmlStyleElement =
  sendMessage domhtmlStyleElement disabledSelector

-- | @- setDisabled:@
setDisabled :: IsDOMHTMLStyleElement domhtmlStyleElement => domhtmlStyleElement -> Bool -> IO ()
setDisabled domhtmlStyleElement value =
  sendMessage domhtmlStyleElement setDisabledSelector value

-- | @- media@
media :: IsDOMHTMLStyleElement domhtmlStyleElement => domhtmlStyleElement -> IO (Id NSString)
media domhtmlStyleElement =
  sendMessage domhtmlStyleElement mediaSelector

-- | @- setMedia:@
setMedia :: (IsDOMHTMLStyleElement domhtmlStyleElement, IsNSString value) => domhtmlStyleElement -> value -> IO ()
setMedia domhtmlStyleElement value =
  sendMessage domhtmlStyleElement setMediaSelector (toNSString value)

-- | @- type@
type_ :: IsDOMHTMLStyleElement domhtmlStyleElement => domhtmlStyleElement -> IO (Id NSString)
type_ domhtmlStyleElement =
  sendMessage domhtmlStyleElement typeSelector

-- | @- setType:@
setType :: (IsDOMHTMLStyleElement domhtmlStyleElement, IsNSString value) => domhtmlStyleElement -> value -> IO ()
setType domhtmlStyleElement value =
  sendMessage domhtmlStyleElement setTypeSelector (toNSString value)

-- | @- sheet@
sheet :: IsDOMHTMLStyleElement domhtmlStyleElement => domhtmlStyleElement -> IO (Id DOMStyleSheet)
sheet domhtmlStyleElement =
  sendMessage domhtmlStyleElement sheetSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @disabled@
disabledSelector :: Selector '[] Bool
disabledSelector = mkSelector "disabled"

-- | @Selector@ for @setDisabled:@
setDisabledSelector :: Selector '[Bool] ()
setDisabledSelector = mkSelector "setDisabled:"

-- | @Selector@ for @media@
mediaSelector :: Selector '[] (Id NSString)
mediaSelector = mkSelector "media"

-- | @Selector@ for @setMedia:@
setMediaSelector :: Selector '[Id NSString] ()
setMediaSelector = mkSelector "setMedia:"

-- | @Selector@ for @type@
typeSelector :: Selector '[] (Id NSString)
typeSelector = mkSelector "type"

-- | @Selector@ for @setType:@
setTypeSelector :: Selector '[Id NSString] ()
setTypeSelector = mkSelector "setType:"

-- | @Selector@ for @sheet@
sheetSelector :: Selector '[] (Id DOMStyleSheet)
sheetSelector = mkSelector "sheet"

