{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A template for customizing the display of relevant shortcuts.
--
-- INRelevantShortcut
--
-- Generated bindings for @INDefaultCardTemplate@.
module ObjC.Intents.INDefaultCardTemplate
  ( INDefaultCardTemplate
  , IsINDefaultCardTemplate(..)
  , initWithTitle
  , init_
  , title
  , setTitle
  , subtitle
  , setSubtitle
  , image
  , setImage
  , initWithTitleSelector
  , initSelector
  , titleSelector
  , setTitleSelector
  , subtitleSelector
  , setSubtitleSelector
  , imageSelector
  , setImageSelector


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

-- | Initializes a default card template with the provided title.
--
-- ObjC selector: @- initWithTitle:@
initWithTitle :: (IsINDefaultCardTemplate inDefaultCardTemplate, IsNSString title) => inDefaultCardTemplate -> title -> IO (Id INDefaultCardTemplate)
initWithTitle inDefaultCardTemplate  title =
withObjCPtr title $ \raw_title ->
    sendMsg inDefaultCardTemplate (mkSelector "initWithTitle:") (retPtr retVoid) [argPtr (castPtr raw_title :: Ptr ())] >>= ownedObject . castPtr

-- | Note: Must be initilaized with a title, using those initializers.
--
-- ObjC selector: @- init@
init_ :: IsINDefaultCardTemplate inDefaultCardTemplate => inDefaultCardTemplate -> IO (Id INDefaultCardTemplate)
init_ inDefaultCardTemplate  =
  sendMsg inDefaultCardTemplate (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | The title used when displaying the relevant shortcut.
--
-- ObjC selector: @- title@
title :: IsINDefaultCardTemplate inDefaultCardTemplate => inDefaultCardTemplate -> IO (Id NSString)
title inDefaultCardTemplate  =
  sendMsg inDefaultCardTemplate (mkSelector "title") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The title used when displaying the relevant shortcut.
--
-- ObjC selector: @- setTitle:@
setTitle :: (IsINDefaultCardTemplate inDefaultCardTemplate, IsNSString value) => inDefaultCardTemplate -> value -> IO ()
setTitle inDefaultCardTemplate  value =
withObjCPtr value $ \raw_value ->
    sendMsg inDefaultCardTemplate (mkSelector "setTitle:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | The subtitle used when displaying the relevant shortcut.
--
-- ObjC selector: @- subtitle@
subtitle :: IsINDefaultCardTemplate inDefaultCardTemplate => inDefaultCardTemplate -> IO (Id NSString)
subtitle inDefaultCardTemplate  =
  sendMsg inDefaultCardTemplate (mkSelector "subtitle") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The subtitle used when displaying the relevant shortcut.
--
-- ObjC selector: @- setSubtitle:@
setSubtitle :: (IsINDefaultCardTemplate inDefaultCardTemplate, IsNSString value) => inDefaultCardTemplate -> value -> IO ()
setSubtitle inDefaultCardTemplate  value =
withObjCPtr value $ \raw_value ->
    sendMsg inDefaultCardTemplate (mkSelector "setSubtitle:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | The image used when displaying the relevant shortcut.
--
-- ObjC selector: @- image@
image :: IsINDefaultCardTemplate inDefaultCardTemplate => inDefaultCardTemplate -> IO (Id INImage)
image inDefaultCardTemplate  =
  sendMsg inDefaultCardTemplate (mkSelector "image") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The image used when displaying the relevant shortcut.
--
-- ObjC selector: @- setImage:@
setImage :: (IsINDefaultCardTemplate inDefaultCardTemplate, IsINImage value) => inDefaultCardTemplate -> value -> IO ()
setImage inDefaultCardTemplate  value =
withObjCPtr value $ \raw_value ->
    sendMsg inDefaultCardTemplate (mkSelector "setImage:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithTitle:@
initWithTitleSelector :: Selector
initWithTitleSelector = mkSelector "initWithTitle:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @title@
titleSelector :: Selector
titleSelector = mkSelector "title"

-- | @Selector@ for @setTitle:@
setTitleSelector :: Selector
setTitleSelector = mkSelector "setTitle:"

-- | @Selector@ for @subtitle@
subtitleSelector :: Selector
subtitleSelector = mkSelector "subtitle"

-- | @Selector@ for @setSubtitle:@
setSubtitleSelector :: Selector
setSubtitleSelector = mkSelector "setSubtitle:"

-- | @Selector@ for @image@
imageSelector :: Selector
imageSelector = mkSelector "image"

-- | @Selector@ for @setImage:@
setImageSelector :: Selector
setImageSelector = mkSelector "setImage:"

