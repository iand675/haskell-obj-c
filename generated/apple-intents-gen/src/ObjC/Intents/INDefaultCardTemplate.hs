{-# LANGUAGE DataKinds #-}
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
  , imageSelector
  , initSelector
  , initWithTitleSelector
  , setImageSelector
  , setSubtitleSelector
  , setTitleSelector
  , subtitleSelector
  , titleSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Intents.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Initializes a default card template with the provided title.
--
-- ObjC selector: @- initWithTitle:@
initWithTitle :: (IsINDefaultCardTemplate inDefaultCardTemplate, IsNSString title) => inDefaultCardTemplate -> title -> IO (Id INDefaultCardTemplate)
initWithTitle inDefaultCardTemplate title =
  sendOwnedMessage inDefaultCardTemplate initWithTitleSelector (toNSString title)

-- | Note: Must be initilaized with a title, using those initializers.
--
-- ObjC selector: @- init@
init_ :: IsINDefaultCardTemplate inDefaultCardTemplate => inDefaultCardTemplate -> IO (Id INDefaultCardTemplate)
init_ inDefaultCardTemplate =
  sendOwnedMessage inDefaultCardTemplate initSelector

-- | The title used when displaying the relevant shortcut.
--
-- ObjC selector: @- title@
title :: IsINDefaultCardTemplate inDefaultCardTemplate => inDefaultCardTemplate -> IO (Id NSString)
title inDefaultCardTemplate =
  sendMessage inDefaultCardTemplate titleSelector

-- | The title used when displaying the relevant shortcut.
--
-- ObjC selector: @- setTitle:@
setTitle :: (IsINDefaultCardTemplate inDefaultCardTemplate, IsNSString value) => inDefaultCardTemplate -> value -> IO ()
setTitle inDefaultCardTemplate value =
  sendMessage inDefaultCardTemplate setTitleSelector (toNSString value)

-- | The subtitle used when displaying the relevant shortcut.
--
-- ObjC selector: @- subtitle@
subtitle :: IsINDefaultCardTemplate inDefaultCardTemplate => inDefaultCardTemplate -> IO (Id NSString)
subtitle inDefaultCardTemplate =
  sendMessage inDefaultCardTemplate subtitleSelector

-- | The subtitle used when displaying the relevant shortcut.
--
-- ObjC selector: @- setSubtitle:@
setSubtitle :: (IsINDefaultCardTemplate inDefaultCardTemplate, IsNSString value) => inDefaultCardTemplate -> value -> IO ()
setSubtitle inDefaultCardTemplate value =
  sendMessage inDefaultCardTemplate setSubtitleSelector (toNSString value)

-- | The image used when displaying the relevant shortcut.
--
-- ObjC selector: @- image@
image :: IsINDefaultCardTemplate inDefaultCardTemplate => inDefaultCardTemplate -> IO (Id INImage)
image inDefaultCardTemplate =
  sendMessage inDefaultCardTemplate imageSelector

-- | The image used when displaying the relevant shortcut.
--
-- ObjC selector: @- setImage:@
setImage :: (IsINDefaultCardTemplate inDefaultCardTemplate, IsINImage value) => inDefaultCardTemplate -> value -> IO ()
setImage inDefaultCardTemplate value =
  sendMessage inDefaultCardTemplate setImageSelector (toINImage value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithTitle:@
initWithTitleSelector :: Selector '[Id NSString] (Id INDefaultCardTemplate)
initWithTitleSelector = mkSelector "initWithTitle:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id INDefaultCardTemplate)
initSelector = mkSelector "init"

-- | @Selector@ for @title@
titleSelector :: Selector '[] (Id NSString)
titleSelector = mkSelector "title"

-- | @Selector@ for @setTitle:@
setTitleSelector :: Selector '[Id NSString] ()
setTitleSelector = mkSelector "setTitle:"

-- | @Selector@ for @subtitle@
subtitleSelector :: Selector '[] (Id NSString)
subtitleSelector = mkSelector "subtitle"

-- | @Selector@ for @setSubtitle:@
setSubtitleSelector :: Selector '[Id NSString] ()
setSubtitleSelector = mkSelector "setSubtitle:"

-- | @Selector@ for @image@
imageSelector :: Selector '[] (Id INImage)
imageSelector = mkSelector "image"

-- | @Selector@ for @setImage:@
setImageSelector :: Selector '[Id INImage] ()
setImageSelector = mkSelector "setImage:"

