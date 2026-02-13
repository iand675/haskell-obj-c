{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSTableViewRowAction@.
module ObjC.AppKit.NSTableViewRowAction
  ( NSTableViewRowAction
  , IsNSTableViewRowAction(..)
  , rowActionWithStyle_title_handler
  , style
  , title
  , setTitle
  , backgroundColor
  , setBackgroundColor
  , image
  , setImage
  , backgroundColorSelector
  , imageSelector
  , rowActionWithStyle_title_handlerSelector
  , setBackgroundColorSelector
  , setImageSelector
  , setTitleSelector
  , styleSelector
  , titleSelector

  -- * Enum types
  , NSTableViewRowActionStyle(NSTableViewRowActionStyle)
  , pattern NSTableViewRowActionStyleRegular
  , pattern NSTableViewRowActionStyleDestructive

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.AppKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @+ rowActionWithStyle:title:handler:@
rowActionWithStyle_title_handler :: IsNSString title => NSTableViewRowActionStyle -> title -> Ptr () -> IO (Id NSTableViewRowAction)
rowActionWithStyle_title_handler style title handler =
  do
    cls' <- getRequiredClass "NSTableViewRowAction"
    sendClassMessage cls' rowActionWithStyle_title_handlerSelector style (toNSString title) handler

-- | @- style@
style :: IsNSTableViewRowAction nsTableViewRowAction => nsTableViewRowAction -> IO NSTableViewRowActionStyle
style nsTableViewRowAction =
  sendMessage nsTableViewRowAction styleSelector

-- | @- title@
title :: IsNSTableViewRowAction nsTableViewRowAction => nsTableViewRowAction -> IO (Id NSString)
title nsTableViewRowAction =
  sendMessage nsTableViewRowAction titleSelector

-- | @- setTitle:@
setTitle :: (IsNSTableViewRowAction nsTableViewRowAction, IsNSString value) => nsTableViewRowAction -> value -> IO ()
setTitle nsTableViewRowAction value =
  sendMessage nsTableViewRowAction setTitleSelector (toNSString value)

-- | @- backgroundColor@
backgroundColor :: IsNSTableViewRowAction nsTableViewRowAction => nsTableViewRowAction -> IO (Id NSColor)
backgroundColor nsTableViewRowAction =
  sendMessage nsTableViewRowAction backgroundColorSelector

-- | @- setBackgroundColor:@
setBackgroundColor :: (IsNSTableViewRowAction nsTableViewRowAction, IsNSColor value) => nsTableViewRowAction -> value -> IO ()
setBackgroundColor nsTableViewRowAction value =
  sendMessage nsTableViewRowAction setBackgroundColorSelector (toNSColor value)

-- | @- image@
image :: IsNSTableViewRowAction nsTableViewRowAction => nsTableViewRowAction -> IO (Id NSImage)
image nsTableViewRowAction =
  sendMessage nsTableViewRowAction imageSelector

-- | @- setImage:@
setImage :: (IsNSTableViewRowAction nsTableViewRowAction, IsNSImage value) => nsTableViewRowAction -> value -> IO ()
setImage nsTableViewRowAction value =
  sendMessage nsTableViewRowAction setImageSelector (toNSImage value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @rowActionWithStyle:title:handler:@
rowActionWithStyle_title_handlerSelector :: Selector '[NSTableViewRowActionStyle, Id NSString, Ptr ()] (Id NSTableViewRowAction)
rowActionWithStyle_title_handlerSelector = mkSelector "rowActionWithStyle:title:handler:"

-- | @Selector@ for @style@
styleSelector :: Selector '[] NSTableViewRowActionStyle
styleSelector = mkSelector "style"

-- | @Selector@ for @title@
titleSelector :: Selector '[] (Id NSString)
titleSelector = mkSelector "title"

-- | @Selector@ for @setTitle:@
setTitleSelector :: Selector '[Id NSString] ()
setTitleSelector = mkSelector "setTitle:"

-- | @Selector@ for @backgroundColor@
backgroundColorSelector :: Selector '[] (Id NSColor)
backgroundColorSelector = mkSelector "backgroundColor"

-- | @Selector@ for @setBackgroundColor:@
setBackgroundColorSelector :: Selector '[Id NSColor] ()
setBackgroundColorSelector = mkSelector "setBackgroundColor:"

-- | @Selector@ for @image@
imageSelector :: Selector '[] (Id NSImage)
imageSelector = mkSelector "image"

-- | @Selector@ for @setImage:@
setImageSelector :: Selector '[Id NSImage] ()
setImageSelector = mkSelector "setImage:"

