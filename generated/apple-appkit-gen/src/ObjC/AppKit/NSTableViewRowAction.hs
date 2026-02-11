{-# LANGUAGE PatternSynonyms #-}
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
  , rowActionWithStyle_title_handlerSelector
  , styleSelector
  , titleSelector
  , setTitleSelector
  , backgroundColorSelector
  , setBackgroundColorSelector
  , imageSelector
  , setImageSelector

  -- * Enum types
  , NSTableViewRowActionStyle(NSTableViewRowActionStyle)
  , pattern NSTableViewRowActionStyleRegular
  , pattern NSTableViewRowActionStyleDestructive

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

import ObjC.AppKit.Internal.Classes
import ObjC.AppKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @+ rowActionWithStyle:title:handler:@
rowActionWithStyle_title_handler :: IsNSString title => NSTableViewRowActionStyle -> title -> Ptr () -> IO (Id NSTableViewRowAction)
rowActionWithStyle_title_handler style title handler =
  do
    cls' <- getRequiredClass "NSTableViewRowAction"
    withObjCPtr title $ \raw_title ->
      sendClassMsg cls' (mkSelector "rowActionWithStyle:title:handler:") (retPtr retVoid) [argCLong (coerce style), argPtr (castPtr raw_title :: Ptr ()), argPtr (castPtr handler :: Ptr ())] >>= retainedObject . castPtr

-- | @- style@
style :: IsNSTableViewRowAction nsTableViewRowAction => nsTableViewRowAction -> IO NSTableViewRowActionStyle
style nsTableViewRowAction  =
    fmap (coerce :: CLong -> NSTableViewRowActionStyle) $ sendMsg nsTableViewRowAction (mkSelector "style") retCLong []

-- | @- title@
title :: IsNSTableViewRowAction nsTableViewRowAction => nsTableViewRowAction -> IO (Id NSString)
title nsTableViewRowAction  =
    sendMsg nsTableViewRowAction (mkSelector "title") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTitle:@
setTitle :: (IsNSTableViewRowAction nsTableViewRowAction, IsNSString value) => nsTableViewRowAction -> value -> IO ()
setTitle nsTableViewRowAction  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsTableViewRowAction (mkSelector "setTitle:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- backgroundColor@
backgroundColor :: IsNSTableViewRowAction nsTableViewRowAction => nsTableViewRowAction -> IO (Id NSColor)
backgroundColor nsTableViewRowAction  =
    sendMsg nsTableViewRowAction (mkSelector "backgroundColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setBackgroundColor:@
setBackgroundColor :: (IsNSTableViewRowAction nsTableViewRowAction, IsNSColor value) => nsTableViewRowAction -> value -> IO ()
setBackgroundColor nsTableViewRowAction  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsTableViewRowAction (mkSelector "setBackgroundColor:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- image@
image :: IsNSTableViewRowAction nsTableViewRowAction => nsTableViewRowAction -> IO (Id NSImage)
image nsTableViewRowAction  =
    sendMsg nsTableViewRowAction (mkSelector "image") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setImage:@
setImage :: (IsNSTableViewRowAction nsTableViewRowAction, IsNSImage value) => nsTableViewRowAction -> value -> IO ()
setImage nsTableViewRowAction  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsTableViewRowAction (mkSelector "setImage:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @rowActionWithStyle:title:handler:@
rowActionWithStyle_title_handlerSelector :: Selector
rowActionWithStyle_title_handlerSelector = mkSelector "rowActionWithStyle:title:handler:"

-- | @Selector@ for @style@
styleSelector :: Selector
styleSelector = mkSelector "style"

-- | @Selector@ for @title@
titleSelector :: Selector
titleSelector = mkSelector "title"

-- | @Selector@ for @setTitle:@
setTitleSelector :: Selector
setTitleSelector = mkSelector "setTitle:"

-- | @Selector@ for @backgroundColor@
backgroundColorSelector :: Selector
backgroundColorSelector = mkSelector "backgroundColor"

-- | @Selector@ for @setBackgroundColor:@
setBackgroundColorSelector :: Selector
setBackgroundColorSelector = mkSelector "setBackgroundColor:"

-- | @Selector@ for @image@
imageSelector :: Selector
imageSelector = mkSelector "image"

-- | @Selector@ for @setImage:@
setImageSelector :: Selector
setImageSelector = mkSelector "setImage:"

