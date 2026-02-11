{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSPathControlItem@.
module ObjC.AppKit.NSPathControlItem
  ( NSPathControlItem
  , IsNSPathControlItem(..)
  , title
  , setTitle
  , attributedTitle
  , setAttributedTitle
  , image
  , setImage
  , url
  , titleSelector
  , setTitleSelector
  , attributedTitleSelector
  , setAttributedTitleSelector
  , imageSelector
  , setImageSelector
  , urlSelector


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
import ObjC.Foundation.Internal.Classes

-- | @- title@
title :: IsNSPathControlItem nsPathControlItem => nsPathControlItem -> IO (Id NSString)
title nsPathControlItem  =
  sendMsg nsPathControlItem (mkSelector "title") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTitle:@
setTitle :: (IsNSPathControlItem nsPathControlItem, IsNSString value) => nsPathControlItem -> value -> IO ()
setTitle nsPathControlItem  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsPathControlItem (mkSelector "setTitle:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- attributedTitle@
attributedTitle :: IsNSPathControlItem nsPathControlItem => nsPathControlItem -> IO (Id NSAttributedString)
attributedTitle nsPathControlItem  =
  sendMsg nsPathControlItem (mkSelector "attributedTitle") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAttributedTitle:@
setAttributedTitle :: (IsNSPathControlItem nsPathControlItem, IsNSAttributedString value) => nsPathControlItem -> value -> IO ()
setAttributedTitle nsPathControlItem  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsPathControlItem (mkSelector "setAttributedTitle:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- image@
image :: IsNSPathControlItem nsPathControlItem => nsPathControlItem -> IO (Id NSImage)
image nsPathControlItem  =
  sendMsg nsPathControlItem (mkSelector "image") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setImage:@
setImage :: (IsNSPathControlItem nsPathControlItem, IsNSImage value) => nsPathControlItem -> value -> IO ()
setImage nsPathControlItem  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsPathControlItem (mkSelector "setImage:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- URL@
url :: IsNSPathControlItem nsPathControlItem => nsPathControlItem -> IO (Id NSURL)
url nsPathControlItem  =
  sendMsg nsPathControlItem (mkSelector "URL") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @title@
titleSelector :: Selector
titleSelector = mkSelector "title"

-- | @Selector@ for @setTitle:@
setTitleSelector :: Selector
setTitleSelector = mkSelector "setTitle:"

-- | @Selector@ for @attributedTitle@
attributedTitleSelector :: Selector
attributedTitleSelector = mkSelector "attributedTitle"

-- | @Selector@ for @setAttributedTitle:@
setAttributedTitleSelector :: Selector
setAttributedTitleSelector = mkSelector "setAttributedTitle:"

-- | @Selector@ for @image@
imageSelector :: Selector
imageSelector = mkSelector "image"

-- | @Selector@ for @setImage:@
setImageSelector :: Selector
setImageSelector = mkSelector "setImage:"

-- | @Selector@ for @URL@
urlSelector :: Selector
urlSelector = mkSelector "URL"

