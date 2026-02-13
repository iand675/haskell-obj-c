{-# LANGUAGE DataKinds #-}
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
  , attributedTitleSelector
  , imageSelector
  , setAttributedTitleSelector
  , setImageSelector
  , setTitleSelector
  , titleSelector
  , urlSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- title@
title :: IsNSPathControlItem nsPathControlItem => nsPathControlItem -> IO (Id NSString)
title nsPathControlItem =
  sendMessage nsPathControlItem titleSelector

-- | @- setTitle:@
setTitle :: (IsNSPathControlItem nsPathControlItem, IsNSString value) => nsPathControlItem -> value -> IO ()
setTitle nsPathControlItem value =
  sendMessage nsPathControlItem setTitleSelector (toNSString value)

-- | @- attributedTitle@
attributedTitle :: IsNSPathControlItem nsPathControlItem => nsPathControlItem -> IO (Id NSAttributedString)
attributedTitle nsPathControlItem =
  sendMessage nsPathControlItem attributedTitleSelector

-- | @- setAttributedTitle:@
setAttributedTitle :: (IsNSPathControlItem nsPathControlItem, IsNSAttributedString value) => nsPathControlItem -> value -> IO ()
setAttributedTitle nsPathControlItem value =
  sendMessage nsPathControlItem setAttributedTitleSelector (toNSAttributedString value)

-- | @- image@
image :: IsNSPathControlItem nsPathControlItem => nsPathControlItem -> IO (Id NSImage)
image nsPathControlItem =
  sendMessage nsPathControlItem imageSelector

-- | @- setImage:@
setImage :: (IsNSPathControlItem nsPathControlItem, IsNSImage value) => nsPathControlItem -> value -> IO ()
setImage nsPathControlItem value =
  sendMessage nsPathControlItem setImageSelector (toNSImage value)

-- | @- URL@
url :: IsNSPathControlItem nsPathControlItem => nsPathControlItem -> IO (Id NSURL)
url nsPathControlItem =
  sendMessage nsPathControlItem urlSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @title@
titleSelector :: Selector '[] (Id NSString)
titleSelector = mkSelector "title"

-- | @Selector@ for @setTitle:@
setTitleSelector :: Selector '[Id NSString] ()
setTitleSelector = mkSelector "setTitle:"

-- | @Selector@ for @attributedTitle@
attributedTitleSelector :: Selector '[] (Id NSAttributedString)
attributedTitleSelector = mkSelector "attributedTitle"

-- | @Selector@ for @setAttributedTitle:@
setAttributedTitleSelector :: Selector '[Id NSAttributedString] ()
setAttributedTitleSelector = mkSelector "setAttributedTitle:"

-- | @Selector@ for @image@
imageSelector :: Selector '[] (Id NSImage)
imageSelector = mkSelector "image"

-- | @Selector@ for @setImage:@
setImageSelector :: Selector '[Id NSImage] ()
setImageSelector = mkSelector "setImage:"

-- | @Selector@ for @URL@
urlSelector :: Selector '[] (Id NSURL)
urlSelector = mkSelector "URL"

