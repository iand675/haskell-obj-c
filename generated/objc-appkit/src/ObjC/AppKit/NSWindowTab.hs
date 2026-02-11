{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSWindowTab@.
module ObjC.AppKit.NSWindowTab
  ( NSWindowTab
  , IsNSWindowTab(..)
  , title
  , setTitle
  , attributedTitle
  , setAttributedTitle
  , toolTip
  , setToolTip
  , accessoryView
  , setAccessoryView
  , titleSelector
  , setTitleSelector
  , attributedTitleSelector
  , setAttributedTitleSelector
  , toolTipSelector
  , setToolTipSelector
  , accessoryViewSelector
  , setAccessoryViewSelector


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
title :: IsNSWindowTab nsWindowTab => nsWindowTab -> IO (Id NSString)
title nsWindowTab  =
  sendMsg nsWindowTab (mkSelector "title") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTitle:@
setTitle :: (IsNSWindowTab nsWindowTab, IsNSString value) => nsWindowTab -> value -> IO ()
setTitle nsWindowTab  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsWindowTab (mkSelector "setTitle:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- attributedTitle@
attributedTitle :: IsNSWindowTab nsWindowTab => nsWindowTab -> IO (Id NSAttributedString)
attributedTitle nsWindowTab  =
  sendMsg nsWindowTab (mkSelector "attributedTitle") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAttributedTitle:@
setAttributedTitle :: (IsNSWindowTab nsWindowTab, IsNSAttributedString value) => nsWindowTab -> value -> IO ()
setAttributedTitle nsWindowTab  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsWindowTab (mkSelector "setAttributedTitle:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- toolTip@
toolTip :: IsNSWindowTab nsWindowTab => nsWindowTab -> IO (Id NSString)
toolTip nsWindowTab  =
  sendMsg nsWindowTab (mkSelector "toolTip") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setToolTip:@
setToolTip :: (IsNSWindowTab nsWindowTab, IsNSString value) => nsWindowTab -> value -> IO ()
setToolTip nsWindowTab  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsWindowTab (mkSelector "setToolTip:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- accessoryView@
accessoryView :: IsNSWindowTab nsWindowTab => nsWindowTab -> IO (Id NSView)
accessoryView nsWindowTab  =
  sendMsg nsWindowTab (mkSelector "accessoryView") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAccessoryView:@
setAccessoryView :: (IsNSWindowTab nsWindowTab, IsNSView value) => nsWindowTab -> value -> IO ()
setAccessoryView nsWindowTab  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsWindowTab (mkSelector "setAccessoryView:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

-- | @Selector@ for @toolTip@
toolTipSelector :: Selector
toolTipSelector = mkSelector "toolTip"

-- | @Selector@ for @setToolTip:@
setToolTipSelector :: Selector
setToolTipSelector = mkSelector "setToolTip:"

-- | @Selector@ for @accessoryView@
accessoryViewSelector :: Selector
accessoryViewSelector = mkSelector "accessoryView"

-- | @Selector@ for @setAccessoryView:@
setAccessoryViewSelector :: Selector
setAccessoryViewSelector = mkSelector "setAccessoryView:"

