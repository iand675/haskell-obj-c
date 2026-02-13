{-# LANGUAGE DataKinds #-}
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
  , accessoryViewSelector
  , attributedTitleSelector
  , setAccessoryViewSelector
  , setAttributedTitleSelector
  , setTitleSelector
  , setToolTipSelector
  , titleSelector
  , toolTipSelector


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
title :: IsNSWindowTab nsWindowTab => nsWindowTab -> IO (Id NSString)
title nsWindowTab =
  sendMessage nsWindowTab titleSelector

-- | @- setTitle:@
setTitle :: (IsNSWindowTab nsWindowTab, IsNSString value) => nsWindowTab -> value -> IO ()
setTitle nsWindowTab value =
  sendMessage nsWindowTab setTitleSelector (toNSString value)

-- | @- attributedTitle@
attributedTitle :: IsNSWindowTab nsWindowTab => nsWindowTab -> IO (Id NSAttributedString)
attributedTitle nsWindowTab =
  sendMessage nsWindowTab attributedTitleSelector

-- | @- setAttributedTitle:@
setAttributedTitle :: (IsNSWindowTab nsWindowTab, IsNSAttributedString value) => nsWindowTab -> value -> IO ()
setAttributedTitle nsWindowTab value =
  sendMessage nsWindowTab setAttributedTitleSelector (toNSAttributedString value)

-- | @- toolTip@
toolTip :: IsNSWindowTab nsWindowTab => nsWindowTab -> IO (Id NSString)
toolTip nsWindowTab =
  sendMessage nsWindowTab toolTipSelector

-- | @- setToolTip:@
setToolTip :: (IsNSWindowTab nsWindowTab, IsNSString value) => nsWindowTab -> value -> IO ()
setToolTip nsWindowTab value =
  sendMessage nsWindowTab setToolTipSelector (toNSString value)

-- | @- accessoryView@
accessoryView :: IsNSWindowTab nsWindowTab => nsWindowTab -> IO (Id NSView)
accessoryView nsWindowTab =
  sendMessage nsWindowTab accessoryViewSelector

-- | @- setAccessoryView:@
setAccessoryView :: (IsNSWindowTab nsWindowTab, IsNSView value) => nsWindowTab -> value -> IO ()
setAccessoryView nsWindowTab value =
  sendMessage nsWindowTab setAccessoryViewSelector (toNSView value)

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

-- | @Selector@ for @toolTip@
toolTipSelector :: Selector '[] (Id NSString)
toolTipSelector = mkSelector "toolTip"

-- | @Selector@ for @setToolTip:@
setToolTipSelector :: Selector '[Id NSString] ()
setToolTipSelector = mkSelector "setToolTip:"

-- | @Selector@ for @accessoryView@
accessoryViewSelector :: Selector '[] (Id NSView)
accessoryViewSelector = mkSelector "accessoryView"

-- | @Selector@ for @setAccessoryView:@
setAccessoryViewSelector :: Selector '[Id NSView] ()
setAccessoryViewSelector = mkSelector "setAccessoryView:"

