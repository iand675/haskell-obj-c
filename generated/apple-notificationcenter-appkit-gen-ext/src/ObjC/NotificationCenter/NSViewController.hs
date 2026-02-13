{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSViewController@.
module ObjC.NotificationCenter.NSViewController
  ( NSViewController
  , IsNSViewController(..)
  , presentViewControllerInWidget
  , presentViewControllerInWidgetSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.NotificationCenter.Internal.Classes
import ObjC.AppKit.Internal.Classes

-- | @- presentViewControllerInWidget:@
presentViewControllerInWidget :: (IsNSViewController nsViewController, IsNSViewController viewController) => nsViewController -> viewController -> IO ()
presentViewControllerInWidget nsViewController viewController =
  sendMessage nsViewController presentViewControllerInWidgetSelector (toNSViewController viewController)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @presentViewControllerInWidget:@
presentViewControllerInWidgetSelector :: Selector '[Id NSViewController] ()
presentViewControllerInWidgetSelector = mkSelector "presentViewControllerInWidget:"

