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

import ObjC.NotificationCenter.Internal.Classes
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- presentViewControllerInWidget:@
presentViewControllerInWidget :: (IsNSViewController nsViewController, IsNSViewController viewController) => nsViewController -> viewController -> IO ()
presentViewControllerInWidget nsViewController  viewController =
withObjCPtr viewController $ \raw_viewController ->
    sendMsg nsViewController (mkSelector "presentViewControllerInWidget:") retVoid [argPtr (castPtr raw_viewController :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @presentViewControllerInWidget:@
presentViewControllerInWidgetSelector :: Selector
presentViewControllerInWidgetSelector = mkSelector "presentViewControllerInWidget:"

