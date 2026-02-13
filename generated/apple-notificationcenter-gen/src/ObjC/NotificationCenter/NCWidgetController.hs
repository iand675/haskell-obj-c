{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NCWidgetController@.
module ObjC.NotificationCenter.NCWidgetController
  ( NCWidgetController
  , IsNCWidgetController(..)
  , widgetController
  , defaultWidgetController
  , setHasContent_forWidgetWithBundleIdentifier
  , defaultWidgetControllerSelector
  , setHasContent_forWidgetWithBundleIdentifierSelector
  , widgetControllerSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.NotificationCenter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ widgetController@
widgetController :: IO (Id NCWidgetController)
widgetController  =
  do
    cls' <- getRequiredClass "NCWidgetController"
    sendClassMessage cls' widgetControllerSelector

-- | @+ defaultWidgetController@
defaultWidgetController :: IO (Id NCWidgetController)
defaultWidgetController  =
  do
    cls' <- getRequiredClass "NCWidgetController"
    sendClassMessage cls' defaultWidgetControllerSelector

-- | @- setHasContent:forWidgetWithBundleIdentifier:@
setHasContent_forWidgetWithBundleIdentifier :: (IsNCWidgetController ncWidgetController, IsNSString bundleID) => ncWidgetController -> Bool -> bundleID -> IO ()
setHasContent_forWidgetWithBundleIdentifier ncWidgetController flag bundleID =
  sendMessage ncWidgetController setHasContent_forWidgetWithBundleIdentifierSelector flag (toNSString bundleID)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @widgetController@
widgetControllerSelector :: Selector '[] (Id NCWidgetController)
widgetControllerSelector = mkSelector "widgetController"

-- | @Selector@ for @defaultWidgetController@
defaultWidgetControllerSelector :: Selector '[] (Id NCWidgetController)
defaultWidgetControllerSelector = mkSelector "defaultWidgetController"

-- | @Selector@ for @setHasContent:forWidgetWithBundleIdentifier:@
setHasContent_forWidgetWithBundleIdentifierSelector :: Selector '[Bool, Id NSString] ()
setHasContent_forWidgetWithBundleIdentifierSelector = mkSelector "setHasContent:forWidgetWithBundleIdentifier:"

