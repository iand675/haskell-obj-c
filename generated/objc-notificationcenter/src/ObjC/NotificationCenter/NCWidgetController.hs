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
  , widgetControllerSelector
  , defaultWidgetControllerSelector
  , setHasContent_forWidgetWithBundleIdentifierSelector


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
import ObjC.Foundation.Internal.Classes

-- | @+ widgetController@
widgetController :: IO (Id NCWidgetController)
widgetController  =
  do
    cls' <- getRequiredClass "NCWidgetController"
    sendClassMsg cls' (mkSelector "widgetController") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ defaultWidgetController@
defaultWidgetController :: IO (Id NCWidgetController)
defaultWidgetController  =
  do
    cls' <- getRequiredClass "NCWidgetController"
    sendClassMsg cls' (mkSelector "defaultWidgetController") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setHasContent:forWidgetWithBundleIdentifier:@
setHasContent_forWidgetWithBundleIdentifier :: (IsNCWidgetController ncWidgetController, IsNSString bundleID) => ncWidgetController -> Bool -> bundleID -> IO ()
setHasContent_forWidgetWithBundleIdentifier ncWidgetController  flag bundleID =
withObjCPtr bundleID $ \raw_bundleID ->
    sendMsg ncWidgetController (mkSelector "setHasContent:forWidgetWithBundleIdentifier:") retVoid [argCULong (if flag then 1 else 0), argPtr (castPtr raw_bundleID :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @widgetController@
widgetControllerSelector :: Selector
widgetControllerSelector = mkSelector "widgetController"

-- | @Selector@ for @defaultWidgetController@
defaultWidgetControllerSelector :: Selector
defaultWidgetControllerSelector = mkSelector "defaultWidgetController"

-- | @Selector@ for @setHasContent:forWidgetWithBundleIdentifier:@
setHasContent_forWidgetWithBundleIdentifierSelector :: Selector
setHasContent_forWidgetWithBundleIdentifierSelector = mkSelector "setHasContent:forWidgetWithBundleIdentifier:"

