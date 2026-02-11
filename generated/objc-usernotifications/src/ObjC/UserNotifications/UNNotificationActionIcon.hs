{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @UNNotificationActionIcon@.
module ObjC.UserNotifications.UNNotificationActionIcon
  ( UNNotificationActionIcon
  , IsUNNotificationActionIcon(..)
  , iconWithTemplateImageName
  , iconWithSystemImageName
  , init_
  , iconWithTemplateImageNameSelector
  , iconWithSystemImageNameSelector
  , initSelector


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

import ObjC.UserNotifications.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ iconWithTemplateImageName:@
iconWithTemplateImageName :: IsNSString templateImageName => templateImageName -> IO (Id UNNotificationActionIcon)
iconWithTemplateImageName templateImageName =
  do
    cls' <- getRequiredClass "UNNotificationActionIcon"
    withObjCPtr templateImageName $ \raw_templateImageName ->
      sendClassMsg cls' (mkSelector "iconWithTemplateImageName:") (retPtr retVoid) [argPtr (castPtr raw_templateImageName :: Ptr ())] >>= retainedObject . castPtr

-- | @+ iconWithSystemImageName:@
iconWithSystemImageName :: IsNSString systemImageName => systemImageName -> IO (Id UNNotificationActionIcon)
iconWithSystemImageName systemImageName =
  do
    cls' <- getRequiredClass "UNNotificationActionIcon"
    withObjCPtr systemImageName $ \raw_systemImageName ->
      sendClassMsg cls' (mkSelector "iconWithSystemImageName:") (retPtr retVoid) [argPtr (castPtr raw_systemImageName :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsUNNotificationActionIcon unNotificationActionIcon => unNotificationActionIcon -> IO (Id UNNotificationActionIcon)
init_ unNotificationActionIcon  =
  sendMsg unNotificationActionIcon (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @iconWithTemplateImageName:@
iconWithTemplateImageNameSelector :: Selector
iconWithTemplateImageNameSelector = mkSelector "iconWithTemplateImageName:"

-- | @Selector@ for @iconWithSystemImageName:@
iconWithSystemImageNameSelector :: Selector
iconWithSystemImageNameSelector = mkSelector "iconWithSystemImageName:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

