{-# LANGUAGE DataKinds #-}
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
  , iconWithSystemImageNameSelector
  , iconWithTemplateImageNameSelector
  , initSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.UserNotifications.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ iconWithTemplateImageName:@
iconWithTemplateImageName :: IsNSString templateImageName => templateImageName -> IO (Id UNNotificationActionIcon)
iconWithTemplateImageName templateImageName =
  do
    cls' <- getRequiredClass "UNNotificationActionIcon"
    sendClassMessage cls' iconWithTemplateImageNameSelector (toNSString templateImageName)

-- | @+ iconWithSystemImageName:@
iconWithSystemImageName :: IsNSString systemImageName => systemImageName -> IO (Id UNNotificationActionIcon)
iconWithSystemImageName systemImageName =
  do
    cls' <- getRequiredClass "UNNotificationActionIcon"
    sendClassMessage cls' iconWithSystemImageNameSelector (toNSString systemImageName)

-- | @- init@
init_ :: IsUNNotificationActionIcon unNotificationActionIcon => unNotificationActionIcon -> IO (Id UNNotificationActionIcon)
init_ unNotificationActionIcon =
  sendOwnedMessage unNotificationActionIcon initSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @iconWithTemplateImageName:@
iconWithTemplateImageNameSelector :: Selector '[Id NSString] (Id UNNotificationActionIcon)
iconWithTemplateImageNameSelector = mkSelector "iconWithTemplateImageName:"

-- | @Selector@ for @iconWithSystemImageName:@
iconWithSystemImageNameSelector :: Selector '[Id NSString] (Id UNNotificationActionIcon)
iconWithSystemImageNameSelector = mkSelector "iconWithSystemImageName:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id UNNotificationActionIcon)
initSelector = mkSelector "init"

