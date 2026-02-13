{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @UNNotificationSettings@.
module ObjC.UserNotifications.UNNotificationSettings
  ( UNNotificationSettings
  , IsUNNotificationSettings(..)
  , init_
  , authorizationStatus
  , soundSetting
  , badgeSetting
  , alertSetting
  , notificationCenterSetting
  , lockScreenSetting
  , carPlaySetting
  , alertStyle
  , showPreviewsSetting
  , criticalAlertSetting
  , providesAppNotificationSettings
  , announcementSetting
  , timeSensitiveSetting
  , scheduledDeliverySetting
  , directMessagesSetting
  , alertSettingSelector
  , alertStyleSelector
  , announcementSettingSelector
  , authorizationStatusSelector
  , badgeSettingSelector
  , carPlaySettingSelector
  , criticalAlertSettingSelector
  , directMessagesSettingSelector
  , initSelector
  , lockScreenSettingSelector
  , notificationCenterSettingSelector
  , providesAppNotificationSettingsSelector
  , scheduledDeliverySettingSelector
  , showPreviewsSettingSelector
  , soundSettingSelector
  , timeSensitiveSettingSelector

  -- * Enum types
  , UNAlertStyle(UNAlertStyle)
  , pattern UNAlertStyleNone
  , pattern UNAlertStyleBanner
  , pattern UNAlertStyleAlert
  , UNAuthorizationStatus(UNAuthorizationStatus)
  , pattern UNAuthorizationStatusNotDetermined
  , pattern UNAuthorizationStatusDenied
  , pattern UNAuthorizationStatusAuthorized
  , pattern UNAuthorizationStatusProvisional
  , pattern UNAuthorizationStatusEphemeral
  , UNNotificationSetting(UNNotificationSetting)
  , pattern UNNotificationSettingNotSupported
  , pattern UNNotificationSettingDisabled
  , pattern UNNotificationSettingEnabled
  , UNShowPreviewsSetting(UNShowPreviewsSetting)
  , pattern UNShowPreviewsSettingAlways
  , pattern UNShowPreviewsSettingWhenAuthenticated
  , pattern UNShowPreviewsSettingNever

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.UserNotifications.Internal.Classes
import ObjC.UserNotifications.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsUNNotificationSettings unNotificationSettings => unNotificationSettings -> IO (Id UNNotificationSettings)
init_ unNotificationSettings =
  sendOwnedMessage unNotificationSettings initSelector

-- | @- authorizationStatus@
authorizationStatus :: IsUNNotificationSettings unNotificationSettings => unNotificationSettings -> IO UNAuthorizationStatus
authorizationStatus unNotificationSettings =
  sendMessage unNotificationSettings authorizationStatusSelector

-- | @- soundSetting@
soundSetting :: IsUNNotificationSettings unNotificationSettings => unNotificationSettings -> IO UNNotificationSetting
soundSetting unNotificationSettings =
  sendMessage unNotificationSettings soundSettingSelector

-- | @- badgeSetting@
badgeSetting :: IsUNNotificationSettings unNotificationSettings => unNotificationSettings -> IO UNNotificationSetting
badgeSetting unNotificationSettings =
  sendMessage unNotificationSettings badgeSettingSelector

-- | @- alertSetting@
alertSetting :: IsUNNotificationSettings unNotificationSettings => unNotificationSettings -> IO UNNotificationSetting
alertSetting unNotificationSettings =
  sendMessage unNotificationSettings alertSettingSelector

-- | @- notificationCenterSetting@
notificationCenterSetting :: IsUNNotificationSettings unNotificationSettings => unNotificationSettings -> IO UNNotificationSetting
notificationCenterSetting unNotificationSettings =
  sendMessage unNotificationSettings notificationCenterSettingSelector

-- | @- lockScreenSetting@
lockScreenSetting :: IsUNNotificationSettings unNotificationSettings => unNotificationSettings -> IO UNNotificationSetting
lockScreenSetting unNotificationSettings =
  sendMessage unNotificationSettings lockScreenSettingSelector

-- | @- carPlaySetting@
carPlaySetting :: IsUNNotificationSettings unNotificationSettings => unNotificationSettings -> IO UNNotificationSetting
carPlaySetting unNotificationSettings =
  sendMessage unNotificationSettings carPlaySettingSelector

-- | @- alertStyle@
alertStyle :: IsUNNotificationSettings unNotificationSettings => unNotificationSettings -> IO UNAlertStyle
alertStyle unNotificationSettings =
  sendMessage unNotificationSettings alertStyleSelector

-- | @- showPreviewsSetting@
showPreviewsSetting :: IsUNNotificationSettings unNotificationSettings => unNotificationSettings -> IO UNShowPreviewsSetting
showPreviewsSetting unNotificationSettings =
  sendMessage unNotificationSettings showPreviewsSettingSelector

-- | @- criticalAlertSetting@
criticalAlertSetting :: IsUNNotificationSettings unNotificationSettings => unNotificationSettings -> IO UNNotificationSetting
criticalAlertSetting unNotificationSettings =
  sendMessage unNotificationSettings criticalAlertSettingSelector

-- | @- providesAppNotificationSettings@
providesAppNotificationSettings :: IsUNNotificationSettings unNotificationSettings => unNotificationSettings -> IO Bool
providesAppNotificationSettings unNotificationSettings =
  sendMessage unNotificationSettings providesAppNotificationSettingsSelector

-- | @- announcementSetting@
announcementSetting :: IsUNNotificationSettings unNotificationSettings => unNotificationSettings -> IO UNNotificationSetting
announcementSetting unNotificationSettings =
  sendMessage unNotificationSettings announcementSettingSelector

-- | @- timeSensitiveSetting@
timeSensitiveSetting :: IsUNNotificationSettings unNotificationSettings => unNotificationSettings -> IO UNNotificationSetting
timeSensitiveSetting unNotificationSettings =
  sendMessage unNotificationSettings timeSensitiveSettingSelector

-- | @- scheduledDeliverySetting@
scheduledDeliverySetting :: IsUNNotificationSettings unNotificationSettings => unNotificationSettings -> IO UNNotificationSetting
scheduledDeliverySetting unNotificationSettings =
  sendMessage unNotificationSettings scheduledDeliverySettingSelector

-- | @- directMessagesSetting@
directMessagesSetting :: IsUNNotificationSettings unNotificationSettings => unNotificationSettings -> IO UNNotificationSetting
directMessagesSetting unNotificationSettings =
  sendMessage unNotificationSettings directMessagesSettingSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id UNNotificationSettings)
initSelector = mkSelector "init"

-- | @Selector@ for @authorizationStatus@
authorizationStatusSelector :: Selector '[] UNAuthorizationStatus
authorizationStatusSelector = mkSelector "authorizationStatus"

-- | @Selector@ for @soundSetting@
soundSettingSelector :: Selector '[] UNNotificationSetting
soundSettingSelector = mkSelector "soundSetting"

-- | @Selector@ for @badgeSetting@
badgeSettingSelector :: Selector '[] UNNotificationSetting
badgeSettingSelector = mkSelector "badgeSetting"

-- | @Selector@ for @alertSetting@
alertSettingSelector :: Selector '[] UNNotificationSetting
alertSettingSelector = mkSelector "alertSetting"

-- | @Selector@ for @notificationCenterSetting@
notificationCenterSettingSelector :: Selector '[] UNNotificationSetting
notificationCenterSettingSelector = mkSelector "notificationCenterSetting"

-- | @Selector@ for @lockScreenSetting@
lockScreenSettingSelector :: Selector '[] UNNotificationSetting
lockScreenSettingSelector = mkSelector "lockScreenSetting"

-- | @Selector@ for @carPlaySetting@
carPlaySettingSelector :: Selector '[] UNNotificationSetting
carPlaySettingSelector = mkSelector "carPlaySetting"

-- | @Selector@ for @alertStyle@
alertStyleSelector :: Selector '[] UNAlertStyle
alertStyleSelector = mkSelector "alertStyle"

-- | @Selector@ for @showPreviewsSetting@
showPreviewsSettingSelector :: Selector '[] UNShowPreviewsSetting
showPreviewsSettingSelector = mkSelector "showPreviewsSetting"

-- | @Selector@ for @criticalAlertSetting@
criticalAlertSettingSelector :: Selector '[] UNNotificationSetting
criticalAlertSettingSelector = mkSelector "criticalAlertSetting"

-- | @Selector@ for @providesAppNotificationSettings@
providesAppNotificationSettingsSelector :: Selector '[] Bool
providesAppNotificationSettingsSelector = mkSelector "providesAppNotificationSettings"

-- | @Selector@ for @announcementSetting@
announcementSettingSelector :: Selector '[] UNNotificationSetting
announcementSettingSelector = mkSelector "announcementSetting"

-- | @Selector@ for @timeSensitiveSetting@
timeSensitiveSettingSelector :: Selector '[] UNNotificationSetting
timeSensitiveSettingSelector = mkSelector "timeSensitiveSetting"

-- | @Selector@ for @scheduledDeliverySetting@
scheduledDeliverySettingSelector :: Selector '[] UNNotificationSetting
scheduledDeliverySettingSelector = mkSelector "scheduledDeliverySetting"

-- | @Selector@ for @directMessagesSetting@
directMessagesSettingSelector :: Selector '[] UNNotificationSetting
directMessagesSettingSelector = mkSelector "directMessagesSetting"

