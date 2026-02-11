{-# LANGUAGE PatternSynonyms #-}
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
  , initSelector
  , authorizationStatusSelector
  , soundSettingSelector
  , badgeSettingSelector
  , alertSettingSelector
  , notificationCenterSettingSelector
  , lockScreenSettingSelector
  , carPlaySettingSelector
  , alertStyleSelector
  , showPreviewsSettingSelector
  , criticalAlertSettingSelector
  , providesAppNotificationSettingsSelector
  , announcementSettingSelector
  , timeSensitiveSettingSelector
  , scheduledDeliverySettingSelector
  , directMessagesSettingSelector

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
import ObjC.UserNotifications.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsUNNotificationSettings unNotificationSettings => unNotificationSettings -> IO (Id UNNotificationSettings)
init_ unNotificationSettings  =
  sendMsg unNotificationSettings (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- authorizationStatus@
authorizationStatus :: IsUNNotificationSettings unNotificationSettings => unNotificationSettings -> IO UNAuthorizationStatus
authorizationStatus unNotificationSettings  =
  fmap (coerce :: CLong -> UNAuthorizationStatus) $ sendMsg unNotificationSettings (mkSelector "authorizationStatus") retCLong []

-- | @- soundSetting@
soundSetting :: IsUNNotificationSettings unNotificationSettings => unNotificationSettings -> IO UNNotificationSetting
soundSetting unNotificationSettings  =
  fmap (coerce :: CLong -> UNNotificationSetting) $ sendMsg unNotificationSettings (mkSelector "soundSetting") retCLong []

-- | @- badgeSetting@
badgeSetting :: IsUNNotificationSettings unNotificationSettings => unNotificationSettings -> IO UNNotificationSetting
badgeSetting unNotificationSettings  =
  fmap (coerce :: CLong -> UNNotificationSetting) $ sendMsg unNotificationSettings (mkSelector "badgeSetting") retCLong []

-- | @- alertSetting@
alertSetting :: IsUNNotificationSettings unNotificationSettings => unNotificationSettings -> IO UNNotificationSetting
alertSetting unNotificationSettings  =
  fmap (coerce :: CLong -> UNNotificationSetting) $ sendMsg unNotificationSettings (mkSelector "alertSetting") retCLong []

-- | @- notificationCenterSetting@
notificationCenterSetting :: IsUNNotificationSettings unNotificationSettings => unNotificationSettings -> IO UNNotificationSetting
notificationCenterSetting unNotificationSettings  =
  fmap (coerce :: CLong -> UNNotificationSetting) $ sendMsg unNotificationSettings (mkSelector "notificationCenterSetting") retCLong []

-- | @- lockScreenSetting@
lockScreenSetting :: IsUNNotificationSettings unNotificationSettings => unNotificationSettings -> IO UNNotificationSetting
lockScreenSetting unNotificationSettings  =
  fmap (coerce :: CLong -> UNNotificationSetting) $ sendMsg unNotificationSettings (mkSelector "lockScreenSetting") retCLong []

-- | @- carPlaySetting@
carPlaySetting :: IsUNNotificationSettings unNotificationSettings => unNotificationSettings -> IO UNNotificationSetting
carPlaySetting unNotificationSettings  =
  fmap (coerce :: CLong -> UNNotificationSetting) $ sendMsg unNotificationSettings (mkSelector "carPlaySetting") retCLong []

-- | @- alertStyle@
alertStyle :: IsUNNotificationSettings unNotificationSettings => unNotificationSettings -> IO UNAlertStyle
alertStyle unNotificationSettings  =
  fmap (coerce :: CLong -> UNAlertStyle) $ sendMsg unNotificationSettings (mkSelector "alertStyle") retCLong []

-- | @- showPreviewsSetting@
showPreviewsSetting :: IsUNNotificationSettings unNotificationSettings => unNotificationSettings -> IO UNShowPreviewsSetting
showPreviewsSetting unNotificationSettings  =
  fmap (coerce :: CLong -> UNShowPreviewsSetting) $ sendMsg unNotificationSettings (mkSelector "showPreviewsSetting") retCLong []

-- | @- criticalAlertSetting@
criticalAlertSetting :: IsUNNotificationSettings unNotificationSettings => unNotificationSettings -> IO UNNotificationSetting
criticalAlertSetting unNotificationSettings  =
  fmap (coerce :: CLong -> UNNotificationSetting) $ sendMsg unNotificationSettings (mkSelector "criticalAlertSetting") retCLong []

-- | @- providesAppNotificationSettings@
providesAppNotificationSettings :: IsUNNotificationSettings unNotificationSettings => unNotificationSettings -> IO Bool
providesAppNotificationSettings unNotificationSettings  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg unNotificationSettings (mkSelector "providesAppNotificationSettings") retCULong []

-- | @- announcementSetting@
announcementSetting :: IsUNNotificationSettings unNotificationSettings => unNotificationSettings -> IO UNNotificationSetting
announcementSetting unNotificationSettings  =
  fmap (coerce :: CLong -> UNNotificationSetting) $ sendMsg unNotificationSettings (mkSelector "announcementSetting") retCLong []

-- | @- timeSensitiveSetting@
timeSensitiveSetting :: IsUNNotificationSettings unNotificationSettings => unNotificationSettings -> IO UNNotificationSetting
timeSensitiveSetting unNotificationSettings  =
  fmap (coerce :: CLong -> UNNotificationSetting) $ sendMsg unNotificationSettings (mkSelector "timeSensitiveSetting") retCLong []

-- | @- scheduledDeliverySetting@
scheduledDeliverySetting :: IsUNNotificationSettings unNotificationSettings => unNotificationSettings -> IO UNNotificationSetting
scheduledDeliverySetting unNotificationSettings  =
  fmap (coerce :: CLong -> UNNotificationSetting) $ sendMsg unNotificationSettings (mkSelector "scheduledDeliverySetting") retCLong []

-- | @- directMessagesSetting@
directMessagesSetting :: IsUNNotificationSettings unNotificationSettings => unNotificationSettings -> IO UNNotificationSetting
directMessagesSetting unNotificationSettings  =
  fmap (coerce :: CLong -> UNNotificationSetting) $ sendMsg unNotificationSettings (mkSelector "directMessagesSetting") retCLong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @authorizationStatus@
authorizationStatusSelector :: Selector
authorizationStatusSelector = mkSelector "authorizationStatus"

-- | @Selector@ for @soundSetting@
soundSettingSelector :: Selector
soundSettingSelector = mkSelector "soundSetting"

-- | @Selector@ for @badgeSetting@
badgeSettingSelector :: Selector
badgeSettingSelector = mkSelector "badgeSetting"

-- | @Selector@ for @alertSetting@
alertSettingSelector :: Selector
alertSettingSelector = mkSelector "alertSetting"

-- | @Selector@ for @notificationCenterSetting@
notificationCenterSettingSelector :: Selector
notificationCenterSettingSelector = mkSelector "notificationCenterSetting"

-- | @Selector@ for @lockScreenSetting@
lockScreenSettingSelector :: Selector
lockScreenSettingSelector = mkSelector "lockScreenSetting"

-- | @Selector@ for @carPlaySetting@
carPlaySettingSelector :: Selector
carPlaySettingSelector = mkSelector "carPlaySetting"

-- | @Selector@ for @alertStyle@
alertStyleSelector :: Selector
alertStyleSelector = mkSelector "alertStyle"

-- | @Selector@ for @showPreviewsSetting@
showPreviewsSettingSelector :: Selector
showPreviewsSettingSelector = mkSelector "showPreviewsSetting"

-- | @Selector@ for @criticalAlertSetting@
criticalAlertSettingSelector :: Selector
criticalAlertSettingSelector = mkSelector "criticalAlertSetting"

-- | @Selector@ for @providesAppNotificationSettings@
providesAppNotificationSettingsSelector :: Selector
providesAppNotificationSettingsSelector = mkSelector "providesAppNotificationSettings"

-- | @Selector@ for @announcementSetting@
announcementSettingSelector :: Selector
announcementSettingSelector = mkSelector "announcementSetting"

-- | @Selector@ for @timeSensitiveSetting@
timeSensitiveSettingSelector :: Selector
timeSensitiveSettingSelector = mkSelector "timeSensitiveSetting"

-- | @Selector@ for @scheduledDeliverySetting@
scheduledDeliverySettingSelector :: Selector
scheduledDeliverySettingSelector = mkSelector "scheduledDeliverySetting"

-- | @Selector@ for @directMessagesSetting@
directMessagesSettingSelector :: Selector
directMessagesSettingSelector = mkSelector "directMessagesSetting"

