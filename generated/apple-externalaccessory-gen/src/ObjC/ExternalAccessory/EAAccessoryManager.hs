{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @EAAccessoryManager@.
module ObjC.ExternalAccessory.EAAccessoryManager
  ( EAAccessoryManager
  , IsEAAccessoryManager(..)
  , sharedAccessoryManager
  , showBluetoothAccessoryPickerWithNameFilter_completion
  , registerForLocalNotifications
  , unregisterForLocalNotifications
  , registerForLocalNotificationsSelector
  , sharedAccessoryManagerSelector
  , showBluetoothAccessoryPickerWithNameFilter_completionSelector
  , unregisterForLocalNotificationsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.ExternalAccessory.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ sharedAccessoryManager@
sharedAccessoryManager :: IO (Id EAAccessoryManager)
sharedAccessoryManager  =
  do
    cls' <- getRequiredClass "EAAccessoryManager"
    sendClassMessage cls' sharedAccessoryManagerSelector

-- | @- showBluetoothAccessoryPickerWithNameFilter:completion:@
showBluetoothAccessoryPickerWithNameFilter_completion :: (IsEAAccessoryManager eaAccessoryManager, IsNSPredicate predicate) => eaAccessoryManager -> predicate -> Ptr () -> IO ()
showBluetoothAccessoryPickerWithNameFilter_completion eaAccessoryManager predicate completion =
  sendMessage eaAccessoryManager showBluetoothAccessoryPickerWithNameFilter_completionSelector (toNSPredicate predicate) completion

-- | @- registerForLocalNotifications@
registerForLocalNotifications :: IsEAAccessoryManager eaAccessoryManager => eaAccessoryManager -> IO ()
registerForLocalNotifications eaAccessoryManager =
  sendMessage eaAccessoryManager registerForLocalNotificationsSelector

-- | @- unregisterForLocalNotifications@
unregisterForLocalNotifications :: IsEAAccessoryManager eaAccessoryManager => eaAccessoryManager -> IO ()
unregisterForLocalNotifications eaAccessoryManager =
  sendMessage eaAccessoryManager unregisterForLocalNotificationsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sharedAccessoryManager@
sharedAccessoryManagerSelector :: Selector '[] (Id EAAccessoryManager)
sharedAccessoryManagerSelector = mkSelector "sharedAccessoryManager"

-- | @Selector@ for @showBluetoothAccessoryPickerWithNameFilter:completion:@
showBluetoothAccessoryPickerWithNameFilter_completionSelector :: Selector '[Id NSPredicate, Ptr ()] ()
showBluetoothAccessoryPickerWithNameFilter_completionSelector = mkSelector "showBluetoothAccessoryPickerWithNameFilter:completion:"

-- | @Selector@ for @registerForLocalNotifications@
registerForLocalNotificationsSelector :: Selector '[] ()
registerForLocalNotificationsSelector = mkSelector "registerForLocalNotifications"

-- | @Selector@ for @unregisterForLocalNotifications@
unregisterForLocalNotificationsSelector :: Selector '[] ()
unregisterForLocalNotificationsSelector = mkSelector "unregisterForLocalNotifications"

