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
  , sharedAccessoryManagerSelector
  , showBluetoothAccessoryPickerWithNameFilter_completionSelector
  , registerForLocalNotificationsSelector
  , unregisterForLocalNotificationsSelector


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

import ObjC.ExternalAccessory.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ sharedAccessoryManager@
sharedAccessoryManager :: IO (Id EAAccessoryManager)
sharedAccessoryManager  =
  do
    cls' <- getRequiredClass "EAAccessoryManager"
    sendClassMsg cls' (mkSelector "sharedAccessoryManager") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- showBluetoothAccessoryPickerWithNameFilter:completion:@
showBluetoothAccessoryPickerWithNameFilter_completion :: (IsEAAccessoryManager eaAccessoryManager, IsNSPredicate predicate) => eaAccessoryManager -> predicate -> Ptr () -> IO ()
showBluetoothAccessoryPickerWithNameFilter_completion eaAccessoryManager  predicate completion =
withObjCPtr predicate $ \raw_predicate ->
    sendMsg eaAccessoryManager (mkSelector "showBluetoothAccessoryPickerWithNameFilter:completion:") retVoid [argPtr (castPtr raw_predicate :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- registerForLocalNotifications@
registerForLocalNotifications :: IsEAAccessoryManager eaAccessoryManager => eaAccessoryManager -> IO ()
registerForLocalNotifications eaAccessoryManager  =
  sendMsg eaAccessoryManager (mkSelector "registerForLocalNotifications") retVoid []

-- | @- unregisterForLocalNotifications@
unregisterForLocalNotifications :: IsEAAccessoryManager eaAccessoryManager => eaAccessoryManager -> IO ()
unregisterForLocalNotifications eaAccessoryManager  =
  sendMsg eaAccessoryManager (mkSelector "unregisterForLocalNotifications") retVoid []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sharedAccessoryManager@
sharedAccessoryManagerSelector :: Selector
sharedAccessoryManagerSelector = mkSelector "sharedAccessoryManager"

-- | @Selector@ for @showBluetoothAccessoryPickerWithNameFilter:completion:@
showBluetoothAccessoryPickerWithNameFilter_completionSelector :: Selector
showBluetoothAccessoryPickerWithNameFilter_completionSelector = mkSelector "showBluetoothAccessoryPickerWithNameFilter:completion:"

-- | @Selector@ for @registerForLocalNotifications@
registerForLocalNotificationsSelector :: Selector
registerForLocalNotificationsSelector = mkSelector "registerForLocalNotifications"

-- | @Selector@ for @unregisterForLocalNotifications@
unregisterForLocalNotificationsSelector :: Selector
unregisterForLocalNotificationsSelector = mkSelector "unregisterForLocalNotifications"

