{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @JRSAppKitAWT@.
module ObjC.JavaRuntimeSupport.JRSAppKitAWT
  ( JRSAppKitAWT
  , IsJRSAppKitAWT(..)
  , awtAppDelegate
  , registerAWTAppWithOptions
  , markAppIsDaemon
  , awtAppDelegateSelector
  , markAppIsDaemonSelector
  , registerAWTAppWithOptionsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.JavaRuntimeSupport.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ awtAppDelegate@
awtAppDelegate :: IO RawId
awtAppDelegate  =
  do
    cls' <- getRequiredClass "JRSAppKitAWT"
    sendClassMessage cls' awtAppDelegateSelector

-- | @+ registerAWTAppWithOptions:@
registerAWTAppWithOptions :: IsNSDictionary options => options -> IO ()
registerAWTAppWithOptions options =
  do
    cls' <- getRequiredClass "JRSAppKitAWT"
    sendClassMessage cls' registerAWTAppWithOptionsSelector (toNSDictionary options)

-- | @+ markAppIsDaemon@
markAppIsDaemon :: IO Bool
markAppIsDaemon  =
  do
    cls' <- getRequiredClass "JRSAppKitAWT"
    sendClassMessage cls' markAppIsDaemonSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @awtAppDelegate@
awtAppDelegateSelector :: Selector '[] RawId
awtAppDelegateSelector = mkSelector "awtAppDelegate"

-- | @Selector@ for @registerAWTAppWithOptions:@
registerAWTAppWithOptionsSelector :: Selector '[Id NSDictionary] ()
registerAWTAppWithOptionsSelector = mkSelector "registerAWTAppWithOptions:"

-- | @Selector@ for @markAppIsDaemon@
markAppIsDaemonSelector :: Selector '[] Bool
markAppIsDaemonSelector = mkSelector "markAppIsDaemon"

