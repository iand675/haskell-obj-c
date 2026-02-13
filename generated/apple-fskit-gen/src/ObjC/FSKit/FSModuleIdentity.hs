{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An installed file system module.
--
-- Generated bindings for @FSModuleIdentity@.
module ObjC.FSKit.FSModuleIdentity
  ( FSModuleIdentity
  , IsFSModuleIdentity(..)
  , bundleIdentifier
  , url
  , enabled
  , bundleIdentifierSelector
  , enabledSelector
  , urlSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.FSKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | The module's bundle identifier.
--
-- ObjC selector: @- bundleIdentifier@
bundleIdentifier :: IsFSModuleIdentity fsModuleIdentity => fsModuleIdentity -> IO (Id NSString)
bundleIdentifier fsModuleIdentity =
  sendMessage fsModuleIdentity bundleIdentifierSelector

-- | The module's URL.
--
-- ObjC selector: @- url@
url :: IsFSModuleIdentity fsModuleIdentity => fsModuleIdentity -> IO (Id NSURL)
url fsModuleIdentity =
  sendMessage fsModuleIdentity urlSelector

-- | A Boolean value that indicates if the module is enabled.
--
-- ObjC selector: @- enabled@
enabled :: IsFSModuleIdentity fsModuleIdentity => fsModuleIdentity -> IO Bool
enabled fsModuleIdentity =
  sendMessage fsModuleIdentity enabledSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @bundleIdentifier@
bundleIdentifierSelector :: Selector '[] (Id NSString)
bundleIdentifierSelector = mkSelector "bundleIdentifier"

-- | @Selector@ for @url@
urlSelector :: Selector '[] (Id NSURL)
urlSelector = mkSelector "url"

-- | @Selector@ for @enabled@
enabledSelector :: Selector '[] Bool
enabledSelector = mkSelector "enabled"

