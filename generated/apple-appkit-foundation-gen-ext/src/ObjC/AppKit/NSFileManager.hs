{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSFileManager@.
module ObjC.AppKit.NSFileManager
  ( NSFileManager
  , IsNSFileManager(..)
  , fileManagerWithAuthorization
  , fileManagerWithAuthorizationSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ fileManagerWithAuthorization:@
fileManagerWithAuthorization :: IsNSWorkspaceAuthorization authorization => authorization -> IO (Id NSFileManager)
fileManagerWithAuthorization authorization =
  do
    cls' <- getRequiredClass "NSFileManager"
    sendClassMessage cls' fileManagerWithAuthorizationSelector (toNSWorkspaceAuthorization authorization)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @fileManagerWithAuthorization:@
fileManagerWithAuthorizationSelector :: Selector '[Id NSWorkspaceAuthorization] (Id NSFileManager)
fileManagerWithAuthorizationSelector = mkSelector "fileManagerWithAuthorization:"

