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

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ fileManagerWithAuthorization:@
fileManagerWithAuthorization :: IsNSWorkspaceAuthorization authorization => authorization -> IO (Id NSFileManager)
fileManagerWithAuthorization authorization =
  do
    cls' <- getRequiredClass "NSFileManager"
    withObjCPtr authorization $ \raw_authorization ->
      sendClassMsg cls' (mkSelector "fileManagerWithAuthorization:") (retPtr retVoid) [argPtr (castPtr raw_authorization :: Ptr ())] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @fileManagerWithAuthorization:@
fileManagerWithAuthorizationSelector :: Selector
fileManagerWithAuthorizationSelector = mkSelector "fileManagerWithAuthorization:"

