{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSFilePromiseProvider@.
module ObjC.AppKit.NSFilePromiseProvider
  ( NSFilePromiseProvider
  , IsNSFilePromiseProvider(..)
  , initWithFileType_delegate
  , init_
  , fileType
  , setFileType
  , delegate
  , setDelegate
  , userInfo
  , setUserInfo
  , delegateSelector
  , fileTypeSelector
  , initSelector
  , initWithFileType_delegateSelector
  , setDelegateSelector
  , setFileTypeSelector
  , setUserInfoSelector
  , userInfoSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithFileType:delegate:@
initWithFileType_delegate :: (IsNSFilePromiseProvider nsFilePromiseProvider, IsNSString fileType) => nsFilePromiseProvider -> fileType -> RawId -> IO (Id NSFilePromiseProvider)
initWithFileType_delegate nsFilePromiseProvider fileType delegate =
  sendOwnedMessage nsFilePromiseProvider initWithFileType_delegateSelector (toNSString fileType) delegate

-- | @- init@
init_ :: IsNSFilePromiseProvider nsFilePromiseProvider => nsFilePromiseProvider -> IO (Id NSFilePromiseProvider)
init_ nsFilePromiseProvider =
  sendOwnedMessage nsFilePromiseProvider initSelector

-- | @- fileType@
fileType :: IsNSFilePromiseProvider nsFilePromiseProvider => nsFilePromiseProvider -> IO (Id NSString)
fileType nsFilePromiseProvider =
  sendMessage nsFilePromiseProvider fileTypeSelector

-- | @- setFileType:@
setFileType :: (IsNSFilePromiseProvider nsFilePromiseProvider, IsNSString value) => nsFilePromiseProvider -> value -> IO ()
setFileType nsFilePromiseProvider value =
  sendMessage nsFilePromiseProvider setFileTypeSelector (toNSString value)

-- | @- delegate@
delegate :: IsNSFilePromiseProvider nsFilePromiseProvider => nsFilePromiseProvider -> IO RawId
delegate nsFilePromiseProvider =
  sendMessage nsFilePromiseProvider delegateSelector

-- | @- setDelegate:@
setDelegate :: IsNSFilePromiseProvider nsFilePromiseProvider => nsFilePromiseProvider -> RawId -> IO ()
setDelegate nsFilePromiseProvider value =
  sendMessage nsFilePromiseProvider setDelegateSelector value

-- | @- userInfo@
userInfo :: IsNSFilePromiseProvider nsFilePromiseProvider => nsFilePromiseProvider -> IO RawId
userInfo nsFilePromiseProvider =
  sendMessage nsFilePromiseProvider userInfoSelector

-- | @- setUserInfo:@
setUserInfo :: IsNSFilePromiseProvider nsFilePromiseProvider => nsFilePromiseProvider -> RawId -> IO ()
setUserInfo nsFilePromiseProvider value =
  sendMessage nsFilePromiseProvider setUserInfoSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithFileType:delegate:@
initWithFileType_delegateSelector :: Selector '[Id NSString, RawId] (Id NSFilePromiseProvider)
initWithFileType_delegateSelector = mkSelector "initWithFileType:delegate:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id NSFilePromiseProvider)
initSelector = mkSelector "init"

-- | @Selector@ for @fileType@
fileTypeSelector :: Selector '[] (Id NSString)
fileTypeSelector = mkSelector "fileType"

-- | @Selector@ for @setFileType:@
setFileTypeSelector :: Selector '[Id NSString] ()
setFileTypeSelector = mkSelector "setFileType:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @userInfo@
userInfoSelector :: Selector '[] RawId
userInfoSelector = mkSelector "userInfo"

-- | @Selector@ for @setUserInfo:@
setUserInfoSelector :: Selector '[RawId] ()
setUserInfoSelector = mkSelector "setUserInfo:"

