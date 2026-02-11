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
  , userInfo
  , setUserInfo
  , initWithFileType_delegateSelector
  , initSelector
  , fileTypeSelector
  , setFileTypeSelector
  , userInfoSelector
  , setUserInfoSelector


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

-- | @- initWithFileType:delegate:@
initWithFileType_delegate :: (IsNSFilePromiseProvider nsFilePromiseProvider, IsNSString fileType) => nsFilePromiseProvider -> fileType -> RawId -> IO (Id NSFilePromiseProvider)
initWithFileType_delegate nsFilePromiseProvider  fileType delegate =
withObjCPtr fileType $ \raw_fileType ->
    sendMsg nsFilePromiseProvider (mkSelector "initWithFileType:delegate:") (retPtr retVoid) [argPtr (castPtr raw_fileType :: Ptr ()), argPtr (castPtr (unRawId delegate) :: Ptr ())] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsNSFilePromiseProvider nsFilePromiseProvider => nsFilePromiseProvider -> IO (Id NSFilePromiseProvider)
init_ nsFilePromiseProvider  =
  sendMsg nsFilePromiseProvider (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- fileType@
fileType :: IsNSFilePromiseProvider nsFilePromiseProvider => nsFilePromiseProvider -> IO (Id NSString)
fileType nsFilePromiseProvider  =
  sendMsg nsFilePromiseProvider (mkSelector "fileType") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFileType:@
setFileType :: (IsNSFilePromiseProvider nsFilePromiseProvider, IsNSString value) => nsFilePromiseProvider -> value -> IO ()
setFileType nsFilePromiseProvider  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsFilePromiseProvider (mkSelector "setFileType:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- userInfo@
userInfo :: IsNSFilePromiseProvider nsFilePromiseProvider => nsFilePromiseProvider -> IO RawId
userInfo nsFilePromiseProvider  =
  fmap (RawId . castPtr) $ sendMsg nsFilePromiseProvider (mkSelector "userInfo") (retPtr retVoid) []

-- | @- setUserInfo:@
setUserInfo :: IsNSFilePromiseProvider nsFilePromiseProvider => nsFilePromiseProvider -> RawId -> IO ()
setUserInfo nsFilePromiseProvider  value =
  sendMsg nsFilePromiseProvider (mkSelector "setUserInfo:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithFileType:delegate:@
initWithFileType_delegateSelector :: Selector
initWithFileType_delegateSelector = mkSelector "initWithFileType:delegate:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @fileType@
fileTypeSelector :: Selector
fileTypeSelector = mkSelector "fileType"

-- | @Selector@ for @setFileType:@
setFileTypeSelector :: Selector
setFileTypeSelector = mkSelector "setFileType:"

-- | @Selector@ for @userInfo@
userInfoSelector :: Selector
userInfoSelector = mkSelector "userInfo"

-- | @Selector@ for @setUserInfo:@
setUserInfoSelector :: Selector
setUserInfoSelector = mkSelector "setUserInfo:"

