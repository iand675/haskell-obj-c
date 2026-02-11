{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSExtensionItem@.
module ObjC.Foundation.NSExtensionItem
  ( NSExtensionItem
  , IsNSExtensionItem(..)
  , attributedTitle
  , setAttributedTitle
  , attributedContentText
  , setAttributedContentText
  , attachments
  , setAttachments
  , userInfo
  , setUserInfo
  , attributedTitleSelector
  , setAttributedTitleSelector
  , attributedContentTextSelector
  , setAttributedContentTextSelector
  , attachmentsSelector
  , setAttachmentsSelector
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

import ObjC.Foundation.Internal.Classes

-- | @- attributedTitle@
attributedTitle :: IsNSExtensionItem nsExtensionItem => nsExtensionItem -> IO (Id NSAttributedString)
attributedTitle nsExtensionItem  =
  sendMsg nsExtensionItem (mkSelector "attributedTitle") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAttributedTitle:@
setAttributedTitle :: (IsNSExtensionItem nsExtensionItem, IsNSAttributedString value) => nsExtensionItem -> value -> IO ()
setAttributedTitle nsExtensionItem  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsExtensionItem (mkSelector "setAttributedTitle:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- attributedContentText@
attributedContentText :: IsNSExtensionItem nsExtensionItem => nsExtensionItem -> IO (Id NSAttributedString)
attributedContentText nsExtensionItem  =
  sendMsg nsExtensionItem (mkSelector "attributedContentText") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAttributedContentText:@
setAttributedContentText :: (IsNSExtensionItem nsExtensionItem, IsNSAttributedString value) => nsExtensionItem -> value -> IO ()
setAttributedContentText nsExtensionItem  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsExtensionItem (mkSelector "setAttributedContentText:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- attachments@
attachments :: IsNSExtensionItem nsExtensionItem => nsExtensionItem -> IO (Id NSArray)
attachments nsExtensionItem  =
  sendMsg nsExtensionItem (mkSelector "attachments") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAttachments:@
setAttachments :: (IsNSExtensionItem nsExtensionItem, IsNSArray value) => nsExtensionItem -> value -> IO ()
setAttachments nsExtensionItem  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsExtensionItem (mkSelector "setAttachments:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- userInfo@
userInfo :: IsNSExtensionItem nsExtensionItem => nsExtensionItem -> IO (Id NSDictionary)
userInfo nsExtensionItem  =
  sendMsg nsExtensionItem (mkSelector "userInfo") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setUserInfo:@
setUserInfo :: (IsNSExtensionItem nsExtensionItem, IsNSDictionary value) => nsExtensionItem -> value -> IO ()
setUserInfo nsExtensionItem  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsExtensionItem (mkSelector "setUserInfo:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @attributedTitle@
attributedTitleSelector :: Selector
attributedTitleSelector = mkSelector "attributedTitle"

-- | @Selector@ for @setAttributedTitle:@
setAttributedTitleSelector :: Selector
setAttributedTitleSelector = mkSelector "setAttributedTitle:"

-- | @Selector@ for @attributedContentText@
attributedContentTextSelector :: Selector
attributedContentTextSelector = mkSelector "attributedContentText"

-- | @Selector@ for @setAttributedContentText:@
setAttributedContentTextSelector :: Selector
setAttributedContentTextSelector = mkSelector "setAttributedContentText:"

-- | @Selector@ for @attachments@
attachmentsSelector :: Selector
attachmentsSelector = mkSelector "attachments"

-- | @Selector@ for @setAttachments:@
setAttachmentsSelector :: Selector
setAttachmentsSelector = mkSelector "setAttachments:"

-- | @Selector@ for @userInfo@
userInfoSelector :: Selector
userInfoSelector = mkSelector "userInfo"

-- | @Selector@ for @setUserInfo:@
setUserInfoSelector :: Selector
setUserInfoSelector = mkSelector "setUserInfo:"

