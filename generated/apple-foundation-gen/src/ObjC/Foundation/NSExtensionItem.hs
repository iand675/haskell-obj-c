{-# LANGUAGE DataKinds #-}
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
  , attachmentsSelector
  , attributedContentTextSelector
  , attributedTitleSelector
  , setAttachmentsSelector
  , setAttributedContentTextSelector
  , setAttributedTitleSelector
  , setUserInfoSelector
  , userInfoSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes

-- | @- attributedTitle@
attributedTitle :: IsNSExtensionItem nsExtensionItem => nsExtensionItem -> IO (Id NSAttributedString)
attributedTitle nsExtensionItem =
  sendMessage nsExtensionItem attributedTitleSelector

-- | @- setAttributedTitle:@
setAttributedTitle :: (IsNSExtensionItem nsExtensionItem, IsNSAttributedString value) => nsExtensionItem -> value -> IO ()
setAttributedTitle nsExtensionItem value =
  sendMessage nsExtensionItem setAttributedTitleSelector (toNSAttributedString value)

-- | @- attributedContentText@
attributedContentText :: IsNSExtensionItem nsExtensionItem => nsExtensionItem -> IO (Id NSAttributedString)
attributedContentText nsExtensionItem =
  sendMessage nsExtensionItem attributedContentTextSelector

-- | @- setAttributedContentText:@
setAttributedContentText :: (IsNSExtensionItem nsExtensionItem, IsNSAttributedString value) => nsExtensionItem -> value -> IO ()
setAttributedContentText nsExtensionItem value =
  sendMessage nsExtensionItem setAttributedContentTextSelector (toNSAttributedString value)

-- | @- attachments@
attachments :: IsNSExtensionItem nsExtensionItem => nsExtensionItem -> IO (Id NSArray)
attachments nsExtensionItem =
  sendMessage nsExtensionItem attachmentsSelector

-- | @- setAttachments:@
setAttachments :: (IsNSExtensionItem nsExtensionItem, IsNSArray value) => nsExtensionItem -> value -> IO ()
setAttachments nsExtensionItem value =
  sendMessage nsExtensionItem setAttachmentsSelector (toNSArray value)

-- | @- userInfo@
userInfo :: IsNSExtensionItem nsExtensionItem => nsExtensionItem -> IO (Id NSDictionary)
userInfo nsExtensionItem =
  sendMessage nsExtensionItem userInfoSelector

-- | @- setUserInfo:@
setUserInfo :: (IsNSExtensionItem nsExtensionItem, IsNSDictionary value) => nsExtensionItem -> value -> IO ()
setUserInfo nsExtensionItem value =
  sendMessage nsExtensionItem setUserInfoSelector (toNSDictionary value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @attributedTitle@
attributedTitleSelector :: Selector '[] (Id NSAttributedString)
attributedTitleSelector = mkSelector "attributedTitle"

-- | @Selector@ for @setAttributedTitle:@
setAttributedTitleSelector :: Selector '[Id NSAttributedString] ()
setAttributedTitleSelector = mkSelector "setAttributedTitle:"

-- | @Selector@ for @attributedContentText@
attributedContentTextSelector :: Selector '[] (Id NSAttributedString)
attributedContentTextSelector = mkSelector "attributedContentText"

-- | @Selector@ for @setAttributedContentText:@
setAttributedContentTextSelector :: Selector '[Id NSAttributedString] ()
setAttributedContentTextSelector = mkSelector "setAttributedContentText:"

-- | @Selector@ for @attachments@
attachmentsSelector :: Selector '[] (Id NSArray)
attachmentsSelector = mkSelector "attachments"

-- | @Selector@ for @setAttachments:@
setAttachmentsSelector :: Selector '[Id NSArray] ()
setAttachmentsSelector = mkSelector "setAttachments:"

-- | @Selector@ for @userInfo@
userInfoSelector :: Selector '[] (Id NSDictionary)
userInfoSelector = mkSelector "userInfo"

-- | @Selector@ for @setUserInfo:@
setUserInfoSelector :: Selector '[Id NSDictionary] ()
setUserInfoSelector = mkSelector "setUserInfo:"

