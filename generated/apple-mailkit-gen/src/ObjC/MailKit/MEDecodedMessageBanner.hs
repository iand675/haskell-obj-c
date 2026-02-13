{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Contains security information in order to populate a banner in the message view.
--
-- Generated bindings for @MEDecodedMessageBanner@.
module ObjC.MailKit.MEDecodedMessageBanner
  ( MEDecodedMessageBanner
  , IsMEDecodedMessageBanner(..)
  , new
  , init_
  , initWithTitle_primaryActionTitle_dismissable
  , title
  , primaryActionTitle
  , dismissable
  , dismissableSelector
  , initSelector
  , initWithTitle_primaryActionTitle_dismissableSelector
  , newSelector
  , primaryActionTitleSelector
  , titleSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MailKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ new@
new :: IO (Id MEDecodedMessageBanner)
new  =
  do
    cls' <- getRequiredClass "MEDecodedMessageBanner"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsMEDecodedMessageBanner meDecodedMessageBanner => meDecodedMessageBanner -> IO (Id MEDecodedMessageBanner)
init_ meDecodedMessageBanner =
  sendOwnedMessage meDecodedMessageBanner initSelector

-- | @- initWithTitle:primaryActionTitle:dismissable:@
initWithTitle_primaryActionTitle_dismissable :: (IsMEDecodedMessageBanner meDecodedMessageBanner, IsNSString title, IsNSString primaryActionTitle) => meDecodedMessageBanner -> title -> primaryActionTitle -> Bool -> IO (Id MEDecodedMessageBanner)
initWithTitle_primaryActionTitle_dismissable meDecodedMessageBanner title primaryActionTitle dismissable =
  sendOwnedMessage meDecodedMessageBanner initWithTitle_primaryActionTitle_dismissableSelector (toNSString title) (toNSString primaryActionTitle) dismissable

-- | @- title@
title :: IsMEDecodedMessageBanner meDecodedMessageBanner => meDecodedMessageBanner -> IO (Id NSString)
title meDecodedMessageBanner =
  sendMessage meDecodedMessageBanner titleSelector

-- | @- primaryActionTitle@
primaryActionTitle :: IsMEDecodedMessageBanner meDecodedMessageBanner => meDecodedMessageBanner -> IO (Id NSString)
primaryActionTitle meDecodedMessageBanner =
  sendMessage meDecodedMessageBanner primaryActionTitleSelector

-- | @- dismissable@
dismissable :: IsMEDecodedMessageBanner meDecodedMessageBanner => meDecodedMessageBanner -> IO Bool
dismissable meDecodedMessageBanner =
  sendMessage meDecodedMessageBanner dismissableSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MEDecodedMessageBanner)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MEDecodedMessageBanner)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithTitle:primaryActionTitle:dismissable:@
initWithTitle_primaryActionTitle_dismissableSelector :: Selector '[Id NSString, Id NSString, Bool] (Id MEDecodedMessageBanner)
initWithTitle_primaryActionTitle_dismissableSelector = mkSelector "initWithTitle:primaryActionTitle:dismissable:"

-- | @Selector@ for @title@
titleSelector :: Selector '[] (Id NSString)
titleSelector = mkSelector "title"

-- | @Selector@ for @primaryActionTitle@
primaryActionTitleSelector :: Selector '[] (Id NSString)
primaryActionTitleSelector = mkSelector "primaryActionTitle"

-- | @Selector@ for @dismissable@
dismissableSelector :: Selector '[] Bool
dismissableSelector = mkSelector "dismissable"

