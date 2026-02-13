{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSUserNotificationAction@.
module ObjC.Foundation.NSUserNotificationAction
  ( NSUserNotificationAction
  , IsNSUserNotificationAction(..)
  , actionWithIdentifier_title
  , identifier
  , title
  , actionWithIdentifier_titleSelector
  , identifierSelector
  , titleSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes

-- | @+ actionWithIdentifier:title:@
actionWithIdentifier_title :: (IsNSString identifier, IsNSString title) => identifier -> title -> IO (Id NSUserNotificationAction)
actionWithIdentifier_title identifier title =
  do
    cls' <- getRequiredClass "NSUserNotificationAction"
    sendClassMessage cls' actionWithIdentifier_titleSelector (toNSString identifier) (toNSString title)

-- | @- identifier@
identifier :: IsNSUserNotificationAction nsUserNotificationAction => nsUserNotificationAction -> IO (Id NSString)
identifier nsUserNotificationAction =
  sendMessage nsUserNotificationAction identifierSelector

-- | @- title@
title :: IsNSUserNotificationAction nsUserNotificationAction => nsUserNotificationAction -> IO (Id NSString)
title nsUserNotificationAction =
  sendMessage nsUserNotificationAction titleSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @actionWithIdentifier:title:@
actionWithIdentifier_titleSelector :: Selector '[Id NSString, Id NSString] (Id NSUserNotificationAction)
actionWithIdentifier_titleSelector = mkSelector "actionWithIdentifier:title:"

-- | @Selector@ for @identifier@
identifierSelector :: Selector '[] (Id NSString)
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @title@
titleSelector :: Selector '[] (Id NSString)
titleSelector = mkSelector "title"

