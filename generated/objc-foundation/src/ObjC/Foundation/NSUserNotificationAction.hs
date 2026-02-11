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

-- | @+ actionWithIdentifier:title:@
actionWithIdentifier_title :: (IsNSString identifier, IsNSString title) => identifier -> title -> IO (Id NSUserNotificationAction)
actionWithIdentifier_title identifier title =
  do
    cls' <- getRequiredClass "NSUserNotificationAction"
    withObjCPtr identifier $ \raw_identifier ->
      withObjCPtr title $ \raw_title ->
        sendClassMsg cls' (mkSelector "actionWithIdentifier:title:") (retPtr retVoid) [argPtr (castPtr raw_identifier :: Ptr ()), argPtr (castPtr raw_title :: Ptr ())] >>= retainedObject . castPtr

-- | @- identifier@
identifier :: IsNSUserNotificationAction nsUserNotificationAction => nsUserNotificationAction -> IO (Id NSString)
identifier nsUserNotificationAction  =
  sendMsg nsUserNotificationAction (mkSelector "identifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- title@
title :: IsNSUserNotificationAction nsUserNotificationAction => nsUserNotificationAction -> IO (Id NSString)
title nsUserNotificationAction  =
  sendMsg nsUserNotificationAction (mkSelector "title") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @actionWithIdentifier:title:@
actionWithIdentifier_titleSelector :: Selector
actionWithIdentifier_titleSelector = mkSelector "actionWithIdentifier:title:"

-- | @Selector@ for @identifier@
identifierSelector :: Selector
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @title@
titleSelector :: Selector
titleSelector = mkSelector "title"

