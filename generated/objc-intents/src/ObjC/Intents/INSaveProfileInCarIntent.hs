{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INSaveProfileInCarIntent@.
module ObjC.Intents.INSaveProfileInCarIntent
  ( INSaveProfileInCarIntent
  , IsINSaveProfileInCarIntent(..)
  , initWithProfileNumber_profileName
  , initWithProfileNumber_profileLabel
  , initWithProfileNumber_profileNameSelector
  , initWithProfileNumber_profileLabelSelector


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

import ObjC.Intents.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithProfileNumber:profileName:@
initWithProfileNumber_profileName :: (IsINSaveProfileInCarIntent inSaveProfileInCarIntent, IsNSNumber profileNumber, IsNSString profileName) => inSaveProfileInCarIntent -> profileNumber -> profileName -> IO (Id INSaveProfileInCarIntent)
initWithProfileNumber_profileName inSaveProfileInCarIntent  profileNumber profileName =
withObjCPtr profileNumber $ \raw_profileNumber ->
  withObjCPtr profileName $ \raw_profileName ->
      sendMsg inSaveProfileInCarIntent (mkSelector "initWithProfileNumber:profileName:") (retPtr retVoid) [argPtr (castPtr raw_profileNumber :: Ptr ()), argPtr (castPtr raw_profileName :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithProfileNumber:profileLabel:@
initWithProfileNumber_profileLabel :: (IsINSaveProfileInCarIntent inSaveProfileInCarIntent, IsNSNumber profileNumber, IsNSString profileLabel) => inSaveProfileInCarIntent -> profileNumber -> profileLabel -> IO (Id INSaveProfileInCarIntent)
initWithProfileNumber_profileLabel inSaveProfileInCarIntent  profileNumber profileLabel =
withObjCPtr profileNumber $ \raw_profileNumber ->
  withObjCPtr profileLabel $ \raw_profileLabel ->
      sendMsg inSaveProfileInCarIntent (mkSelector "initWithProfileNumber:profileLabel:") (retPtr retVoid) [argPtr (castPtr raw_profileNumber :: Ptr ()), argPtr (castPtr raw_profileLabel :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithProfileNumber:profileName:@
initWithProfileNumber_profileNameSelector :: Selector
initWithProfileNumber_profileNameSelector = mkSelector "initWithProfileNumber:profileName:"

-- | @Selector@ for @initWithProfileNumber:profileLabel:@
initWithProfileNumber_profileLabelSelector :: Selector
initWithProfileNumber_profileLabelSelector = mkSelector "initWithProfileNumber:profileLabel:"

