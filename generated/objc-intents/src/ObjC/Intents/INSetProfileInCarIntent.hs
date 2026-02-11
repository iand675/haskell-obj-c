{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INSetProfileInCarIntent@.
module ObjC.Intents.INSetProfileInCarIntent
  ( INSetProfileInCarIntent
  , IsINSetProfileInCarIntent(..)
  , initWithProfileNumber_profileName_defaultProfile_carName
  , initWithProfileNumber_profileLabel_defaultProfile
  , initWithProfileNumber_profileName_defaultProfile
  , initWithProfileNumber_profileName_defaultProfile_carNameSelector
  , initWithProfileNumber_profileLabel_defaultProfileSelector
  , initWithProfileNumber_profileName_defaultProfileSelector


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

-- | @- initWithProfileNumber:profileName:defaultProfile:carName:@
initWithProfileNumber_profileName_defaultProfile_carName :: (IsINSetProfileInCarIntent inSetProfileInCarIntent, IsNSNumber profileNumber, IsNSString profileName, IsNSNumber defaultProfile, IsINSpeakableString carName) => inSetProfileInCarIntent -> profileNumber -> profileName -> defaultProfile -> carName -> IO (Id INSetProfileInCarIntent)
initWithProfileNumber_profileName_defaultProfile_carName inSetProfileInCarIntent  profileNumber profileName defaultProfile carName =
withObjCPtr profileNumber $ \raw_profileNumber ->
  withObjCPtr profileName $ \raw_profileName ->
    withObjCPtr defaultProfile $ \raw_defaultProfile ->
      withObjCPtr carName $ \raw_carName ->
          sendMsg inSetProfileInCarIntent (mkSelector "initWithProfileNumber:profileName:defaultProfile:carName:") (retPtr retVoid) [argPtr (castPtr raw_profileNumber :: Ptr ()), argPtr (castPtr raw_profileName :: Ptr ()), argPtr (castPtr raw_defaultProfile :: Ptr ()), argPtr (castPtr raw_carName :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithProfileNumber:profileLabel:defaultProfile:@
initWithProfileNumber_profileLabel_defaultProfile :: (IsINSetProfileInCarIntent inSetProfileInCarIntent, IsNSNumber profileNumber, IsNSString profileLabel, IsNSNumber defaultProfile) => inSetProfileInCarIntent -> profileNumber -> profileLabel -> defaultProfile -> IO (Id INSetProfileInCarIntent)
initWithProfileNumber_profileLabel_defaultProfile inSetProfileInCarIntent  profileNumber profileLabel defaultProfile =
withObjCPtr profileNumber $ \raw_profileNumber ->
  withObjCPtr profileLabel $ \raw_profileLabel ->
    withObjCPtr defaultProfile $ \raw_defaultProfile ->
        sendMsg inSetProfileInCarIntent (mkSelector "initWithProfileNumber:profileLabel:defaultProfile:") (retPtr retVoid) [argPtr (castPtr raw_profileNumber :: Ptr ()), argPtr (castPtr raw_profileLabel :: Ptr ()), argPtr (castPtr raw_defaultProfile :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithProfileNumber:profileName:defaultProfile:@
initWithProfileNumber_profileName_defaultProfile :: (IsINSetProfileInCarIntent inSetProfileInCarIntent, IsNSNumber profileNumber, IsNSString profileName, IsNSNumber defaultProfile) => inSetProfileInCarIntent -> profileNumber -> profileName -> defaultProfile -> IO (Id INSetProfileInCarIntent)
initWithProfileNumber_profileName_defaultProfile inSetProfileInCarIntent  profileNumber profileName defaultProfile =
withObjCPtr profileNumber $ \raw_profileNumber ->
  withObjCPtr profileName $ \raw_profileName ->
    withObjCPtr defaultProfile $ \raw_defaultProfile ->
        sendMsg inSetProfileInCarIntent (mkSelector "initWithProfileNumber:profileName:defaultProfile:") (retPtr retVoid) [argPtr (castPtr raw_profileNumber :: Ptr ()), argPtr (castPtr raw_profileName :: Ptr ()), argPtr (castPtr raw_defaultProfile :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithProfileNumber:profileName:defaultProfile:carName:@
initWithProfileNumber_profileName_defaultProfile_carNameSelector :: Selector
initWithProfileNumber_profileName_defaultProfile_carNameSelector = mkSelector "initWithProfileNumber:profileName:defaultProfile:carName:"

-- | @Selector@ for @initWithProfileNumber:profileLabel:defaultProfile:@
initWithProfileNumber_profileLabel_defaultProfileSelector :: Selector
initWithProfileNumber_profileLabel_defaultProfileSelector = mkSelector "initWithProfileNumber:profileLabel:defaultProfile:"

-- | @Selector@ for @initWithProfileNumber:profileName:defaultProfile:@
initWithProfileNumber_profileName_defaultProfileSelector :: Selector
initWithProfileNumber_profileName_defaultProfileSelector = mkSelector "initWithProfileNumber:profileName:defaultProfile:"

