{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INTermsAndConditions@.
module ObjC.Intents.INTermsAndConditions
  ( INTermsAndConditions
  , IsINTermsAndConditions(..)
  , initWithLocalizedTermsAndConditionsText_privacyPolicyURL_termsAndConditionsURL
  , localizedTermsAndConditionsText
  , privacyPolicyURL
  , termsAndConditionsURL
  , initWithLocalizedTermsAndConditionsText_privacyPolicyURL_termsAndConditionsURLSelector
  , localizedTermsAndConditionsTextSelector
  , privacyPolicyURLSelector
  , termsAndConditionsURLSelector


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

-- | @- initWithLocalizedTermsAndConditionsText:privacyPolicyURL:termsAndConditionsURL:@
initWithLocalizedTermsAndConditionsText_privacyPolicyURL_termsAndConditionsURL :: (IsINTermsAndConditions inTermsAndConditions, IsNSString localizedTermsAndConditionsText, IsNSURL privacyPolicyURL, IsNSURL termsAndConditionsURL) => inTermsAndConditions -> localizedTermsAndConditionsText -> privacyPolicyURL -> termsAndConditionsURL -> IO (Id INTermsAndConditions)
initWithLocalizedTermsAndConditionsText_privacyPolicyURL_termsAndConditionsURL inTermsAndConditions  localizedTermsAndConditionsText privacyPolicyURL termsAndConditionsURL =
withObjCPtr localizedTermsAndConditionsText $ \raw_localizedTermsAndConditionsText ->
  withObjCPtr privacyPolicyURL $ \raw_privacyPolicyURL ->
    withObjCPtr termsAndConditionsURL $ \raw_termsAndConditionsURL ->
        sendMsg inTermsAndConditions (mkSelector "initWithLocalizedTermsAndConditionsText:privacyPolicyURL:termsAndConditionsURL:") (retPtr retVoid) [argPtr (castPtr raw_localizedTermsAndConditionsText :: Ptr ()), argPtr (castPtr raw_privacyPolicyURL :: Ptr ()), argPtr (castPtr raw_termsAndConditionsURL :: Ptr ())] >>= ownedObject . castPtr

-- | @- localizedTermsAndConditionsText@
localizedTermsAndConditionsText :: IsINTermsAndConditions inTermsAndConditions => inTermsAndConditions -> IO (Id NSString)
localizedTermsAndConditionsText inTermsAndConditions  =
  sendMsg inTermsAndConditions (mkSelector "localizedTermsAndConditionsText") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- privacyPolicyURL@
privacyPolicyURL :: IsINTermsAndConditions inTermsAndConditions => inTermsAndConditions -> IO (Id NSURL)
privacyPolicyURL inTermsAndConditions  =
  sendMsg inTermsAndConditions (mkSelector "privacyPolicyURL") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- termsAndConditionsURL@
termsAndConditionsURL :: IsINTermsAndConditions inTermsAndConditions => inTermsAndConditions -> IO (Id NSURL)
termsAndConditionsURL inTermsAndConditions  =
  sendMsg inTermsAndConditions (mkSelector "termsAndConditionsURL") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithLocalizedTermsAndConditionsText:privacyPolicyURL:termsAndConditionsURL:@
initWithLocalizedTermsAndConditionsText_privacyPolicyURL_termsAndConditionsURLSelector :: Selector
initWithLocalizedTermsAndConditionsText_privacyPolicyURL_termsAndConditionsURLSelector = mkSelector "initWithLocalizedTermsAndConditionsText:privacyPolicyURL:termsAndConditionsURL:"

-- | @Selector@ for @localizedTermsAndConditionsText@
localizedTermsAndConditionsTextSelector :: Selector
localizedTermsAndConditionsTextSelector = mkSelector "localizedTermsAndConditionsText"

-- | @Selector@ for @privacyPolicyURL@
privacyPolicyURLSelector :: Selector
privacyPolicyURLSelector = mkSelector "privacyPolicyURL"

-- | @Selector@ for @termsAndConditionsURL@
termsAndConditionsURLSelector :: Selector
termsAndConditionsURLSelector = mkSelector "termsAndConditionsURL"

