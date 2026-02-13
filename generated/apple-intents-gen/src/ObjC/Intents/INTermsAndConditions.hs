{-# LANGUAGE DataKinds #-}
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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Intents.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithLocalizedTermsAndConditionsText:privacyPolicyURL:termsAndConditionsURL:@
initWithLocalizedTermsAndConditionsText_privacyPolicyURL_termsAndConditionsURL :: (IsINTermsAndConditions inTermsAndConditions, IsNSString localizedTermsAndConditionsText, IsNSURL privacyPolicyURL, IsNSURL termsAndConditionsURL) => inTermsAndConditions -> localizedTermsAndConditionsText -> privacyPolicyURL -> termsAndConditionsURL -> IO (Id INTermsAndConditions)
initWithLocalizedTermsAndConditionsText_privacyPolicyURL_termsAndConditionsURL inTermsAndConditions localizedTermsAndConditionsText privacyPolicyURL termsAndConditionsURL =
  sendOwnedMessage inTermsAndConditions initWithLocalizedTermsAndConditionsText_privacyPolicyURL_termsAndConditionsURLSelector (toNSString localizedTermsAndConditionsText) (toNSURL privacyPolicyURL) (toNSURL termsAndConditionsURL)

-- | @- localizedTermsAndConditionsText@
localizedTermsAndConditionsText :: IsINTermsAndConditions inTermsAndConditions => inTermsAndConditions -> IO (Id NSString)
localizedTermsAndConditionsText inTermsAndConditions =
  sendMessage inTermsAndConditions localizedTermsAndConditionsTextSelector

-- | @- privacyPolicyURL@
privacyPolicyURL :: IsINTermsAndConditions inTermsAndConditions => inTermsAndConditions -> IO (Id NSURL)
privacyPolicyURL inTermsAndConditions =
  sendMessage inTermsAndConditions privacyPolicyURLSelector

-- | @- termsAndConditionsURL@
termsAndConditionsURL :: IsINTermsAndConditions inTermsAndConditions => inTermsAndConditions -> IO (Id NSURL)
termsAndConditionsURL inTermsAndConditions =
  sendMessage inTermsAndConditions termsAndConditionsURLSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithLocalizedTermsAndConditionsText:privacyPolicyURL:termsAndConditionsURL:@
initWithLocalizedTermsAndConditionsText_privacyPolicyURL_termsAndConditionsURLSelector :: Selector '[Id NSString, Id NSURL, Id NSURL] (Id INTermsAndConditions)
initWithLocalizedTermsAndConditionsText_privacyPolicyURL_termsAndConditionsURLSelector = mkSelector "initWithLocalizedTermsAndConditionsText:privacyPolicyURL:termsAndConditionsURL:"

-- | @Selector@ for @localizedTermsAndConditionsText@
localizedTermsAndConditionsTextSelector :: Selector '[] (Id NSString)
localizedTermsAndConditionsTextSelector = mkSelector "localizedTermsAndConditionsText"

-- | @Selector@ for @privacyPolicyURL@
privacyPolicyURLSelector :: Selector '[] (Id NSURL)
privacyPolicyURLSelector = mkSelector "privacyPolicyURL"

-- | @Selector@ for @termsAndConditionsURL@
termsAndConditionsURLSelector :: Selector '[] (Id NSURL)
termsAndConditionsURLSelector = mkSelector "termsAndConditionsURL"

