{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSInflectionRule@.
module ObjC.Foundation.NSInflectionRule
  ( NSInflectionRule
  , IsNSInflectionRule(..)
  , init_
  , canInflectLanguage
  , automaticRule
  , canInflectPreferredLocalization
  , automaticRuleSelector
  , canInflectLanguageSelector
  , canInflectPreferredLocalizationSelector
  , initSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsNSInflectionRule nsInflectionRule => nsInflectionRule -> IO RawId
init_ nsInflectionRule =
  sendOwnedMessage nsInflectionRule initSelector

-- | @+ canInflectLanguage:@
canInflectLanguage :: IsNSString language => language -> IO Bool
canInflectLanguage language =
  do
    cls' <- getRequiredClass "NSInflectionRule"
    sendClassMessage cls' canInflectLanguageSelector (toNSString language)

-- | @+ automaticRule@
automaticRule :: IO (Id NSInflectionRule)
automaticRule  =
  do
    cls' <- getRequiredClass "NSInflectionRule"
    sendClassMessage cls' automaticRuleSelector

-- | @+ canInflectPreferredLocalization@
canInflectPreferredLocalization :: IO Bool
canInflectPreferredLocalization  =
  do
    cls' <- getRequiredClass "NSInflectionRule"
    sendClassMessage cls' canInflectPreferredLocalizationSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] RawId
initSelector = mkSelector "init"

-- | @Selector@ for @canInflectLanguage:@
canInflectLanguageSelector :: Selector '[Id NSString] Bool
canInflectLanguageSelector = mkSelector "canInflectLanguage:"

-- | @Selector@ for @automaticRule@
automaticRuleSelector :: Selector '[] (Id NSInflectionRule)
automaticRuleSelector = mkSelector "automaticRule"

-- | @Selector@ for @canInflectPreferredLocalization@
canInflectPreferredLocalizationSelector :: Selector '[] Bool
canInflectPreferredLocalizationSelector = mkSelector "canInflectPreferredLocalization"

