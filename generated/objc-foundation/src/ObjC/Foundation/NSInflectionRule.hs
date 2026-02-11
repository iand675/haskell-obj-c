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
  , initSelector
  , canInflectLanguageSelector
  , automaticRuleSelector
  , canInflectPreferredLocalizationSelector


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

-- | @- init@
init_ :: IsNSInflectionRule nsInflectionRule => nsInflectionRule -> IO RawId
init_ nsInflectionRule  =
  fmap (RawId . castPtr) $ sendMsg nsInflectionRule (mkSelector "init") (retPtr retVoid) []

-- | @+ canInflectLanguage:@
canInflectLanguage :: IsNSString language => language -> IO Bool
canInflectLanguage language =
  do
    cls' <- getRequiredClass "NSInflectionRule"
    withObjCPtr language $ \raw_language ->
      fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "canInflectLanguage:") retCULong [argPtr (castPtr raw_language :: Ptr ())]

-- | @+ automaticRule@
automaticRule :: IO (Id NSInflectionRule)
automaticRule  =
  do
    cls' <- getRequiredClass "NSInflectionRule"
    sendClassMsg cls' (mkSelector "automaticRule") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ canInflectPreferredLocalization@
canInflectPreferredLocalization :: IO Bool
canInflectPreferredLocalization  =
  do
    cls' <- getRequiredClass "NSInflectionRule"
    fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "canInflectPreferredLocalization") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @canInflectLanguage:@
canInflectLanguageSelector :: Selector
canInflectLanguageSelector = mkSelector "canInflectLanguage:"

-- | @Selector@ for @automaticRule@
automaticRuleSelector :: Selector
automaticRuleSelector = mkSelector "automaticRule"

-- | @Selector@ for @canInflectPreferredLocalization@
canInflectPreferredLocalizationSelector :: Selector
canInflectPreferredLocalizationSelector = mkSelector "canInflectPreferredLocalization"

