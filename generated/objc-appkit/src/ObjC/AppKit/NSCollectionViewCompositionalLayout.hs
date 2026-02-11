{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSCollectionViewCompositionalLayout@.
module ObjC.AppKit.NSCollectionViewCompositionalLayout
  ( NSCollectionViewCompositionalLayout
  , IsNSCollectionViewCompositionalLayout(..)
  , initWithSection
  , initWithSection_configuration
  , initWithSectionProvider
  , initWithSectionProvider_configuration
  , init_
  , new
  , configuration
  , setConfiguration
  , initWithSectionSelector
  , initWithSection_configurationSelector
  , initWithSectionProviderSelector
  , initWithSectionProvider_configurationSelector
  , initSelector
  , newSelector
  , configurationSelector
  , setConfigurationSelector


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

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithSection:@
initWithSection :: (IsNSCollectionViewCompositionalLayout nsCollectionViewCompositionalLayout, IsNSCollectionLayoutSection section) => nsCollectionViewCompositionalLayout -> section -> IO (Id NSCollectionViewCompositionalLayout)
initWithSection nsCollectionViewCompositionalLayout  section =
withObjCPtr section $ \raw_section ->
    sendMsg nsCollectionViewCompositionalLayout (mkSelector "initWithSection:") (retPtr retVoid) [argPtr (castPtr raw_section :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithSection:configuration:@
initWithSection_configuration :: (IsNSCollectionViewCompositionalLayout nsCollectionViewCompositionalLayout, IsNSCollectionLayoutSection section, IsNSCollectionViewCompositionalLayoutConfiguration configuration) => nsCollectionViewCompositionalLayout -> section -> configuration -> IO (Id NSCollectionViewCompositionalLayout)
initWithSection_configuration nsCollectionViewCompositionalLayout  section configuration =
withObjCPtr section $ \raw_section ->
  withObjCPtr configuration $ \raw_configuration ->
      sendMsg nsCollectionViewCompositionalLayout (mkSelector "initWithSection:configuration:") (retPtr retVoid) [argPtr (castPtr raw_section :: Ptr ()), argPtr (castPtr raw_configuration :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithSectionProvider:@
initWithSectionProvider :: IsNSCollectionViewCompositionalLayout nsCollectionViewCompositionalLayout => nsCollectionViewCompositionalLayout -> Ptr () -> IO (Id NSCollectionViewCompositionalLayout)
initWithSectionProvider nsCollectionViewCompositionalLayout  sectionProvider =
  sendMsg nsCollectionViewCompositionalLayout (mkSelector "initWithSectionProvider:") (retPtr retVoid) [argPtr (castPtr sectionProvider :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithSectionProvider:configuration:@
initWithSectionProvider_configuration :: (IsNSCollectionViewCompositionalLayout nsCollectionViewCompositionalLayout, IsNSCollectionViewCompositionalLayoutConfiguration configuration) => nsCollectionViewCompositionalLayout -> Ptr () -> configuration -> IO (Id NSCollectionViewCompositionalLayout)
initWithSectionProvider_configuration nsCollectionViewCompositionalLayout  sectionProvider configuration =
withObjCPtr configuration $ \raw_configuration ->
    sendMsg nsCollectionViewCompositionalLayout (mkSelector "initWithSectionProvider:configuration:") (retPtr retVoid) [argPtr (castPtr sectionProvider :: Ptr ()), argPtr (castPtr raw_configuration :: Ptr ())] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsNSCollectionViewCompositionalLayout nsCollectionViewCompositionalLayout => nsCollectionViewCompositionalLayout -> IO (Id NSCollectionViewCompositionalLayout)
init_ nsCollectionViewCompositionalLayout  =
  sendMsg nsCollectionViewCompositionalLayout (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id NSCollectionViewCompositionalLayout)
new  =
  do
    cls' <- getRequiredClass "NSCollectionViewCompositionalLayout"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- configuration@
configuration :: IsNSCollectionViewCompositionalLayout nsCollectionViewCompositionalLayout => nsCollectionViewCompositionalLayout -> IO (Id NSCollectionViewCompositionalLayoutConfiguration)
configuration nsCollectionViewCompositionalLayout  =
  sendMsg nsCollectionViewCompositionalLayout (mkSelector "configuration") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setConfiguration:@
setConfiguration :: (IsNSCollectionViewCompositionalLayout nsCollectionViewCompositionalLayout, IsNSCollectionViewCompositionalLayoutConfiguration value) => nsCollectionViewCompositionalLayout -> value -> IO ()
setConfiguration nsCollectionViewCompositionalLayout  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsCollectionViewCompositionalLayout (mkSelector "setConfiguration:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithSection:@
initWithSectionSelector :: Selector
initWithSectionSelector = mkSelector "initWithSection:"

-- | @Selector@ for @initWithSection:configuration:@
initWithSection_configurationSelector :: Selector
initWithSection_configurationSelector = mkSelector "initWithSection:configuration:"

-- | @Selector@ for @initWithSectionProvider:@
initWithSectionProviderSelector :: Selector
initWithSectionProviderSelector = mkSelector "initWithSectionProvider:"

-- | @Selector@ for @initWithSectionProvider:configuration:@
initWithSectionProvider_configurationSelector :: Selector
initWithSectionProvider_configurationSelector = mkSelector "initWithSectionProvider:configuration:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @configuration@
configurationSelector :: Selector
configurationSelector = mkSelector "configuration"

-- | @Selector@ for @setConfiguration:@
setConfigurationSelector :: Selector
setConfigurationSelector = mkSelector "setConfiguration:"

