{-# LANGUAGE DataKinds #-}
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
  , configurationSelector
  , initSelector
  , initWithSectionProviderSelector
  , initWithSectionProvider_configurationSelector
  , initWithSectionSelector
  , initWithSection_configurationSelector
  , newSelector
  , setConfigurationSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithSection:@
initWithSection :: (IsNSCollectionViewCompositionalLayout nsCollectionViewCompositionalLayout, IsNSCollectionLayoutSection section) => nsCollectionViewCompositionalLayout -> section -> IO (Id NSCollectionViewCompositionalLayout)
initWithSection nsCollectionViewCompositionalLayout section =
  sendOwnedMessage nsCollectionViewCompositionalLayout initWithSectionSelector (toNSCollectionLayoutSection section)

-- | @- initWithSection:configuration:@
initWithSection_configuration :: (IsNSCollectionViewCompositionalLayout nsCollectionViewCompositionalLayout, IsNSCollectionLayoutSection section, IsNSCollectionViewCompositionalLayoutConfiguration configuration) => nsCollectionViewCompositionalLayout -> section -> configuration -> IO (Id NSCollectionViewCompositionalLayout)
initWithSection_configuration nsCollectionViewCompositionalLayout section configuration =
  sendOwnedMessage nsCollectionViewCompositionalLayout initWithSection_configurationSelector (toNSCollectionLayoutSection section) (toNSCollectionViewCompositionalLayoutConfiguration configuration)

-- | @- initWithSectionProvider:@
initWithSectionProvider :: IsNSCollectionViewCompositionalLayout nsCollectionViewCompositionalLayout => nsCollectionViewCompositionalLayout -> Ptr () -> IO (Id NSCollectionViewCompositionalLayout)
initWithSectionProvider nsCollectionViewCompositionalLayout sectionProvider =
  sendOwnedMessage nsCollectionViewCompositionalLayout initWithSectionProviderSelector sectionProvider

-- | @- initWithSectionProvider:configuration:@
initWithSectionProvider_configuration :: (IsNSCollectionViewCompositionalLayout nsCollectionViewCompositionalLayout, IsNSCollectionViewCompositionalLayoutConfiguration configuration) => nsCollectionViewCompositionalLayout -> Ptr () -> configuration -> IO (Id NSCollectionViewCompositionalLayout)
initWithSectionProvider_configuration nsCollectionViewCompositionalLayout sectionProvider configuration =
  sendOwnedMessage nsCollectionViewCompositionalLayout initWithSectionProvider_configurationSelector sectionProvider (toNSCollectionViewCompositionalLayoutConfiguration configuration)

-- | @- init@
init_ :: IsNSCollectionViewCompositionalLayout nsCollectionViewCompositionalLayout => nsCollectionViewCompositionalLayout -> IO (Id NSCollectionViewCompositionalLayout)
init_ nsCollectionViewCompositionalLayout =
  sendOwnedMessage nsCollectionViewCompositionalLayout initSelector

-- | @+ new@
new :: IO (Id NSCollectionViewCompositionalLayout)
new  =
  do
    cls' <- getRequiredClass "NSCollectionViewCompositionalLayout"
    sendOwnedClassMessage cls' newSelector

-- | @- configuration@
configuration :: IsNSCollectionViewCompositionalLayout nsCollectionViewCompositionalLayout => nsCollectionViewCompositionalLayout -> IO (Id NSCollectionViewCompositionalLayoutConfiguration)
configuration nsCollectionViewCompositionalLayout =
  sendMessage nsCollectionViewCompositionalLayout configurationSelector

-- | @- setConfiguration:@
setConfiguration :: (IsNSCollectionViewCompositionalLayout nsCollectionViewCompositionalLayout, IsNSCollectionViewCompositionalLayoutConfiguration value) => nsCollectionViewCompositionalLayout -> value -> IO ()
setConfiguration nsCollectionViewCompositionalLayout value =
  sendMessage nsCollectionViewCompositionalLayout setConfigurationSelector (toNSCollectionViewCompositionalLayoutConfiguration value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithSection:@
initWithSectionSelector :: Selector '[Id NSCollectionLayoutSection] (Id NSCollectionViewCompositionalLayout)
initWithSectionSelector = mkSelector "initWithSection:"

-- | @Selector@ for @initWithSection:configuration:@
initWithSection_configurationSelector :: Selector '[Id NSCollectionLayoutSection, Id NSCollectionViewCompositionalLayoutConfiguration] (Id NSCollectionViewCompositionalLayout)
initWithSection_configurationSelector = mkSelector "initWithSection:configuration:"

-- | @Selector@ for @initWithSectionProvider:@
initWithSectionProviderSelector :: Selector '[Ptr ()] (Id NSCollectionViewCompositionalLayout)
initWithSectionProviderSelector = mkSelector "initWithSectionProvider:"

-- | @Selector@ for @initWithSectionProvider:configuration:@
initWithSectionProvider_configurationSelector :: Selector '[Ptr (), Id NSCollectionViewCompositionalLayoutConfiguration] (Id NSCollectionViewCompositionalLayout)
initWithSectionProvider_configurationSelector = mkSelector "initWithSectionProvider:configuration:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id NSCollectionViewCompositionalLayout)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id NSCollectionViewCompositionalLayout)
newSelector = mkSelector "new"

-- | @Selector@ for @configuration@
configurationSelector :: Selector '[] (Id NSCollectionViewCompositionalLayoutConfiguration)
configurationSelector = mkSelector "configuration"

-- | @Selector@ for @setConfiguration:@
setConfigurationSelector :: Selector '[Id NSCollectionViewCompositionalLayoutConfiguration] ()
setConfigurationSelector = mkSelector "setConfiguration:"

