{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSStoryboard@.
module ObjC.AppKit.NSStoryboard
  ( NSStoryboard
  , IsNSStoryboard(..)
  , storyboardWithName_bundle
  , instantiateInitialController
  , instantiateInitialControllerWithCreator
  , instantiateControllerWithIdentifier
  , instantiateControllerWithIdentifier_creator
  , mainStoryboard
  , instantiateControllerWithIdentifierSelector
  , instantiateControllerWithIdentifier_creatorSelector
  , instantiateInitialControllerSelector
  , instantiateInitialControllerWithCreatorSelector
  , mainStoryboardSelector
  , storyboardWithName_bundleSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ storyboardWithName:bundle:@
storyboardWithName_bundle :: (IsNSString name, IsNSBundle storyboardBundleOrNil) => name -> storyboardBundleOrNil -> IO (Id NSStoryboard)
storyboardWithName_bundle name storyboardBundleOrNil =
  do
    cls' <- getRequiredClass "NSStoryboard"
    sendClassMessage cls' storyboardWithName_bundleSelector (toNSString name) (toNSBundle storyboardBundleOrNil)

-- | @- instantiateInitialController@
instantiateInitialController :: IsNSStoryboard nsStoryboard => nsStoryboard -> IO RawId
instantiateInitialController nsStoryboard =
  sendMessage nsStoryboard instantiateInitialControllerSelector

-- | @- instantiateInitialControllerWithCreator:@
instantiateInitialControllerWithCreator :: IsNSStoryboard nsStoryboard => nsStoryboard -> RawId -> IO RawId
instantiateInitialControllerWithCreator nsStoryboard block =
  sendMessage nsStoryboard instantiateInitialControllerWithCreatorSelector block

-- | @- instantiateControllerWithIdentifier:@
instantiateControllerWithIdentifier :: (IsNSStoryboard nsStoryboard, IsNSString identifier) => nsStoryboard -> identifier -> IO RawId
instantiateControllerWithIdentifier nsStoryboard identifier =
  sendMessage nsStoryboard instantiateControllerWithIdentifierSelector (toNSString identifier)

-- | @- instantiateControllerWithIdentifier:creator:@
instantiateControllerWithIdentifier_creator :: (IsNSStoryboard nsStoryboard, IsNSString identifier) => nsStoryboard -> identifier -> RawId -> IO RawId
instantiateControllerWithIdentifier_creator nsStoryboard identifier block =
  sendMessage nsStoryboard instantiateControllerWithIdentifier_creatorSelector (toNSString identifier) block

-- | @+ mainStoryboard@
mainStoryboard :: IO (Id NSStoryboard)
mainStoryboard  =
  do
    cls' <- getRequiredClass "NSStoryboard"
    sendClassMessage cls' mainStoryboardSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @storyboardWithName:bundle:@
storyboardWithName_bundleSelector :: Selector '[Id NSString, Id NSBundle] (Id NSStoryboard)
storyboardWithName_bundleSelector = mkSelector "storyboardWithName:bundle:"

-- | @Selector@ for @instantiateInitialController@
instantiateInitialControllerSelector :: Selector '[] RawId
instantiateInitialControllerSelector = mkSelector "instantiateInitialController"

-- | @Selector@ for @instantiateInitialControllerWithCreator:@
instantiateInitialControllerWithCreatorSelector :: Selector '[RawId] RawId
instantiateInitialControllerWithCreatorSelector = mkSelector "instantiateInitialControllerWithCreator:"

-- | @Selector@ for @instantiateControllerWithIdentifier:@
instantiateControllerWithIdentifierSelector :: Selector '[Id NSString] RawId
instantiateControllerWithIdentifierSelector = mkSelector "instantiateControllerWithIdentifier:"

-- | @Selector@ for @instantiateControllerWithIdentifier:creator:@
instantiateControllerWithIdentifier_creatorSelector :: Selector '[Id NSString, RawId] RawId
instantiateControllerWithIdentifier_creatorSelector = mkSelector "instantiateControllerWithIdentifier:creator:"

-- | @Selector@ for @mainStoryboard@
mainStoryboardSelector :: Selector '[] (Id NSStoryboard)
mainStoryboardSelector = mkSelector "mainStoryboard"

