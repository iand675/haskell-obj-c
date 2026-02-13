{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSDataAsset@.
module ObjC.AppKit.NSDataAsset
  ( NSDataAsset
  , IsNSDataAsset(..)
  , init_
  , initWithName
  , initWithName_bundle
  , name
  , data_
  , typeIdentifier
  , dataSelector
  , initSelector
  , initWithNameSelector
  , initWithName_bundleSelector
  , nameSelector
  , typeIdentifierSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsNSDataAsset nsDataAsset => nsDataAsset -> IO (Id NSDataAsset)
init_ nsDataAsset =
  sendOwnedMessage nsDataAsset initSelector

-- | Equivalent to -initWithName:name bundle:[NSBundle mainBundle];
--
-- ObjC selector: @- initWithName:@
initWithName :: (IsNSDataAsset nsDataAsset, IsNSString name) => nsDataAsset -> name -> IO (Id NSDataAsset)
initWithName nsDataAsset name =
  sendOwnedMessage nsDataAsset initWithNameSelector (toNSString name)

-- | Create a data asset with the given name from the given bundle. Returns nil if the asset was not found.
--
-- ObjC selector: @- initWithName:bundle:@
initWithName_bundle :: (IsNSDataAsset nsDataAsset, IsNSString name, IsNSBundle bundle) => nsDataAsset -> name -> bundle -> IO (Id NSDataAsset)
initWithName_bundle nsDataAsset name bundle =
  sendOwnedMessage nsDataAsset initWithName_bundleSelector (toNSString name) (toNSBundle bundle)

-- | The name used to reference the data asset
--
-- ObjC selector: @- name@
name :: IsNSDataAsset nsDataAsset => nsDataAsset -> IO (Id NSString)
name nsDataAsset =
  sendMessage nsDataAsset nameSelector

-- | The data for this asset, as stored in the asset catalog
--
-- ObjC selector: @- data@
data_ :: IsNSDataAsset nsDataAsset => nsDataAsset -> IO (Id NSData)
data_ nsDataAsset =
  sendMessage nsDataAsset dataSelector

-- | The Uniform Type Identifier for this data object.
--
-- ObjC selector: @- typeIdentifier@
typeIdentifier :: IsNSDataAsset nsDataAsset => nsDataAsset -> IO (Id NSString)
typeIdentifier nsDataAsset =
  sendMessage nsDataAsset typeIdentifierSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id NSDataAsset)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithName:@
initWithNameSelector :: Selector '[Id NSString] (Id NSDataAsset)
initWithNameSelector = mkSelector "initWithName:"

-- | @Selector@ for @initWithName:bundle:@
initWithName_bundleSelector :: Selector '[Id NSString, Id NSBundle] (Id NSDataAsset)
initWithName_bundleSelector = mkSelector "initWithName:bundle:"

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

-- | @Selector@ for @data@
dataSelector :: Selector '[] (Id NSData)
dataSelector = mkSelector "data"

-- | @Selector@ for @typeIdentifier@
typeIdentifierSelector :: Selector '[] (Id NSString)
typeIdentifierSelector = mkSelector "typeIdentifier"

