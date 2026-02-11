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
  , initSelector
  , initWithNameSelector
  , initWithName_bundleSelector
  , nameSelector
  , dataSelector
  , typeIdentifierSelector


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

-- | @- init@
init_ :: IsNSDataAsset nsDataAsset => nsDataAsset -> IO (Id NSDataAsset)
init_ nsDataAsset  =
  sendMsg nsDataAsset (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Equivalent to -initWithName:name bundle:[NSBundle mainBundle];
--
-- ObjC selector: @- initWithName:@
initWithName :: (IsNSDataAsset nsDataAsset, IsNSString name) => nsDataAsset -> name -> IO (Id NSDataAsset)
initWithName nsDataAsset  name =
withObjCPtr name $ \raw_name ->
    sendMsg nsDataAsset (mkSelector "initWithName:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ())] >>= ownedObject . castPtr

-- | Create a data asset with the given name from the given bundle. Returns nil if the asset was not found.
--
-- ObjC selector: @- initWithName:bundle:@
initWithName_bundle :: (IsNSDataAsset nsDataAsset, IsNSString name, IsNSBundle bundle) => nsDataAsset -> name -> bundle -> IO (Id NSDataAsset)
initWithName_bundle nsDataAsset  name bundle =
withObjCPtr name $ \raw_name ->
  withObjCPtr bundle $ \raw_bundle ->
      sendMsg nsDataAsset (mkSelector "initWithName:bundle:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ()), argPtr (castPtr raw_bundle :: Ptr ())] >>= ownedObject . castPtr

-- | The name used to reference the data asset
--
-- ObjC selector: @- name@
name :: IsNSDataAsset nsDataAsset => nsDataAsset -> IO (Id NSString)
name nsDataAsset  =
  sendMsg nsDataAsset (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The data for this asset, as stored in the asset catalog
--
-- ObjC selector: @- data@
data_ :: IsNSDataAsset nsDataAsset => nsDataAsset -> IO (Id NSData)
data_ nsDataAsset  =
  sendMsg nsDataAsset (mkSelector "data") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The Uniform Type Identifier for this data object.
--
-- ObjC selector: @- typeIdentifier@
typeIdentifier :: IsNSDataAsset nsDataAsset => nsDataAsset -> IO (Id NSString)
typeIdentifier nsDataAsset  =
  sendMsg nsDataAsset (mkSelector "typeIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithName:@
initWithNameSelector :: Selector
initWithNameSelector = mkSelector "initWithName:"

-- | @Selector@ for @initWithName:bundle:@
initWithName_bundleSelector :: Selector
initWithName_bundleSelector = mkSelector "initWithName:bundle:"

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

-- | @Selector@ for @data@
dataSelector :: Selector
dataSelector = mkSelector "data"

-- | @Selector@ for @typeIdentifier@
typeIdentifierSelector :: Selector
typeIdentifierSelector = mkSelector "typeIdentifier"

