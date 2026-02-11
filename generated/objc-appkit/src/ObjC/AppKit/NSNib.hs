{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSNib@.
module ObjC.AppKit.NSNib
  ( NSNib
  , IsNSNib(..)
  , initWithNibNamed_bundle
  , initWithNibData_bundle
  , instantiateWithOwner_topLevelObjects
  , initWithContentsOfURL
  , instantiateNibWithExternalNameTable
  , instantiateNibWithOwner_topLevelObjects
  , initWithNibNamed_bundleSelector
  , initWithNibData_bundleSelector
  , instantiateWithOwner_topLevelObjectsSelector
  , initWithContentsOfURLSelector
  , instantiateNibWithExternalNameTableSelector
  , instantiateNibWithOwner_topLevelObjectsSelector


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

-- | @- initWithNibNamed:bundle:@
initWithNibNamed_bundle :: (IsNSNib nsNib, IsNSString nibName, IsNSBundle bundle) => nsNib -> nibName -> bundle -> IO (Id NSNib)
initWithNibNamed_bundle nsNib  nibName bundle =
withObjCPtr nibName $ \raw_nibName ->
  withObjCPtr bundle $ \raw_bundle ->
      sendMsg nsNib (mkSelector "initWithNibNamed:bundle:") (retPtr retVoid) [argPtr (castPtr raw_nibName :: Ptr ()), argPtr (castPtr raw_bundle :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithNibData:bundle:@
initWithNibData_bundle :: (IsNSNib nsNib, IsNSData nibData, IsNSBundle bundle) => nsNib -> nibData -> bundle -> IO (Id NSNib)
initWithNibData_bundle nsNib  nibData bundle =
withObjCPtr nibData $ \raw_nibData ->
  withObjCPtr bundle $ \raw_bundle ->
      sendMsg nsNib (mkSelector "initWithNibData:bundle:") (retPtr retVoid) [argPtr (castPtr raw_nibData :: Ptr ()), argPtr (castPtr raw_bundle :: Ptr ())] >>= ownedObject . castPtr

-- | @- instantiateWithOwner:topLevelObjects:@
instantiateWithOwner_topLevelObjects :: (IsNSNib nsNib, IsNSArray topLevelObjects) => nsNib -> RawId -> topLevelObjects -> IO Bool
instantiateWithOwner_topLevelObjects nsNib  owner topLevelObjects =
withObjCPtr topLevelObjects $ \raw_topLevelObjects ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsNib (mkSelector "instantiateWithOwner:topLevelObjects:") retCULong [argPtr (castPtr (unRawId owner) :: Ptr ()), argPtr (castPtr raw_topLevelObjects :: Ptr ())]

-- | @- initWithContentsOfURL:@
initWithContentsOfURL :: (IsNSNib nsNib, IsNSURL nibFileURL) => nsNib -> nibFileURL -> IO RawId
initWithContentsOfURL nsNib  nibFileURL =
withObjCPtr nibFileURL $ \raw_nibFileURL ->
    fmap (RawId . castPtr) $ sendMsg nsNib (mkSelector "initWithContentsOfURL:") (retPtr retVoid) [argPtr (castPtr raw_nibFileURL :: Ptr ())]

-- | @- instantiateNibWithExternalNameTable:@
instantiateNibWithExternalNameTable :: (IsNSNib nsNib, IsNSDictionary externalNameTable) => nsNib -> externalNameTable -> IO Bool
instantiateNibWithExternalNameTable nsNib  externalNameTable =
withObjCPtr externalNameTable $ \raw_externalNameTable ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsNib (mkSelector "instantiateNibWithExternalNameTable:") retCULong [argPtr (castPtr raw_externalNameTable :: Ptr ())]

-- | @- instantiateNibWithOwner:topLevelObjects:@
instantiateNibWithOwner_topLevelObjects :: (IsNSNib nsNib, IsNSArray topLevelObjects) => nsNib -> RawId -> topLevelObjects -> IO Bool
instantiateNibWithOwner_topLevelObjects nsNib  owner topLevelObjects =
withObjCPtr topLevelObjects $ \raw_topLevelObjects ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsNib (mkSelector "instantiateNibWithOwner:topLevelObjects:") retCULong [argPtr (castPtr (unRawId owner) :: Ptr ()), argPtr (castPtr raw_topLevelObjects :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithNibNamed:bundle:@
initWithNibNamed_bundleSelector :: Selector
initWithNibNamed_bundleSelector = mkSelector "initWithNibNamed:bundle:"

-- | @Selector@ for @initWithNibData:bundle:@
initWithNibData_bundleSelector :: Selector
initWithNibData_bundleSelector = mkSelector "initWithNibData:bundle:"

-- | @Selector@ for @instantiateWithOwner:topLevelObjects:@
instantiateWithOwner_topLevelObjectsSelector :: Selector
instantiateWithOwner_topLevelObjectsSelector = mkSelector "instantiateWithOwner:topLevelObjects:"

-- | @Selector@ for @initWithContentsOfURL:@
initWithContentsOfURLSelector :: Selector
initWithContentsOfURLSelector = mkSelector "initWithContentsOfURL:"

-- | @Selector@ for @instantiateNibWithExternalNameTable:@
instantiateNibWithExternalNameTableSelector :: Selector
instantiateNibWithExternalNameTableSelector = mkSelector "instantiateNibWithExternalNameTable:"

-- | @Selector@ for @instantiateNibWithOwner:topLevelObjects:@
instantiateNibWithOwner_topLevelObjectsSelector :: Selector
instantiateNibWithOwner_topLevelObjectsSelector = mkSelector "instantiateNibWithOwner:topLevelObjects:"

