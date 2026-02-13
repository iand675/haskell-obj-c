{-# LANGUAGE DataKinds #-}
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
  , initWithContentsOfURLSelector
  , initWithNibData_bundleSelector
  , initWithNibNamed_bundleSelector
  , instantiateNibWithExternalNameTableSelector
  , instantiateNibWithOwner_topLevelObjectsSelector
  , instantiateWithOwner_topLevelObjectsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithNibNamed:bundle:@
initWithNibNamed_bundle :: (IsNSNib nsNib, IsNSString nibName, IsNSBundle bundle) => nsNib -> nibName -> bundle -> IO (Id NSNib)
initWithNibNamed_bundle nsNib nibName bundle =
  sendOwnedMessage nsNib initWithNibNamed_bundleSelector (toNSString nibName) (toNSBundle bundle)

-- | @- initWithNibData:bundle:@
initWithNibData_bundle :: (IsNSNib nsNib, IsNSData nibData, IsNSBundle bundle) => nsNib -> nibData -> bundle -> IO (Id NSNib)
initWithNibData_bundle nsNib nibData bundle =
  sendOwnedMessage nsNib initWithNibData_bundleSelector (toNSData nibData) (toNSBundle bundle)

-- | @- instantiateWithOwner:topLevelObjects:@
instantiateWithOwner_topLevelObjects :: (IsNSNib nsNib, IsNSArray topLevelObjects) => nsNib -> RawId -> topLevelObjects -> IO Bool
instantiateWithOwner_topLevelObjects nsNib owner topLevelObjects =
  sendMessage nsNib instantiateWithOwner_topLevelObjectsSelector owner (toNSArray topLevelObjects)

-- | @- initWithContentsOfURL:@
initWithContentsOfURL :: (IsNSNib nsNib, IsNSURL nibFileURL) => nsNib -> nibFileURL -> IO RawId
initWithContentsOfURL nsNib nibFileURL =
  sendOwnedMessage nsNib initWithContentsOfURLSelector (toNSURL nibFileURL)

-- | @- instantiateNibWithExternalNameTable:@
instantiateNibWithExternalNameTable :: (IsNSNib nsNib, IsNSDictionary externalNameTable) => nsNib -> externalNameTable -> IO Bool
instantiateNibWithExternalNameTable nsNib externalNameTable =
  sendMessage nsNib instantiateNibWithExternalNameTableSelector (toNSDictionary externalNameTable)

-- | @- instantiateNibWithOwner:topLevelObjects:@
instantiateNibWithOwner_topLevelObjects :: (IsNSNib nsNib, IsNSArray topLevelObjects) => nsNib -> RawId -> topLevelObjects -> IO Bool
instantiateNibWithOwner_topLevelObjects nsNib owner topLevelObjects =
  sendMessage nsNib instantiateNibWithOwner_topLevelObjectsSelector owner (toNSArray topLevelObjects)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithNibNamed:bundle:@
initWithNibNamed_bundleSelector :: Selector '[Id NSString, Id NSBundle] (Id NSNib)
initWithNibNamed_bundleSelector = mkSelector "initWithNibNamed:bundle:"

-- | @Selector@ for @initWithNibData:bundle:@
initWithNibData_bundleSelector :: Selector '[Id NSData, Id NSBundle] (Id NSNib)
initWithNibData_bundleSelector = mkSelector "initWithNibData:bundle:"

-- | @Selector@ for @instantiateWithOwner:topLevelObjects:@
instantiateWithOwner_topLevelObjectsSelector :: Selector '[RawId, Id NSArray] Bool
instantiateWithOwner_topLevelObjectsSelector = mkSelector "instantiateWithOwner:topLevelObjects:"

-- | @Selector@ for @initWithContentsOfURL:@
initWithContentsOfURLSelector :: Selector '[Id NSURL] RawId
initWithContentsOfURLSelector = mkSelector "initWithContentsOfURL:"

-- | @Selector@ for @instantiateNibWithExternalNameTable:@
instantiateNibWithExternalNameTableSelector :: Selector '[Id NSDictionary] Bool
instantiateNibWithExternalNameTableSelector = mkSelector "instantiateNibWithExternalNameTable:"

-- | @Selector@ for @instantiateNibWithOwner:topLevelObjects:@
instantiateNibWithOwner_topLevelObjectsSelector :: Selector '[RawId, Id NSArray] Bool
instantiateNibWithOwner_topLevelObjectsSelector = mkSelector "instantiateNibWithOwner:topLevelObjects:"

