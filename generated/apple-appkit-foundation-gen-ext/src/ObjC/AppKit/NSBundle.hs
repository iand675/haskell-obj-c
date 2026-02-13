{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSBundle@.
module ObjC.AppKit.NSBundle
  ( NSBundle
  , IsNSBundle(..)
  , pathForSoundResource
  , nsBundleLoadNibFile_externalNameTable_withZone
  , loadNibNamed_owner
  , loadNibFile_externalNameTable_withZone
  , loadNibNamed_owner_topLevelObjects
  , imageForResource
  , pathForImageResource
  , urlForImageResource
  , contextHelpForKey
  , contextHelpForKeySelector
  , imageForResourceSelector
  , loadNibFile_externalNameTable_withZoneSelector
  , loadNibNamed_ownerSelector
  , loadNibNamed_owner_topLevelObjectsSelector
  , nsBundleLoadNibFile_externalNameTable_withZoneSelector
  , pathForImageResourceSelector
  , pathForSoundResourceSelector
  , urlForImageResourceSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- pathForSoundResource:@
pathForSoundResource :: (IsNSBundle nsBundle, IsNSString name) => nsBundle -> name -> IO (Id NSString)
pathForSoundResource nsBundle name =
  sendMessage nsBundle pathForSoundResourceSelector (toNSString name)

-- | @+ loadNibFile:externalNameTable:withZone:@
nsBundleLoadNibFile_externalNameTable_withZone :: (IsNSString fileName, IsNSDictionary context) => fileName -> context -> Ptr () -> IO Bool
nsBundleLoadNibFile_externalNameTable_withZone fileName context zone =
  do
    cls' <- getRequiredClass "NSBundle"
    sendClassMessage cls' nsBundleLoadNibFile_externalNameTable_withZoneSelector (toNSString fileName) (toNSDictionary context) zone

-- | @+ loadNibNamed:owner:@
loadNibNamed_owner :: IsNSString nibName => nibName -> RawId -> IO Bool
loadNibNamed_owner nibName owner =
  do
    cls' <- getRequiredClass "NSBundle"
    sendClassMessage cls' loadNibNamed_ownerSelector (toNSString nibName) owner

-- | @- loadNibFile:externalNameTable:withZone:@
loadNibFile_externalNameTable_withZone :: (IsNSBundle nsBundle, IsNSString fileName, IsNSDictionary context) => nsBundle -> fileName -> context -> Ptr () -> IO Bool
loadNibFile_externalNameTable_withZone nsBundle fileName context zone =
  sendMessage nsBundle loadNibFile_externalNameTable_withZoneSelector (toNSString fileName) (toNSDictionary context) zone

-- | @- loadNibNamed:owner:topLevelObjects:@
loadNibNamed_owner_topLevelObjects :: (IsNSBundle nsBundle, IsNSString nibName, IsNSArray topLevelObjects) => nsBundle -> nibName -> RawId -> topLevelObjects -> IO Bool
loadNibNamed_owner_topLevelObjects nsBundle nibName owner topLevelObjects =
  sendMessage nsBundle loadNibNamed_owner_topLevelObjectsSelector (toNSString nibName) owner (toNSArray topLevelObjects)

-- | @- imageForResource:@
imageForResource :: (IsNSBundle nsBundle, IsNSString name) => nsBundle -> name -> IO (Id NSImage)
imageForResource nsBundle name =
  sendMessage nsBundle imageForResourceSelector (toNSString name)

-- | @- pathForImageResource:@
pathForImageResource :: (IsNSBundle nsBundle, IsNSString name) => nsBundle -> name -> IO (Id NSString)
pathForImageResource nsBundle name =
  sendMessage nsBundle pathForImageResourceSelector (toNSString name)

-- | @- URLForImageResource:@
urlForImageResource :: (IsNSBundle nsBundle, IsNSString name) => nsBundle -> name -> IO (Id NSURL)
urlForImageResource nsBundle name =
  sendMessage nsBundle urlForImageResourceSelector (toNSString name)

-- | @- contextHelpForKey:@
contextHelpForKey :: (IsNSBundle nsBundle, IsNSString key) => nsBundle -> key -> IO (Id NSAttributedString)
contextHelpForKey nsBundle key =
  sendMessage nsBundle contextHelpForKeySelector (toNSString key)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @pathForSoundResource:@
pathForSoundResourceSelector :: Selector '[Id NSString] (Id NSString)
pathForSoundResourceSelector = mkSelector "pathForSoundResource:"

-- | @Selector@ for @loadNibFile:externalNameTable:withZone:@
nsBundleLoadNibFile_externalNameTable_withZoneSelector :: Selector '[Id NSString, Id NSDictionary, Ptr ()] Bool
nsBundleLoadNibFile_externalNameTable_withZoneSelector = mkSelector "loadNibFile:externalNameTable:withZone:"

-- | @Selector@ for @loadNibNamed:owner:@
loadNibNamed_ownerSelector :: Selector '[Id NSString, RawId] Bool
loadNibNamed_ownerSelector = mkSelector "loadNibNamed:owner:"

-- | @Selector@ for @loadNibFile:externalNameTable:withZone:@
loadNibFile_externalNameTable_withZoneSelector :: Selector '[Id NSString, Id NSDictionary, Ptr ()] Bool
loadNibFile_externalNameTable_withZoneSelector = mkSelector "loadNibFile:externalNameTable:withZone:"

-- | @Selector@ for @loadNibNamed:owner:topLevelObjects:@
loadNibNamed_owner_topLevelObjectsSelector :: Selector '[Id NSString, RawId, Id NSArray] Bool
loadNibNamed_owner_topLevelObjectsSelector = mkSelector "loadNibNamed:owner:topLevelObjects:"

-- | @Selector@ for @imageForResource:@
imageForResourceSelector :: Selector '[Id NSString] (Id NSImage)
imageForResourceSelector = mkSelector "imageForResource:"

-- | @Selector@ for @pathForImageResource:@
pathForImageResourceSelector :: Selector '[Id NSString] (Id NSString)
pathForImageResourceSelector = mkSelector "pathForImageResource:"

-- | @Selector@ for @URLForImageResource:@
urlForImageResourceSelector :: Selector '[Id NSString] (Id NSURL)
urlForImageResourceSelector = mkSelector "URLForImageResource:"

-- | @Selector@ for @contextHelpForKey:@
contextHelpForKeySelector :: Selector '[Id NSString] (Id NSAttributedString)
contextHelpForKeySelector = mkSelector "contextHelpForKey:"

