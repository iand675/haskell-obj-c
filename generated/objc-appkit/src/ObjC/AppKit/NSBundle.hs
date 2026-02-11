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
  , pathForSoundResourceSelector
  , loadNibFile_externalNameTable_withZoneSelector
  , loadNibNamed_ownerSelector
  , loadNibNamed_owner_topLevelObjectsSelector
  , imageForResourceSelector
  , pathForImageResourceSelector
  , urlForImageResourceSelector
  , contextHelpForKeySelector


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

-- | @- pathForSoundResource:@
pathForSoundResource :: (IsNSBundle nsBundle, IsNSString name) => nsBundle -> name -> IO (Id NSString)
pathForSoundResource nsBundle  name =
withObjCPtr name $ \raw_name ->
    sendMsg nsBundle (mkSelector "pathForSoundResource:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ())] >>= retainedObject . castPtr

-- | @+ loadNibFile:externalNameTable:withZone:@
nsBundleLoadNibFile_externalNameTable_withZone :: (IsNSString fileName, IsNSDictionary context) => fileName -> context -> Ptr () -> IO Bool
nsBundleLoadNibFile_externalNameTable_withZone fileName context zone =
  do
    cls' <- getRequiredClass "NSBundle"
    withObjCPtr fileName $ \raw_fileName ->
      withObjCPtr context $ \raw_context ->
        fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "loadNibFile:externalNameTable:withZone:") retCULong [argPtr (castPtr raw_fileName :: Ptr ()), argPtr (castPtr raw_context :: Ptr ()), argPtr zone]

-- | @+ loadNibNamed:owner:@
loadNibNamed_owner :: IsNSString nibName => nibName -> RawId -> IO Bool
loadNibNamed_owner nibName owner =
  do
    cls' <- getRequiredClass "NSBundle"
    withObjCPtr nibName $ \raw_nibName ->
      fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "loadNibNamed:owner:") retCULong [argPtr (castPtr raw_nibName :: Ptr ()), argPtr (castPtr (unRawId owner) :: Ptr ())]

-- | @- loadNibFile:externalNameTable:withZone:@
loadNibFile_externalNameTable_withZone :: (IsNSBundle nsBundle, IsNSString fileName, IsNSDictionary context) => nsBundle -> fileName -> context -> Ptr () -> IO Bool
loadNibFile_externalNameTable_withZone nsBundle  fileName context zone =
withObjCPtr fileName $ \raw_fileName ->
  withObjCPtr context $ \raw_context ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsBundle (mkSelector "loadNibFile:externalNameTable:withZone:") retCULong [argPtr (castPtr raw_fileName :: Ptr ()), argPtr (castPtr raw_context :: Ptr ()), argPtr zone]

-- | @- loadNibNamed:owner:topLevelObjects:@
loadNibNamed_owner_topLevelObjects :: (IsNSBundle nsBundle, IsNSString nibName, IsNSArray topLevelObjects) => nsBundle -> nibName -> RawId -> topLevelObjects -> IO Bool
loadNibNamed_owner_topLevelObjects nsBundle  nibName owner topLevelObjects =
withObjCPtr nibName $ \raw_nibName ->
  withObjCPtr topLevelObjects $ \raw_topLevelObjects ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsBundle (mkSelector "loadNibNamed:owner:topLevelObjects:") retCULong [argPtr (castPtr raw_nibName :: Ptr ()), argPtr (castPtr (unRawId owner) :: Ptr ()), argPtr (castPtr raw_topLevelObjects :: Ptr ())]

-- | @- imageForResource:@
imageForResource :: (IsNSBundle nsBundle, IsNSString name) => nsBundle -> name -> IO (Id NSImage)
imageForResource nsBundle  name =
withObjCPtr name $ \raw_name ->
    sendMsg nsBundle (mkSelector "imageForResource:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ())] >>= retainedObject . castPtr

-- | @- pathForImageResource:@
pathForImageResource :: (IsNSBundle nsBundle, IsNSString name) => nsBundle -> name -> IO (Id NSString)
pathForImageResource nsBundle  name =
withObjCPtr name $ \raw_name ->
    sendMsg nsBundle (mkSelector "pathForImageResource:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ())] >>= retainedObject . castPtr

-- | @- URLForImageResource:@
urlForImageResource :: (IsNSBundle nsBundle, IsNSString name) => nsBundle -> name -> IO (Id NSURL)
urlForImageResource nsBundle  name =
withObjCPtr name $ \raw_name ->
    sendMsg nsBundle (mkSelector "URLForImageResource:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ())] >>= retainedObject . castPtr

-- | @- contextHelpForKey:@
contextHelpForKey :: (IsNSBundle nsBundle, IsNSString key) => nsBundle -> key -> IO (Id NSAttributedString)
contextHelpForKey nsBundle  key =
withObjCPtr key $ \raw_key ->
    sendMsg nsBundle (mkSelector "contextHelpForKey:") (retPtr retVoid) [argPtr (castPtr raw_key :: Ptr ())] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @pathForSoundResource:@
pathForSoundResourceSelector :: Selector
pathForSoundResourceSelector = mkSelector "pathForSoundResource:"

-- | @Selector@ for @loadNibFile:externalNameTable:withZone:@
loadNibFile_externalNameTable_withZoneSelector :: Selector
loadNibFile_externalNameTable_withZoneSelector = mkSelector "loadNibFile:externalNameTable:withZone:"

-- | @Selector@ for @loadNibNamed:owner:@
loadNibNamed_ownerSelector :: Selector
loadNibNamed_ownerSelector = mkSelector "loadNibNamed:owner:"

-- | @Selector@ for @loadNibNamed:owner:topLevelObjects:@
loadNibNamed_owner_topLevelObjectsSelector :: Selector
loadNibNamed_owner_topLevelObjectsSelector = mkSelector "loadNibNamed:owner:topLevelObjects:"

-- | @Selector@ for @imageForResource:@
imageForResourceSelector :: Selector
imageForResourceSelector = mkSelector "imageForResource:"

-- | @Selector@ for @pathForImageResource:@
pathForImageResourceSelector :: Selector
pathForImageResourceSelector = mkSelector "pathForImageResource:"

-- | @Selector@ for @URLForImageResource:@
urlForImageResourceSelector :: Selector
urlForImageResourceSelector = mkSelector "URLForImageResource:"

-- | @Selector@ for @contextHelpForKey:@
contextHelpForKeySelector :: Selector
contextHelpForKeySelector = mkSelector "contextHelpForKey:"

