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

-- | @- pathForSoundResource:@
pathForSoundResource :: IsNSBundle nsBundle => nsBundle -> RawId -> IO RawId
pathForSoundResource nsBundle  name =
    fmap (RawId . castPtr) $ sendMsg nsBundle (mkSelector "pathForSoundResource:") (retPtr retVoid) [argPtr (castPtr (unRawId name) :: Ptr ())]

-- | @+ loadNibFile:externalNameTable:withZone:@
nsBundleLoadNibFile_externalNameTable_withZone :: RawId -> RawId -> Ptr () -> IO Bool
nsBundleLoadNibFile_externalNameTable_withZone fileName context zone =
  do
    cls' <- getRequiredClass "NSBundle"
    fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "loadNibFile:externalNameTable:withZone:") retCULong [argPtr (castPtr (unRawId fileName) :: Ptr ()), argPtr (castPtr (unRawId context) :: Ptr ()), argPtr zone]

-- | @+ loadNibNamed:owner:@
loadNibNamed_owner :: RawId -> RawId -> IO Bool
loadNibNamed_owner nibName owner =
  do
    cls' <- getRequiredClass "NSBundle"
    fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "loadNibNamed:owner:") retCULong [argPtr (castPtr (unRawId nibName) :: Ptr ()), argPtr (castPtr (unRawId owner) :: Ptr ())]

-- | @- loadNibFile:externalNameTable:withZone:@
loadNibFile_externalNameTable_withZone :: IsNSBundle nsBundle => nsBundle -> RawId -> RawId -> Ptr () -> IO Bool
loadNibFile_externalNameTable_withZone nsBundle  fileName context zone =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsBundle (mkSelector "loadNibFile:externalNameTable:withZone:") retCULong [argPtr (castPtr (unRawId fileName) :: Ptr ()), argPtr (castPtr (unRawId context) :: Ptr ()), argPtr zone]

-- | @- loadNibNamed:owner:topLevelObjects:@
loadNibNamed_owner_topLevelObjects :: IsNSBundle nsBundle => nsBundle -> RawId -> RawId -> RawId -> IO Bool
loadNibNamed_owner_topLevelObjects nsBundle  nibName owner topLevelObjects =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsBundle (mkSelector "loadNibNamed:owner:topLevelObjects:") retCULong [argPtr (castPtr (unRawId nibName) :: Ptr ()), argPtr (castPtr (unRawId owner) :: Ptr ()), argPtr (castPtr (unRawId topLevelObjects) :: Ptr ())]

-- | @- imageForResource:@
imageForResource :: IsNSBundle nsBundle => nsBundle -> RawId -> IO (Id NSImage)
imageForResource nsBundle  name =
    sendMsg nsBundle (mkSelector "imageForResource:") (retPtr retVoid) [argPtr (castPtr (unRawId name) :: Ptr ())] >>= retainedObject . castPtr

-- | @- pathForImageResource:@
pathForImageResource :: IsNSBundle nsBundle => nsBundle -> RawId -> IO RawId
pathForImageResource nsBundle  name =
    fmap (RawId . castPtr) $ sendMsg nsBundle (mkSelector "pathForImageResource:") (retPtr retVoid) [argPtr (castPtr (unRawId name) :: Ptr ())]

-- | @- URLForImageResource:@
urlForImageResource :: IsNSBundle nsBundle => nsBundle -> RawId -> IO RawId
urlForImageResource nsBundle  name =
    fmap (RawId . castPtr) $ sendMsg nsBundle (mkSelector "URLForImageResource:") (retPtr retVoid) [argPtr (castPtr (unRawId name) :: Ptr ())]

-- | @- contextHelpForKey:@
contextHelpForKey :: IsNSBundle nsBundle => nsBundle -> RawId -> IO RawId
contextHelpForKey nsBundle  key =
    fmap (RawId . castPtr) $ sendMsg nsBundle (mkSelector "contextHelpForKey:") (retPtr retVoid) [argPtr (castPtr (unRawId key) :: Ptr ())]

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

