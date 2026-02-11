{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | **********		Archiving: Reading		***************
--
-- Generated bindings for @NSUnarchiver@.
module ObjC.Foundation.NSUnarchiver
  ( NSUnarchiver
  , IsNSUnarchiver(..)
  , initForReadingWithData
  , setObjectZone
  , objectZone
  , unarchiveObjectWithData
  , unarchiveObjectWithFile
  , nsUnarchiverDecodeClassName_asClassName
  , decodeClassName_asClassName
  , nsUnarchiverClassNameDecodedForArchiveClassName
  , classNameDecodedForArchiveClassName
  , replaceObject_withObject
  , atEnd
  , systemVersion
  , initForReadingWithDataSelector
  , setObjectZoneSelector
  , objectZoneSelector
  , unarchiveObjectWithDataSelector
  , unarchiveObjectWithFileSelector
  , decodeClassName_asClassNameSelector
  , classNameDecodedForArchiveClassNameSelector
  , replaceObject_withObjectSelector
  , atEndSelector
  , systemVersionSelector


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

import ObjC.Foundation.Internal.Classes

-- | @- initForReadingWithData:@
initForReadingWithData :: (IsNSUnarchiver nsUnarchiver, IsNSData data_) => nsUnarchiver -> data_ -> IO (Id NSUnarchiver)
initForReadingWithData nsUnarchiver  data_ =
withObjCPtr data_ $ \raw_data_ ->
    sendMsg nsUnarchiver (mkSelector "initForReadingWithData:") (retPtr retVoid) [argPtr (castPtr raw_data_ :: Ptr ())] >>= ownedObject . castPtr

-- | @- setObjectZone:@
setObjectZone :: IsNSUnarchiver nsUnarchiver => nsUnarchiver -> Ptr () -> IO ()
setObjectZone nsUnarchiver  zone =
  sendMsg nsUnarchiver (mkSelector "setObjectZone:") retVoid [argPtr zone]

-- | @- objectZone@
objectZone :: IsNSUnarchiver nsUnarchiver => nsUnarchiver -> IO (Ptr ())
objectZone nsUnarchiver  =
  fmap castPtr $ sendMsg nsUnarchiver (mkSelector "objectZone") (retPtr retVoid) []

-- | @+ unarchiveObjectWithData:@
unarchiveObjectWithData :: IsNSData data_ => data_ -> IO RawId
unarchiveObjectWithData data_ =
  do
    cls' <- getRequiredClass "NSUnarchiver"
    withObjCPtr data_ $ \raw_data_ ->
      fmap (RawId . castPtr) $ sendClassMsg cls' (mkSelector "unarchiveObjectWithData:") (retPtr retVoid) [argPtr (castPtr raw_data_ :: Ptr ())]

-- | @+ unarchiveObjectWithFile:@
unarchiveObjectWithFile :: IsNSString path => path -> IO RawId
unarchiveObjectWithFile path =
  do
    cls' <- getRequiredClass "NSUnarchiver"
    withObjCPtr path $ \raw_path ->
      fmap (RawId . castPtr) $ sendClassMsg cls' (mkSelector "unarchiveObjectWithFile:") (retPtr retVoid) [argPtr (castPtr raw_path :: Ptr ())]

-- | @+ decodeClassName:asClassName:@
nsUnarchiverDecodeClassName_asClassName :: (IsNSString inArchiveName, IsNSString trueName) => inArchiveName -> trueName -> IO ()
nsUnarchiverDecodeClassName_asClassName inArchiveName trueName =
  do
    cls' <- getRequiredClass "NSUnarchiver"
    withObjCPtr inArchiveName $ \raw_inArchiveName ->
      withObjCPtr trueName $ \raw_trueName ->
        sendClassMsg cls' (mkSelector "decodeClassName:asClassName:") retVoid [argPtr (castPtr raw_inArchiveName :: Ptr ()), argPtr (castPtr raw_trueName :: Ptr ())]

-- | @- decodeClassName:asClassName:@
decodeClassName_asClassName :: (IsNSUnarchiver nsUnarchiver, IsNSString inArchiveName, IsNSString trueName) => nsUnarchiver -> inArchiveName -> trueName -> IO ()
decodeClassName_asClassName nsUnarchiver  inArchiveName trueName =
withObjCPtr inArchiveName $ \raw_inArchiveName ->
  withObjCPtr trueName $ \raw_trueName ->
      sendMsg nsUnarchiver (mkSelector "decodeClassName:asClassName:") retVoid [argPtr (castPtr raw_inArchiveName :: Ptr ()), argPtr (castPtr raw_trueName :: Ptr ())]

-- | @+ classNameDecodedForArchiveClassName:@
nsUnarchiverClassNameDecodedForArchiveClassName :: IsNSString inArchiveName => inArchiveName -> IO (Id NSString)
nsUnarchiverClassNameDecodedForArchiveClassName inArchiveName =
  do
    cls' <- getRequiredClass "NSUnarchiver"
    withObjCPtr inArchiveName $ \raw_inArchiveName ->
      sendClassMsg cls' (mkSelector "classNameDecodedForArchiveClassName:") (retPtr retVoid) [argPtr (castPtr raw_inArchiveName :: Ptr ())] >>= retainedObject . castPtr

-- | @- classNameDecodedForArchiveClassName:@
classNameDecodedForArchiveClassName :: (IsNSUnarchiver nsUnarchiver, IsNSString inArchiveName) => nsUnarchiver -> inArchiveName -> IO (Id NSString)
classNameDecodedForArchiveClassName nsUnarchiver  inArchiveName =
withObjCPtr inArchiveName $ \raw_inArchiveName ->
    sendMsg nsUnarchiver (mkSelector "classNameDecodedForArchiveClassName:") (retPtr retVoid) [argPtr (castPtr raw_inArchiveName :: Ptr ())] >>= retainedObject . castPtr

-- | @- replaceObject:withObject:@
replaceObject_withObject :: IsNSUnarchiver nsUnarchiver => nsUnarchiver -> RawId -> RawId -> IO ()
replaceObject_withObject nsUnarchiver  object newObject =
  sendMsg nsUnarchiver (mkSelector "replaceObject:withObject:") retVoid [argPtr (castPtr (unRawId object) :: Ptr ()), argPtr (castPtr (unRawId newObject) :: Ptr ())]

-- | @- atEnd@
atEnd :: IsNSUnarchiver nsUnarchiver => nsUnarchiver -> IO Bool
atEnd nsUnarchiver  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsUnarchiver (mkSelector "atEnd") retCULong []

-- | @- systemVersion@
systemVersion :: IsNSUnarchiver nsUnarchiver => nsUnarchiver -> IO CUInt
systemVersion nsUnarchiver  =
  sendMsg nsUnarchiver (mkSelector "systemVersion") retCUInt []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initForReadingWithData:@
initForReadingWithDataSelector :: Selector
initForReadingWithDataSelector = mkSelector "initForReadingWithData:"

-- | @Selector@ for @setObjectZone:@
setObjectZoneSelector :: Selector
setObjectZoneSelector = mkSelector "setObjectZone:"

-- | @Selector@ for @objectZone@
objectZoneSelector :: Selector
objectZoneSelector = mkSelector "objectZone"

-- | @Selector@ for @unarchiveObjectWithData:@
unarchiveObjectWithDataSelector :: Selector
unarchiveObjectWithDataSelector = mkSelector "unarchiveObjectWithData:"

-- | @Selector@ for @unarchiveObjectWithFile:@
unarchiveObjectWithFileSelector :: Selector
unarchiveObjectWithFileSelector = mkSelector "unarchiveObjectWithFile:"

-- | @Selector@ for @decodeClassName:asClassName:@
decodeClassName_asClassNameSelector :: Selector
decodeClassName_asClassNameSelector = mkSelector "decodeClassName:asClassName:"

-- | @Selector@ for @classNameDecodedForArchiveClassName:@
classNameDecodedForArchiveClassNameSelector :: Selector
classNameDecodedForArchiveClassNameSelector = mkSelector "classNameDecodedForArchiveClassName:"

-- | @Selector@ for @replaceObject:withObject:@
replaceObject_withObjectSelector :: Selector
replaceObject_withObjectSelector = mkSelector "replaceObject:withObject:"

-- | @Selector@ for @atEnd@
atEndSelector :: Selector
atEndSelector = mkSelector "atEnd"

-- | @Selector@ for @systemVersion@
systemVersionSelector :: Selector
systemVersionSelector = mkSelector "systemVersion"

