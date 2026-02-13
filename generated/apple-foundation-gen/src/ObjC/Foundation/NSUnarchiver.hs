{-# LANGUAGE DataKinds #-}
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
  , atEndSelector
  , classNameDecodedForArchiveClassNameSelector
  , decodeClassName_asClassNameSelector
  , initForReadingWithDataSelector
  , nsUnarchiverClassNameDecodedForArchiveClassNameSelector
  , nsUnarchiverDecodeClassName_asClassNameSelector
  , objectZoneSelector
  , replaceObject_withObjectSelector
  , setObjectZoneSelector
  , systemVersionSelector
  , unarchiveObjectWithDataSelector
  , unarchiveObjectWithFileSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes

-- | @- initForReadingWithData:@
initForReadingWithData :: (IsNSUnarchiver nsUnarchiver, IsNSData data_) => nsUnarchiver -> data_ -> IO (Id NSUnarchiver)
initForReadingWithData nsUnarchiver data_ =
  sendOwnedMessage nsUnarchiver initForReadingWithDataSelector (toNSData data_)

-- | @- setObjectZone:@
setObjectZone :: IsNSUnarchiver nsUnarchiver => nsUnarchiver -> Ptr () -> IO ()
setObjectZone nsUnarchiver zone =
  sendMessage nsUnarchiver setObjectZoneSelector zone

-- | @- objectZone@
objectZone :: IsNSUnarchiver nsUnarchiver => nsUnarchiver -> IO (Ptr ())
objectZone nsUnarchiver =
  sendMessage nsUnarchiver objectZoneSelector

-- | @+ unarchiveObjectWithData:@
unarchiveObjectWithData :: IsNSData data_ => data_ -> IO RawId
unarchiveObjectWithData data_ =
  do
    cls' <- getRequiredClass "NSUnarchiver"
    sendClassMessage cls' unarchiveObjectWithDataSelector (toNSData data_)

-- | @+ unarchiveObjectWithFile:@
unarchiveObjectWithFile :: IsNSString path => path -> IO RawId
unarchiveObjectWithFile path =
  do
    cls' <- getRequiredClass "NSUnarchiver"
    sendClassMessage cls' unarchiveObjectWithFileSelector (toNSString path)

-- | @+ decodeClassName:asClassName:@
nsUnarchiverDecodeClassName_asClassName :: (IsNSString inArchiveName, IsNSString trueName) => inArchiveName -> trueName -> IO ()
nsUnarchiverDecodeClassName_asClassName inArchiveName trueName =
  do
    cls' <- getRequiredClass "NSUnarchiver"
    sendClassMessage cls' nsUnarchiverDecodeClassName_asClassNameSelector (toNSString inArchiveName) (toNSString trueName)

-- | @- decodeClassName:asClassName:@
decodeClassName_asClassName :: (IsNSUnarchiver nsUnarchiver, IsNSString inArchiveName, IsNSString trueName) => nsUnarchiver -> inArchiveName -> trueName -> IO ()
decodeClassName_asClassName nsUnarchiver inArchiveName trueName =
  sendMessage nsUnarchiver decodeClassName_asClassNameSelector (toNSString inArchiveName) (toNSString trueName)

-- | @+ classNameDecodedForArchiveClassName:@
nsUnarchiverClassNameDecodedForArchiveClassName :: IsNSString inArchiveName => inArchiveName -> IO (Id NSString)
nsUnarchiverClassNameDecodedForArchiveClassName inArchiveName =
  do
    cls' <- getRequiredClass "NSUnarchiver"
    sendClassMessage cls' nsUnarchiverClassNameDecodedForArchiveClassNameSelector (toNSString inArchiveName)

-- | @- classNameDecodedForArchiveClassName:@
classNameDecodedForArchiveClassName :: (IsNSUnarchiver nsUnarchiver, IsNSString inArchiveName) => nsUnarchiver -> inArchiveName -> IO (Id NSString)
classNameDecodedForArchiveClassName nsUnarchiver inArchiveName =
  sendMessage nsUnarchiver classNameDecodedForArchiveClassNameSelector (toNSString inArchiveName)

-- | @- replaceObject:withObject:@
replaceObject_withObject :: IsNSUnarchiver nsUnarchiver => nsUnarchiver -> RawId -> RawId -> IO ()
replaceObject_withObject nsUnarchiver object newObject =
  sendMessage nsUnarchiver replaceObject_withObjectSelector object newObject

-- | @- atEnd@
atEnd :: IsNSUnarchiver nsUnarchiver => nsUnarchiver -> IO Bool
atEnd nsUnarchiver =
  sendMessage nsUnarchiver atEndSelector

-- | @- systemVersion@
systemVersion :: IsNSUnarchiver nsUnarchiver => nsUnarchiver -> IO CUInt
systemVersion nsUnarchiver =
  sendMessage nsUnarchiver systemVersionSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initForReadingWithData:@
initForReadingWithDataSelector :: Selector '[Id NSData] (Id NSUnarchiver)
initForReadingWithDataSelector = mkSelector "initForReadingWithData:"

-- | @Selector@ for @setObjectZone:@
setObjectZoneSelector :: Selector '[Ptr ()] ()
setObjectZoneSelector = mkSelector "setObjectZone:"

-- | @Selector@ for @objectZone@
objectZoneSelector :: Selector '[] (Ptr ())
objectZoneSelector = mkSelector "objectZone"

-- | @Selector@ for @unarchiveObjectWithData:@
unarchiveObjectWithDataSelector :: Selector '[Id NSData] RawId
unarchiveObjectWithDataSelector = mkSelector "unarchiveObjectWithData:"

-- | @Selector@ for @unarchiveObjectWithFile:@
unarchiveObjectWithFileSelector :: Selector '[Id NSString] RawId
unarchiveObjectWithFileSelector = mkSelector "unarchiveObjectWithFile:"

-- | @Selector@ for @decodeClassName:asClassName:@
nsUnarchiverDecodeClassName_asClassNameSelector :: Selector '[Id NSString, Id NSString] ()
nsUnarchiverDecodeClassName_asClassNameSelector = mkSelector "decodeClassName:asClassName:"

-- | @Selector@ for @decodeClassName:asClassName:@
decodeClassName_asClassNameSelector :: Selector '[Id NSString, Id NSString] ()
decodeClassName_asClassNameSelector = mkSelector "decodeClassName:asClassName:"

-- | @Selector@ for @classNameDecodedForArchiveClassName:@
nsUnarchiverClassNameDecodedForArchiveClassNameSelector :: Selector '[Id NSString] (Id NSString)
nsUnarchiverClassNameDecodedForArchiveClassNameSelector = mkSelector "classNameDecodedForArchiveClassName:"

-- | @Selector@ for @classNameDecodedForArchiveClassName:@
classNameDecodedForArchiveClassNameSelector :: Selector '[Id NSString] (Id NSString)
classNameDecodedForArchiveClassNameSelector = mkSelector "classNameDecodedForArchiveClassName:"

-- | @Selector@ for @replaceObject:withObject:@
replaceObject_withObjectSelector :: Selector '[RawId, RawId] ()
replaceObject_withObjectSelector = mkSelector "replaceObject:withObject:"

-- | @Selector@ for @atEnd@
atEndSelector :: Selector '[] Bool
atEndSelector = mkSelector "atEnd"

-- | @Selector@ for @systemVersion@
systemVersionSelector :: Selector '[] CUInt
systemVersionSelector = mkSelector "systemVersion"

