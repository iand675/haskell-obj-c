{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | **********		Archiving: Writing	***************
--
-- Generated bindings for @NSArchiver@.
module ObjC.Foundation.NSArchiver
  ( NSArchiver
  , IsNSArchiver(..)
  , initForWritingWithMutableData
  , encodeRootObject
  , encodeConditionalObject
  , archivedDataWithRootObject
  , archiveRootObject_toFile
  , encodeClassName_intoClassName
  , classNameEncodedForTrueClassName
  , replaceObject_withObject
  , archiverData
  , archiveRootObject_toFileSelector
  , archivedDataWithRootObjectSelector
  , archiverDataSelector
  , classNameEncodedForTrueClassNameSelector
  , encodeClassName_intoClassNameSelector
  , encodeConditionalObjectSelector
  , encodeRootObjectSelector
  , initForWritingWithMutableDataSelector
  , replaceObject_withObjectSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes

-- | @- initForWritingWithMutableData:@
initForWritingWithMutableData :: (IsNSArchiver nsArchiver, IsNSMutableData mdata) => nsArchiver -> mdata -> IO (Id NSArchiver)
initForWritingWithMutableData nsArchiver mdata =
  sendOwnedMessage nsArchiver initForWritingWithMutableDataSelector (toNSMutableData mdata)

-- | @- encodeRootObject:@
encodeRootObject :: IsNSArchiver nsArchiver => nsArchiver -> RawId -> IO ()
encodeRootObject nsArchiver rootObject =
  sendMessage nsArchiver encodeRootObjectSelector rootObject

-- | @- encodeConditionalObject:@
encodeConditionalObject :: IsNSArchiver nsArchiver => nsArchiver -> RawId -> IO ()
encodeConditionalObject nsArchiver object =
  sendMessage nsArchiver encodeConditionalObjectSelector object

-- | @+ archivedDataWithRootObject:@
archivedDataWithRootObject :: RawId -> IO (Id NSData)
archivedDataWithRootObject rootObject =
  do
    cls' <- getRequiredClass "NSArchiver"
    sendClassMessage cls' archivedDataWithRootObjectSelector rootObject

-- | @+ archiveRootObject:toFile:@
archiveRootObject_toFile :: IsNSString path => RawId -> path -> IO Bool
archiveRootObject_toFile rootObject path =
  do
    cls' <- getRequiredClass "NSArchiver"
    sendClassMessage cls' archiveRootObject_toFileSelector rootObject (toNSString path)

-- | @- encodeClassName:intoClassName:@
encodeClassName_intoClassName :: (IsNSArchiver nsArchiver, IsNSString trueName, IsNSString inArchiveName) => nsArchiver -> trueName -> inArchiveName -> IO ()
encodeClassName_intoClassName nsArchiver trueName inArchiveName =
  sendMessage nsArchiver encodeClassName_intoClassNameSelector (toNSString trueName) (toNSString inArchiveName)

-- | @- classNameEncodedForTrueClassName:@
classNameEncodedForTrueClassName :: (IsNSArchiver nsArchiver, IsNSString trueName) => nsArchiver -> trueName -> IO (Id NSString)
classNameEncodedForTrueClassName nsArchiver trueName =
  sendMessage nsArchiver classNameEncodedForTrueClassNameSelector (toNSString trueName)

-- | @- replaceObject:withObject:@
replaceObject_withObject :: IsNSArchiver nsArchiver => nsArchiver -> RawId -> RawId -> IO ()
replaceObject_withObject nsArchiver object newObject =
  sendMessage nsArchiver replaceObject_withObjectSelector object newObject

-- | @- archiverData@
archiverData :: IsNSArchiver nsArchiver => nsArchiver -> IO (Id NSMutableData)
archiverData nsArchiver =
  sendMessage nsArchiver archiverDataSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initForWritingWithMutableData:@
initForWritingWithMutableDataSelector :: Selector '[Id NSMutableData] (Id NSArchiver)
initForWritingWithMutableDataSelector = mkSelector "initForWritingWithMutableData:"

-- | @Selector@ for @encodeRootObject:@
encodeRootObjectSelector :: Selector '[RawId] ()
encodeRootObjectSelector = mkSelector "encodeRootObject:"

-- | @Selector@ for @encodeConditionalObject:@
encodeConditionalObjectSelector :: Selector '[RawId] ()
encodeConditionalObjectSelector = mkSelector "encodeConditionalObject:"

-- | @Selector@ for @archivedDataWithRootObject:@
archivedDataWithRootObjectSelector :: Selector '[RawId] (Id NSData)
archivedDataWithRootObjectSelector = mkSelector "archivedDataWithRootObject:"

-- | @Selector@ for @archiveRootObject:toFile:@
archiveRootObject_toFileSelector :: Selector '[RawId, Id NSString] Bool
archiveRootObject_toFileSelector = mkSelector "archiveRootObject:toFile:"

-- | @Selector@ for @encodeClassName:intoClassName:@
encodeClassName_intoClassNameSelector :: Selector '[Id NSString, Id NSString] ()
encodeClassName_intoClassNameSelector = mkSelector "encodeClassName:intoClassName:"

-- | @Selector@ for @classNameEncodedForTrueClassName:@
classNameEncodedForTrueClassNameSelector :: Selector '[Id NSString] (Id NSString)
classNameEncodedForTrueClassNameSelector = mkSelector "classNameEncodedForTrueClassName:"

-- | @Selector@ for @replaceObject:withObject:@
replaceObject_withObjectSelector :: Selector '[RawId, RawId] ()
replaceObject_withObjectSelector = mkSelector "replaceObject:withObject:"

-- | @Selector@ for @archiverData@
archiverDataSelector :: Selector '[] (Id NSMutableData)
archiverDataSelector = mkSelector "archiverData"

