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
  , initForWritingWithMutableDataSelector
  , encodeRootObjectSelector
  , encodeConditionalObjectSelector
  , archivedDataWithRootObjectSelector
  , archiveRootObject_toFileSelector
  , encodeClassName_intoClassNameSelector
  , classNameEncodedForTrueClassNameSelector
  , replaceObject_withObjectSelector
  , archiverDataSelector


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

-- | @- initForWritingWithMutableData:@
initForWritingWithMutableData :: (IsNSArchiver nsArchiver, IsNSMutableData mdata) => nsArchiver -> mdata -> IO (Id NSArchiver)
initForWritingWithMutableData nsArchiver  mdata =
withObjCPtr mdata $ \raw_mdata ->
    sendMsg nsArchiver (mkSelector "initForWritingWithMutableData:") (retPtr retVoid) [argPtr (castPtr raw_mdata :: Ptr ())] >>= ownedObject . castPtr

-- | @- encodeRootObject:@
encodeRootObject :: IsNSArchiver nsArchiver => nsArchiver -> RawId -> IO ()
encodeRootObject nsArchiver  rootObject =
  sendMsg nsArchiver (mkSelector "encodeRootObject:") retVoid [argPtr (castPtr (unRawId rootObject) :: Ptr ())]

-- | @- encodeConditionalObject:@
encodeConditionalObject :: IsNSArchiver nsArchiver => nsArchiver -> RawId -> IO ()
encodeConditionalObject nsArchiver  object =
  sendMsg nsArchiver (mkSelector "encodeConditionalObject:") retVoid [argPtr (castPtr (unRawId object) :: Ptr ())]

-- | @+ archivedDataWithRootObject:@
archivedDataWithRootObject :: RawId -> IO (Id NSData)
archivedDataWithRootObject rootObject =
  do
    cls' <- getRequiredClass "NSArchiver"
    sendClassMsg cls' (mkSelector "archivedDataWithRootObject:") (retPtr retVoid) [argPtr (castPtr (unRawId rootObject) :: Ptr ())] >>= retainedObject . castPtr

-- | @+ archiveRootObject:toFile:@
archiveRootObject_toFile :: IsNSString path => RawId -> path -> IO Bool
archiveRootObject_toFile rootObject path =
  do
    cls' <- getRequiredClass "NSArchiver"
    withObjCPtr path $ \raw_path ->
      fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "archiveRootObject:toFile:") retCULong [argPtr (castPtr (unRawId rootObject) :: Ptr ()), argPtr (castPtr raw_path :: Ptr ())]

-- | @- encodeClassName:intoClassName:@
encodeClassName_intoClassName :: (IsNSArchiver nsArchiver, IsNSString trueName, IsNSString inArchiveName) => nsArchiver -> trueName -> inArchiveName -> IO ()
encodeClassName_intoClassName nsArchiver  trueName inArchiveName =
withObjCPtr trueName $ \raw_trueName ->
  withObjCPtr inArchiveName $ \raw_inArchiveName ->
      sendMsg nsArchiver (mkSelector "encodeClassName:intoClassName:") retVoid [argPtr (castPtr raw_trueName :: Ptr ()), argPtr (castPtr raw_inArchiveName :: Ptr ())]

-- | @- classNameEncodedForTrueClassName:@
classNameEncodedForTrueClassName :: (IsNSArchiver nsArchiver, IsNSString trueName) => nsArchiver -> trueName -> IO (Id NSString)
classNameEncodedForTrueClassName nsArchiver  trueName =
withObjCPtr trueName $ \raw_trueName ->
    sendMsg nsArchiver (mkSelector "classNameEncodedForTrueClassName:") (retPtr retVoid) [argPtr (castPtr raw_trueName :: Ptr ())] >>= retainedObject . castPtr

-- | @- replaceObject:withObject:@
replaceObject_withObject :: IsNSArchiver nsArchiver => nsArchiver -> RawId -> RawId -> IO ()
replaceObject_withObject nsArchiver  object newObject =
  sendMsg nsArchiver (mkSelector "replaceObject:withObject:") retVoid [argPtr (castPtr (unRawId object) :: Ptr ()), argPtr (castPtr (unRawId newObject) :: Ptr ())]

-- | @- archiverData@
archiverData :: IsNSArchiver nsArchiver => nsArchiver -> IO (Id NSMutableData)
archiverData nsArchiver  =
  sendMsg nsArchiver (mkSelector "archiverData") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initForWritingWithMutableData:@
initForWritingWithMutableDataSelector :: Selector
initForWritingWithMutableDataSelector = mkSelector "initForWritingWithMutableData:"

-- | @Selector@ for @encodeRootObject:@
encodeRootObjectSelector :: Selector
encodeRootObjectSelector = mkSelector "encodeRootObject:"

-- | @Selector@ for @encodeConditionalObject:@
encodeConditionalObjectSelector :: Selector
encodeConditionalObjectSelector = mkSelector "encodeConditionalObject:"

-- | @Selector@ for @archivedDataWithRootObject:@
archivedDataWithRootObjectSelector :: Selector
archivedDataWithRootObjectSelector = mkSelector "archivedDataWithRootObject:"

-- | @Selector@ for @archiveRootObject:toFile:@
archiveRootObject_toFileSelector :: Selector
archiveRootObject_toFileSelector = mkSelector "archiveRootObject:toFile:"

-- | @Selector@ for @encodeClassName:intoClassName:@
encodeClassName_intoClassNameSelector :: Selector
encodeClassName_intoClassNameSelector = mkSelector "encodeClassName:intoClassName:"

-- | @Selector@ for @classNameEncodedForTrueClassName:@
classNameEncodedForTrueClassNameSelector :: Selector
classNameEncodedForTrueClassNameSelector = mkSelector "classNameEncodedForTrueClassName:"

-- | @Selector@ for @replaceObject:withObject:@
replaceObject_withObjectSelector :: Selector
replaceObject_withObjectSelector = mkSelector "replaceObject:withObject:"

-- | @Selector@ for @archiverData@
archiverDataSelector :: Selector
archiverDataSelector = mkSelector "archiverData"

