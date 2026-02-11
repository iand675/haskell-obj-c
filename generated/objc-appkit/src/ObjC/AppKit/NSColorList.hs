{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSColorList@.
module ObjC.AppKit.NSColorList
  ( NSColorList
  , IsNSColorList(..)
  , colorListNamed
  , initWithName
  , initWithName_fromFile
  , setColor_forKey
  , insertColor_key_atIndex
  , removeColorWithKey
  , colorWithKey
  , writeToURL_error
  , writeToFile
  , removeFile
  , availableColorLists
  , name
  , allKeys
  , editable
  , colorListNamedSelector
  , initWithNameSelector
  , initWithName_fromFileSelector
  , setColor_forKeySelector
  , insertColor_key_atIndexSelector
  , removeColorWithKeySelector
  , colorWithKeySelector
  , writeToURL_errorSelector
  , writeToFileSelector
  , removeFileSelector
  , availableColorListsSelector
  , nameSelector
  , allKeysSelector
  , editableSelector


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

-- | @+ colorListNamed:@
colorListNamed :: IsNSString name => name -> IO (Id NSColorList)
colorListNamed name =
  do
    cls' <- getRequiredClass "NSColorList"
    withObjCPtr name $ \raw_name ->
      sendClassMsg cls' (mkSelector "colorListNamed:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ())] >>= retainedObject . castPtr

-- | @- initWithName:@
initWithName :: (IsNSColorList nsColorList, IsNSString name) => nsColorList -> name -> IO (Id NSColorList)
initWithName nsColorList  name =
withObjCPtr name $ \raw_name ->
    sendMsg nsColorList (mkSelector "initWithName:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithName:fromFile:@
initWithName_fromFile :: (IsNSColorList nsColorList, IsNSString name, IsNSString path) => nsColorList -> name -> path -> IO (Id NSColorList)
initWithName_fromFile nsColorList  name path =
withObjCPtr name $ \raw_name ->
  withObjCPtr path $ \raw_path ->
      sendMsg nsColorList (mkSelector "initWithName:fromFile:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ()), argPtr (castPtr raw_path :: Ptr ())] >>= ownedObject . castPtr

-- | @- setColor:forKey:@
setColor_forKey :: (IsNSColorList nsColorList, IsNSColor color, IsNSString key) => nsColorList -> color -> key -> IO ()
setColor_forKey nsColorList  color key =
withObjCPtr color $ \raw_color ->
  withObjCPtr key $ \raw_key ->
      sendMsg nsColorList (mkSelector "setColor:forKey:") retVoid [argPtr (castPtr raw_color :: Ptr ()), argPtr (castPtr raw_key :: Ptr ())]

-- | @- insertColor:key:atIndex:@
insertColor_key_atIndex :: (IsNSColorList nsColorList, IsNSColor color, IsNSString key) => nsColorList -> color -> key -> CULong -> IO ()
insertColor_key_atIndex nsColorList  color key loc =
withObjCPtr color $ \raw_color ->
  withObjCPtr key $ \raw_key ->
      sendMsg nsColorList (mkSelector "insertColor:key:atIndex:") retVoid [argPtr (castPtr raw_color :: Ptr ()), argPtr (castPtr raw_key :: Ptr ()), argCULong (fromIntegral loc)]

-- | @- removeColorWithKey:@
removeColorWithKey :: (IsNSColorList nsColorList, IsNSString key) => nsColorList -> key -> IO ()
removeColorWithKey nsColorList  key =
withObjCPtr key $ \raw_key ->
    sendMsg nsColorList (mkSelector "removeColorWithKey:") retVoid [argPtr (castPtr raw_key :: Ptr ())]

-- | @- colorWithKey:@
colorWithKey :: (IsNSColorList nsColorList, IsNSString key) => nsColorList -> key -> IO (Id NSColor)
colorWithKey nsColorList  key =
withObjCPtr key $ \raw_key ->
    sendMsg nsColorList (mkSelector "colorWithKey:") (retPtr retVoid) [argPtr (castPtr raw_key :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeToURL:error:@
writeToURL_error :: (IsNSColorList nsColorList, IsNSURL url, IsNSError errPtr) => nsColorList -> url -> errPtr -> IO Bool
writeToURL_error nsColorList  url errPtr =
withObjCPtr url $ \raw_url ->
  withObjCPtr errPtr $ \raw_errPtr ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsColorList (mkSelector "writeToURL:error:") retCULong [argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr raw_errPtr :: Ptr ())]

-- | @- writeToFile:@
writeToFile :: (IsNSColorList nsColorList, IsNSString path) => nsColorList -> path -> IO Bool
writeToFile nsColorList  path =
withObjCPtr path $ \raw_path ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsColorList (mkSelector "writeToFile:") retCULong [argPtr (castPtr raw_path :: Ptr ())]

-- | @- removeFile@
removeFile :: IsNSColorList nsColorList => nsColorList -> IO ()
removeFile nsColorList  =
  sendMsg nsColorList (mkSelector "removeFile") retVoid []

-- | @+ availableColorLists@
availableColorLists :: IO (Id NSArray)
availableColorLists  =
  do
    cls' <- getRequiredClass "NSColorList"
    sendClassMsg cls' (mkSelector "availableColorLists") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- name@
name :: IsNSColorList nsColorList => nsColorList -> IO (Id NSString)
name nsColorList  =
  sendMsg nsColorList (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- allKeys@
allKeys :: IsNSColorList nsColorList => nsColorList -> IO (Id NSArray)
allKeys nsColorList  =
  sendMsg nsColorList (mkSelector "allKeys") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- editable@
editable :: IsNSColorList nsColorList => nsColorList -> IO Bool
editable nsColorList  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsColorList (mkSelector "editable") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @colorListNamed:@
colorListNamedSelector :: Selector
colorListNamedSelector = mkSelector "colorListNamed:"

-- | @Selector@ for @initWithName:@
initWithNameSelector :: Selector
initWithNameSelector = mkSelector "initWithName:"

-- | @Selector@ for @initWithName:fromFile:@
initWithName_fromFileSelector :: Selector
initWithName_fromFileSelector = mkSelector "initWithName:fromFile:"

-- | @Selector@ for @setColor:forKey:@
setColor_forKeySelector :: Selector
setColor_forKeySelector = mkSelector "setColor:forKey:"

-- | @Selector@ for @insertColor:key:atIndex:@
insertColor_key_atIndexSelector :: Selector
insertColor_key_atIndexSelector = mkSelector "insertColor:key:atIndex:"

-- | @Selector@ for @removeColorWithKey:@
removeColorWithKeySelector :: Selector
removeColorWithKeySelector = mkSelector "removeColorWithKey:"

-- | @Selector@ for @colorWithKey:@
colorWithKeySelector :: Selector
colorWithKeySelector = mkSelector "colorWithKey:"

-- | @Selector@ for @writeToURL:error:@
writeToURL_errorSelector :: Selector
writeToURL_errorSelector = mkSelector "writeToURL:error:"

-- | @Selector@ for @writeToFile:@
writeToFileSelector :: Selector
writeToFileSelector = mkSelector "writeToFile:"

-- | @Selector@ for @removeFile@
removeFileSelector :: Selector
removeFileSelector = mkSelector "removeFile"

-- | @Selector@ for @availableColorLists@
availableColorListsSelector :: Selector
availableColorListsSelector = mkSelector "availableColorLists"

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

-- | @Selector@ for @allKeys@
allKeysSelector :: Selector
allKeysSelector = mkSelector "allKeys"

-- | @Selector@ for @editable@
editableSelector :: Selector
editableSelector = mkSelector "editable"

