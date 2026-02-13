{-# LANGUAGE DataKinds #-}
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
  , allKeysSelector
  , availableColorListsSelector
  , colorListNamedSelector
  , colorWithKeySelector
  , editableSelector
  , initWithNameSelector
  , initWithName_fromFileSelector
  , insertColor_key_atIndexSelector
  , nameSelector
  , removeColorWithKeySelector
  , removeFileSelector
  , setColor_forKeySelector
  , writeToFileSelector
  , writeToURL_errorSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ colorListNamed:@
colorListNamed :: IsNSString name => name -> IO (Id NSColorList)
colorListNamed name =
  do
    cls' <- getRequiredClass "NSColorList"
    sendClassMessage cls' colorListNamedSelector (toNSString name)

-- | @- initWithName:@
initWithName :: (IsNSColorList nsColorList, IsNSString name) => nsColorList -> name -> IO (Id NSColorList)
initWithName nsColorList name =
  sendOwnedMessage nsColorList initWithNameSelector (toNSString name)

-- | @- initWithName:fromFile:@
initWithName_fromFile :: (IsNSColorList nsColorList, IsNSString name, IsNSString path) => nsColorList -> name -> path -> IO (Id NSColorList)
initWithName_fromFile nsColorList name path =
  sendOwnedMessage nsColorList initWithName_fromFileSelector (toNSString name) (toNSString path)

-- | @- setColor:forKey:@
setColor_forKey :: (IsNSColorList nsColorList, IsNSColor color, IsNSString key) => nsColorList -> color -> key -> IO ()
setColor_forKey nsColorList color key =
  sendMessage nsColorList setColor_forKeySelector (toNSColor color) (toNSString key)

-- | @- insertColor:key:atIndex:@
insertColor_key_atIndex :: (IsNSColorList nsColorList, IsNSColor color, IsNSString key) => nsColorList -> color -> key -> CULong -> IO ()
insertColor_key_atIndex nsColorList color key loc =
  sendMessage nsColorList insertColor_key_atIndexSelector (toNSColor color) (toNSString key) loc

-- | @- removeColorWithKey:@
removeColorWithKey :: (IsNSColorList nsColorList, IsNSString key) => nsColorList -> key -> IO ()
removeColorWithKey nsColorList key =
  sendMessage nsColorList removeColorWithKeySelector (toNSString key)

-- | @- colorWithKey:@
colorWithKey :: (IsNSColorList nsColorList, IsNSString key) => nsColorList -> key -> IO (Id NSColor)
colorWithKey nsColorList key =
  sendMessage nsColorList colorWithKeySelector (toNSString key)

-- | @- writeToURL:error:@
writeToURL_error :: (IsNSColorList nsColorList, IsNSURL url, IsNSError errPtr) => nsColorList -> url -> errPtr -> IO Bool
writeToURL_error nsColorList url errPtr =
  sendMessage nsColorList writeToURL_errorSelector (toNSURL url) (toNSError errPtr)

-- | @- writeToFile:@
writeToFile :: (IsNSColorList nsColorList, IsNSString path) => nsColorList -> path -> IO Bool
writeToFile nsColorList path =
  sendMessage nsColorList writeToFileSelector (toNSString path)

-- | @- removeFile@
removeFile :: IsNSColorList nsColorList => nsColorList -> IO ()
removeFile nsColorList =
  sendMessage nsColorList removeFileSelector

-- | @+ availableColorLists@
availableColorLists :: IO (Id NSArray)
availableColorLists  =
  do
    cls' <- getRequiredClass "NSColorList"
    sendClassMessage cls' availableColorListsSelector

-- | @- name@
name :: IsNSColorList nsColorList => nsColorList -> IO (Id NSString)
name nsColorList =
  sendMessage nsColorList nameSelector

-- | @- allKeys@
allKeys :: IsNSColorList nsColorList => nsColorList -> IO (Id NSArray)
allKeys nsColorList =
  sendMessage nsColorList allKeysSelector

-- | @- editable@
editable :: IsNSColorList nsColorList => nsColorList -> IO Bool
editable nsColorList =
  sendMessage nsColorList editableSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @colorListNamed:@
colorListNamedSelector :: Selector '[Id NSString] (Id NSColorList)
colorListNamedSelector = mkSelector "colorListNamed:"

-- | @Selector@ for @initWithName:@
initWithNameSelector :: Selector '[Id NSString] (Id NSColorList)
initWithNameSelector = mkSelector "initWithName:"

-- | @Selector@ for @initWithName:fromFile:@
initWithName_fromFileSelector :: Selector '[Id NSString, Id NSString] (Id NSColorList)
initWithName_fromFileSelector = mkSelector "initWithName:fromFile:"

-- | @Selector@ for @setColor:forKey:@
setColor_forKeySelector :: Selector '[Id NSColor, Id NSString] ()
setColor_forKeySelector = mkSelector "setColor:forKey:"

-- | @Selector@ for @insertColor:key:atIndex:@
insertColor_key_atIndexSelector :: Selector '[Id NSColor, Id NSString, CULong] ()
insertColor_key_atIndexSelector = mkSelector "insertColor:key:atIndex:"

-- | @Selector@ for @removeColorWithKey:@
removeColorWithKeySelector :: Selector '[Id NSString] ()
removeColorWithKeySelector = mkSelector "removeColorWithKey:"

-- | @Selector@ for @colorWithKey:@
colorWithKeySelector :: Selector '[Id NSString] (Id NSColor)
colorWithKeySelector = mkSelector "colorWithKey:"

-- | @Selector@ for @writeToURL:error:@
writeToURL_errorSelector :: Selector '[Id NSURL, Id NSError] Bool
writeToURL_errorSelector = mkSelector "writeToURL:error:"

-- | @Selector@ for @writeToFile:@
writeToFileSelector :: Selector '[Id NSString] Bool
writeToFileSelector = mkSelector "writeToFile:"

-- | @Selector@ for @removeFile@
removeFileSelector :: Selector '[] ()
removeFileSelector = mkSelector "removeFile"

-- | @Selector@ for @availableColorLists@
availableColorListsSelector :: Selector '[] (Id NSArray)
availableColorListsSelector = mkSelector "availableColorLists"

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

-- | @Selector@ for @allKeys@
allKeysSelector :: Selector '[] (Id NSArray)
allKeysSelector = mkSelector "allKeys"

-- | @Selector@ for @editable@
editableSelector :: Selector '[] Bool
editableSelector = mkSelector "editable"

