{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSOpenPanel@.
module ObjC.AppKit.NSOpenPanel
  ( NSOpenPanel
  , IsNSOpenPanel(..)
  , openPanel
  , filenames
  , beginSheetForDirectory_file_types_modalForWindow_modalDelegate_didEndSelector_contextInfo
  , beginForDirectory_file_types_modelessDelegate_didEndSelector_contextInfo
  , runModalForDirectory_file_types
  , runModalForTypes
  , urLs
  , resolvesAliases
  , setResolvesAliases
  , canChooseDirectories
  , setCanChooseDirectories
  , allowsMultipleSelection
  , setAllowsMultipleSelection
  , canChooseFiles
  , setCanChooseFiles
  , canResolveUbiquitousConflicts
  , setCanResolveUbiquitousConflicts
  , canDownloadUbiquitousContents
  , setCanDownloadUbiquitousContents
  , accessoryViewDisclosed
  , setAccessoryViewDisclosed
  , showsContentTypes
  , setShowsContentTypes
  , openPanelSelector
  , filenamesSelector
  , beginSheetForDirectory_file_types_modalForWindow_modalDelegate_didEndSelector_contextInfoSelector
  , beginForDirectory_file_types_modelessDelegate_didEndSelector_contextInfoSelector
  , runModalForDirectory_file_typesSelector
  , runModalForTypesSelector
  , urLsSelector
  , resolvesAliasesSelector
  , setResolvesAliasesSelector
  , canChooseDirectoriesSelector
  , setCanChooseDirectoriesSelector
  , allowsMultipleSelectionSelector
  , setAllowsMultipleSelectionSelector
  , canChooseFilesSelector
  , setCanChooseFilesSelector
  , canResolveUbiquitousConflictsSelector
  , setCanResolveUbiquitousConflictsSelector
  , canDownloadUbiquitousContentsSelector
  , setCanDownloadUbiquitousContentsSelector
  , accessoryViewDisclosedSelector
  , setAccessoryViewDisclosedSelector
  , showsContentTypesSelector
  , setShowsContentTypesSelector


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

-- | @+ openPanel@
openPanel :: IO (Id NSOpenPanel)
openPanel  =
  do
    cls' <- getRequiredClass "NSOpenPanel"
    sendClassMsg cls' (mkSelector "openPanel") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- filenames@
filenames :: IsNSOpenPanel nsOpenPanel => nsOpenPanel -> IO (Id NSArray)
filenames nsOpenPanel  =
  sendMsg nsOpenPanel (mkSelector "filenames") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- beginSheetForDirectory:file:types:modalForWindow:modalDelegate:didEndSelector:contextInfo:@
beginSheetForDirectory_file_types_modalForWindow_modalDelegate_didEndSelector_contextInfo :: (IsNSOpenPanel nsOpenPanel, IsNSString path, IsNSString name, IsNSArray fileTypes, IsNSWindow docWindow) => nsOpenPanel -> path -> name -> fileTypes -> docWindow -> RawId -> Selector -> Ptr () -> IO ()
beginSheetForDirectory_file_types_modalForWindow_modalDelegate_didEndSelector_contextInfo nsOpenPanel  path name fileTypes docWindow delegate didEndSelector contextInfo =
withObjCPtr path $ \raw_path ->
  withObjCPtr name $ \raw_name ->
    withObjCPtr fileTypes $ \raw_fileTypes ->
      withObjCPtr docWindow $ \raw_docWindow ->
          sendMsg nsOpenPanel (mkSelector "beginSheetForDirectory:file:types:modalForWindow:modalDelegate:didEndSelector:contextInfo:") retVoid [argPtr (castPtr raw_path :: Ptr ()), argPtr (castPtr raw_name :: Ptr ()), argPtr (castPtr raw_fileTypes :: Ptr ()), argPtr (castPtr raw_docWindow :: Ptr ()), argPtr (castPtr (unRawId delegate) :: Ptr ()), argPtr (unSelector didEndSelector), argPtr contextInfo]

-- | @- beginForDirectory:file:types:modelessDelegate:didEndSelector:contextInfo:@
beginForDirectory_file_types_modelessDelegate_didEndSelector_contextInfo :: (IsNSOpenPanel nsOpenPanel, IsNSString path, IsNSString name, IsNSArray fileTypes) => nsOpenPanel -> path -> name -> fileTypes -> RawId -> Selector -> Ptr () -> IO ()
beginForDirectory_file_types_modelessDelegate_didEndSelector_contextInfo nsOpenPanel  path name fileTypes delegate didEndSelector contextInfo =
withObjCPtr path $ \raw_path ->
  withObjCPtr name $ \raw_name ->
    withObjCPtr fileTypes $ \raw_fileTypes ->
        sendMsg nsOpenPanel (mkSelector "beginForDirectory:file:types:modelessDelegate:didEndSelector:contextInfo:") retVoid [argPtr (castPtr raw_path :: Ptr ()), argPtr (castPtr raw_name :: Ptr ()), argPtr (castPtr raw_fileTypes :: Ptr ()), argPtr (castPtr (unRawId delegate) :: Ptr ()), argPtr (unSelector didEndSelector), argPtr contextInfo]

-- | @- runModalForDirectory:file:types:@
runModalForDirectory_file_types :: (IsNSOpenPanel nsOpenPanel, IsNSString path, IsNSString name, IsNSArray fileTypes) => nsOpenPanel -> path -> name -> fileTypes -> IO CLong
runModalForDirectory_file_types nsOpenPanel  path name fileTypes =
withObjCPtr path $ \raw_path ->
  withObjCPtr name $ \raw_name ->
    withObjCPtr fileTypes $ \raw_fileTypes ->
        sendMsg nsOpenPanel (mkSelector "runModalForDirectory:file:types:") retCLong [argPtr (castPtr raw_path :: Ptr ()), argPtr (castPtr raw_name :: Ptr ()), argPtr (castPtr raw_fileTypes :: Ptr ())]

-- | @- runModalForTypes:@
runModalForTypes :: (IsNSOpenPanel nsOpenPanel, IsNSArray fileTypes) => nsOpenPanel -> fileTypes -> IO CLong
runModalForTypes nsOpenPanel  fileTypes =
withObjCPtr fileTypes $ \raw_fileTypes ->
    sendMsg nsOpenPanel (mkSelector "runModalForTypes:") retCLong [argPtr (castPtr raw_fileTypes :: Ptr ())]

-- | @- URLs@
urLs :: IsNSOpenPanel nsOpenPanel => nsOpenPanel -> IO (Id NSArray)
urLs nsOpenPanel  =
  sendMsg nsOpenPanel (mkSelector "URLs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- resolvesAliases@
resolvesAliases :: IsNSOpenPanel nsOpenPanel => nsOpenPanel -> IO Bool
resolvesAliases nsOpenPanel  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsOpenPanel (mkSelector "resolvesAliases") retCULong []

-- | @- setResolvesAliases:@
setResolvesAliases :: IsNSOpenPanel nsOpenPanel => nsOpenPanel -> Bool -> IO ()
setResolvesAliases nsOpenPanel  value =
  sendMsg nsOpenPanel (mkSelector "setResolvesAliases:") retVoid [argCULong (if value then 1 else 0)]

-- | @- canChooseDirectories@
canChooseDirectories :: IsNSOpenPanel nsOpenPanel => nsOpenPanel -> IO Bool
canChooseDirectories nsOpenPanel  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsOpenPanel (mkSelector "canChooseDirectories") retCULong []

-- | @- setCanChooseDirectories:@
setCanChooseDirectories :: IsNSOpenPanel nsOpenPanel => nsOpenPanel -> Bool -> IO ()
setCanChooseDirectories nsOpenPanel  value =
  sendMsg nsOpenPanel (mkSelector "setCanChooseDirectories:") retVoid [argCULong (if value then 1 else 0)]

-- | @- allowsMultipleSelection@
allowsMultipleSelection :: IsNSOpenPanel nsOpenPanel => nsOpenPanel -> IO Bool
allowsMultipleSelection nsOpenPanel  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsOpenPanel (mkSelector "allowsMultipleSelection") retCULong []

-- | @- setAllowsMultipleSelection:@
setAllowsMultipleSelection :: IsNSOpenPanel nsOpenPanel => nsOpenPanel -> Bool -> IO ()
setAllowsMultipleSelection nsOpenPanel  value =
  sendMsg nsOpenPanel (mkSelector "setAllowsMultipleSelection:") retVoid [argCULong (if value then 1 else 0)]

-- | @- canChooseFiles@
canChooseFiles :: IsNSOpenPanel nsOpenPanel => nsOpenPanel -> IO Bool
canChooseFiles nsOpenPanel  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsOpenPanel (mkSelector "canChooseFiles") retCULong []

-- | @- setCanChooseFiles:@
setCanChooseFiles :: IsNSOpenPanel nsOpenPanel => nsOpenPanel -> Bool -> IO ()
setCanChooseFiles nsOpenPanel  value =
  sendMsg nsOpenPanel (mkSelector "setCanChooseFiles:") retVoid [argCULong (if value then 1 else 0)]

-- | @- canResolveUbiquitousConflicts@
canResolveUbiquitousConflicts :: IsNSOpenPanel nsOpenPanel => nsOpenPanel -> IO Bool
canResolveUbiquitousConflicts nsOpenPanel  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsOpenPanel (mkSelector "canResolveUbiquitousConflicts") retCULong []

-- | @- setCanResolveUbiquitousConflicts:@
setCanResolveUbiquitousConflicts :: IsNSOpenPanel nsOpenPanel => nsOpenPanel -> Bool -> IO ()
setCanResolveUbiquitousConflicts nsOpenPanel  value =
  sendMsg nsOpenPanel (mkSelector "setCanResolveUbiquitousConflicts:") retVoid [argCULong (if value then 1 else 0)]

-- | @- canDownloadUbiquitousContents@
canDownloadUbiquitousContents :: IsNSOpenPanel nsOpenPanel => nsOpenPanel -> IO Bool
canDownloadUbiquitousContents nsOpenPanel  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsOpenPanel (mkSelector "canDownloadUbiquitousContents") retCULong []

-- | @- setCanDownloadUbiquitousContents:@
setCanDownloadUbiquitousContents :: IsNSOpenPanel nsOpenPanel => nsOpenPanel -> Bool -> IO ()
setCanDownloadUbiquitousContents nsOpenPanel  value =
  sendMsg nsOpenPanel (mkSelector "setCanDownloadUbiquitousContents:") retVoid [argCULong (if value then 1 else 0)]

-- | @- accessoryViewDisclosed@
accessoryViewDisclosed :: IsNSOpenPanel nsOpenPanel => nsOpenPanel -> IO Bool
accessoryViewDisclosed nsOpenPanel  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsOpenPanel (mkSelector "accessoryViewDisclosed") retCULong []

-- | @- setAccessoryViewDisclosed:@
setAccessoryViewDisclosed :: IsNSOpenPanel nsOpenPanel => nsOpenPanel -> Bool -> IO ()
setAccessoryViewDisclosed nsOpenPanel  value =
  sendMsg nsOpenPanel (mkSelector "setAccessoryViewDisclosed:") retVoid [argCULong (if value then 1 else 0)]

-- | @- showsContentTypes@
showsContentTypes :: IsNSOpenPanel nsOpenPanel => nsOpenPanel -> IO Bool
showsContentTypes nsOpenPanel  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsOpenPanel (mkSelector "showsContentTypes") retCULong []

-- | @- setShowsContentTypes:@
setShowsContentTypes :: IsNSOpenPanel nsOpenPanel => nsOpenPanel -> Bool -> IO ()
setShowsContentTypes nsOpenPanel  value =
  sendMsg nsOpenPanel (mkSelector "setShowsContentTypes:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @openPanel@
openPanelSelector :: Selector
openPanelSelector = mkSelector "openPanel"

-- | @Selector@ for @filenames@
filenamesSelector :: Selector
filenamesSelector = mkSelector "filenames"

-- | @Selector@ for @beginSheetForDirectory:file:types:modalForWindow:modalDelegate:didEndSelector:contextInfo:@
beginSheetForDirectory_file_types_modalForWindow_modalDelegate_didEndSelector_contextInfoSelector :: Selector
beginSheetForDirectory_file_types_modalForWindow_modalDelegate_didEndSelector_contextInfoSelector = mkSelector "beginSheetForDirectory:file:types:modalForWindow:modalDelegate:didEndSelector:contextInfo:"

-- | @Selector@ for @beginForDirectory:file:types:modelessDelegate:didEndSelector:contextInfo:@
beginForDirectory_file_types_modelessDelegate_didEndSelector_contextInfoSelector :: Selector
beginForDirectory_file_types_modelessDelegate_didEndSelector_contextInfoSelector = mkSelector "beginForDirectory:file:types:modelessDelegate:didEndSelector:contextInfo:"

-- | @Selector@ for @runModalForDirectory:file:types:@
runModalForDirectory_file_typesSelector :: Selector
runModalForDirectory_file_typesSelector = mkSelector "runModalForDirectory:file:types:"

-- | @Selector@ for @runModalForTypes:@
runModalForTypesSelector :: Selector
runModalForTypesSelector = mkSelector "runModalForTypes:"

-- | @Selector@ for @URLs@
urLsSelector :: Selector
urLsSelector = mkSelector "URLs"

-- | @Selector@ for @resolvesAliases@
resolvesAliasesSelector :: Selector
resolvesAliasesSelector = mkSelector "resolvesAliases"

-- | @Selector@ for @setResolvesAliases:@
setResolvesAliasesSelector :: Selector
setResolvesAliasesSelector = mkSelector "setResolvesAliases:"

-- | @Selector@ for @canChooseDirectories@
canChooseDirectoriesSelector :: Selector
canChooseDirectoriesSelector = mkSelector "canChooseDirectories"

-- | @Selector@ for @setCanChooseDirectories:@
setCanChooseDirectoriesSelector :: Selector
setCanChooseDirectoriesSelector = mkSelector "setCanChooseDirectories:"

-- | @Selector@ for @allowsMultipleSelection@
allowsMultipleSelectionSelector :: Selector
allowsMultipleSelectionSelector = mkSelector "allowsMultipleSelection"

-- | @Selector@ for @setAllowsMultipleSelection:@
setAllowsMultipleSelectionSelector :: Selector
setAllowsMultipleSelectionSelector = mkSelector "setAllowsMultipleSelection:"

-- | @Selector@ for @canChooseFiles@
canChooseFilesSelector :: Selector
canChooseFilesSelector = mkSelector "canChooseFiles"

-- | @Selector@ for @setCanChooseFiles:@
setCanChooseFilesSelector :: Selector
setCanChooseFilesSelector = mkSelector "setCanChooseFiles:"

-- | @Selector@ for @canResolveUbiquitousConflicts@
canResolveUbiquitousConflictsSelector :: Selector
canResolveUbiquitousConflictsSelector = mkSelector "canResolveUbiquitousConflicts"

-- | @Selector@ for @setCanResolveUbiquitousConflicts:@
setCanResolveUbiquitousConflictsSelector :: Selector
setCanResolveUbiquitousConflictsSelector = mkSelector "setCanResolveUbiquitousConflicts:"

-- | @Selector@ for @canDownloadUbiquitousContents@
canDownloadUbiquitousContentsSelector :: Selector
canDownloadUbiquitousContentsSelector = mkSelector "canDownloadUbiquitousContents"

-- | @Selector@ for @setCanDownloadUbiquitousContents:@
setCanDownloadUbiquitousContentsSelector :: Selector
setCanDownloadUbiquitousContentsSelector = mkSelector "setCanDownloadUbiquitousContents:"

-- | @Selector@ for @accessoryViewDisclosed@
accessoryViewDisclosedSelector :: Selector
accessoryViewDisclosedSelector = mkSelector "accessoryViewDisclosed"

-- | @Selector@ for @setAccessoryViewDisclosed:@
setAccessoryViewDisclosedSelector :: Selector
setAccessoryViewDisclosedSelector = mkSelector "setAccessoryViewDisclosed:"

-- | @Selector@ for @showsContentTypes@
showsContentTypesSelector :: Selector
showsContentTypesSelector = mkSelector "showsContentTypes"

-- | @Selector@ for @setShowsContentTypes:@
setShowsContentTypesSelector :: Selector
setShowsContentTypesSelector = mkSelector "setShowsContentTypes:"

