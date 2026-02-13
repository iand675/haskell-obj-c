{-# LANGUAGE DataKinds #-}
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
  , accessoryViewDisclosedSelector
  , allowsMultipleSelectionSelector
  , beginForDirectory_file_types_modelessDelegate_didEndSelector_contextInfoSelector
  , beginSheetForDirectory_file_types_modalForWindow_modalDelegate_didEndSelector_contextInfoSelector
  , canChooseDirectoriesSelector
  , canChooseFilesSelector
  , canDownloadUbiquitousContentsSelector
  , canResolveUbiquitousConflictsSelector
  , filenamesSelector
  , openPanelSelector
  , resolvesAliasesSelector
  , runModalForDirectory_file_typesSelector
  , runModalForTypesSelector
  , setAccessoryViewDisclosedSelector
  , setAllowsMultipleSelectionSelector
  , setCanChooseDirectoriesSelector
  , setCanChooseFilesSelector
  , setCanDownloadUbiquitousContentsSelector
  , setCanResolveUbiquitousConflictsSelector
  , setResolvesAliasesSelector
  , setShowsContentTypesSelector
  , showsContentTypesSelector
  , urLsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ openPanel@
openPanel :: IO (Id NSOpenPanel)
openPanel  =
  do
    cls' <- getRequiredClass "NSOpenPanel"
    sendClassMessage cls' openPanelSelector

-- | @- filenames@
filenames :: IsNSOpenPanel nsOpenPanel => nsOpenPanel -> IO (Id NSArray)
filenames nsOpenPanel =
  sendMessage nsOpenPanel filenamesSelector

-- | @- beginSheetForDirectory:file:types:modalForWindow:modalDelegate:didEndSelector:contextInfo:@
beginSheetForDirectory_file_types_modalForWindow_modalDelegate_didEndSelector_contextInfo :: (IsNSOpenPanel nsOpenPanel, IsNSString path, IsNSString name, IsNSArray fileTypes, IsNSWindow docWindow) => nsOpenPanel -> path -> name -> fileTypes -> docWindow -> RawId -> Sel -> Ptr () -> IO ()
beginSheetForDirectory_file_types_modalForWindow_modalDelegate_didEndSelector_contextInfo nsOpenPanel path name fileTypes docWindow delegate didEndSelector contextInfo =
  sendMessage nsOpenPanel beginSheetForDirectory_file_types_modalForWindow_modalDelegate_didEndSelector_contextInfoSelector (toNSString path) (toNSString name) (toNSArray fileTypes) (toNSWindow docWindow) delegate didEndSelector contextInfo

-- | @- beginForDirectory:file:types:modelessDelegate:didEndSelector:contextInfo:@
beginForDirectory_file_types_modelessDelegate_didEndSelector_contextInfo :: (IsNSOpenPanel nsOpenPanel, IsNSString path, IsNSString name, IsNSArray fileTypes) => nsOpenPanel -> path -> name -> fileTypes -> RawId -> Sel -> Ptr () -> IO ()
beginForDirectory_file_types_modelessDelegate_didEndSelector_contextInfo nsOpenPanel path name fileTypes delegate didEndSelector contextInfo =
  sendMessage nsOpenPanel beginForDirectory_file_types_modelessDelegate_didEndSelector_contextInfoSelector (toNSString path) (toNSString name) (toNSArray fileTypes) delegate didEndSelector contextInfo

-- | @- runModalForDirectory:file:types:@
runModalForDirectory_file_types :: (IsNSOpenPanel nsOpenPanel, IsNSString path, IsNSString name, IsNSArray fileTypes) => nsOpenPanel -> path -> name -> fileTypes -> IO CLong
runModalForDirectory_file_types nsOpenPanel path name fileTypes =
  sendMessage nsOpenPanel runModalForDirectory_file_typesSelector (toNSString path) (toNSString name) (toNSArray fileTypes)

-- | @- runModalForTypes:@
runModalForTypes :: (IsNSOpenPanel nsOpenPanel, IsNSArray fileTypes) => nsOpenPanel -> fileTypes -> IO CLong
runModalForTypes nsOpenPanel fileTypes =
  sendMessage nsOpenPanel runModalForTypesSelector (toNSArray fileTypes)

-- | @- URLs@
urLs :: IsNSOpenPanel nsOpenPanel => nsOpenPanel -> IO (Id NSArray)
urLs nsOpenPanel =
  sendMessage nsOpenPanel urLsSelector

-- | @- resolvesAliases@
resolvesAliases :: IsNSOpenPanel nsOpenPanel => nsOpenPanel -> IO Bool
resolvesAliases nsOpenPanel =
  sendMessage nsOpenPanel resolvesAliasesSelector

-- | @- setResolvesAliases:@
setResolvesAliases :: IsNSOpenPanel nsOpenPanel => nsOpenPanel -> Bool -> IO ()
setResolvesAliases nsOpenPanel value =
  sendMessage nsOpenPanel setResolvesAliasesSelector value

-- | @- canChooseDirectories@
canChooseDirectories :: IsNSOpenPanel nsOpenPanel => nsOpenPanel -> IO Bool
canChooseDirectories nsOpenPanel =
  sendMessage nsOpenPanel canChooseDirectoriesSelector

-- | @- setCanChooseDirectories:@
setCanChooseDirectories :: IsNSOpenPanel nsOpenPanel => nsOpenPanel -> Bool -> IO ()
setCanChooseDirectories nsOpenPanel value =
  sendMessage nsOpenPanel setCanChooseDirectoriesSelector value

-- | @- allowsMultipleSelection@
allowsMultipleSelection :: IsNSOpenPanel nsOpenPanel => nsOpenPanel -> IO Bool
allowsMultipleSelection nsOpenPanel =
  sendMessage nsOpenPanel allowsMultipleSelectionSelector

-- | @- setAllowsMultipleSelection:@
setAllowsMultipleSelection :: IsNSOpenPanel nsOpenPanel => nsOpenPanel -> Bool -> IO ()
setAllowsMultipleSelection nsOpenPanel value =
  sendMessage nsOpenPanel setAllowsMultipleSelectionSelector value

-- | @- canChooseFiles@
canChooseFiles :: IsNSOpenPanel nsOpenPanel => nsOpenPanel -> IO Bool
canChooseFiles nsOpenPanel =
  sendMessage nsOpenPanel canChooseFilesSelector

-- | @- setCanChooseFiles:@
setCanChooseFiles :: IsNSOpenPanel nsOpenPanel => nsOpenPanel -> Bool -> IO ()
setCanChooseFiles nsOpenPanel value =
  sendMessage nsOpenPanel setCanChooseFilesSelector value

-- | @- canResolveUbiquitousConflicts@
canResolveUbiquitousConflicts :: IsNSOpenPanel nsOpenPanel => nsOpenPanel -> IO Bool
canResolveUbiquitousConflicts nsOpenPanel =
  sendMessage nsOpenPanel canResolveUbiquitousConflictsSelector

-- | @- setCanResolveUbiquitousConflicts:@
setCanResolveUbiquitousConflicts :: IsNSOpenPanel nsOpenPanel => nsOpenPanel -> Bool -> IO ()
setCanResolveUbiquitousConflicts nsOpenPanel value =
  sendMessage nsOpenPanel setCanResolveUbiquitousConflictsSelector value

-- | @- canDownloadUbiquitousContents@
canDownloadUbiquitousContents :: IsNSOpenPanel nsOpenPanel => nsOpenPanel -> IO Bool
canDownloadUbiquitousContents nsOpenPanel =
  sendMessage nsOpenPanel canDownloadUbiquitousContentsSelector

-- | @- setCanDownloadUbiquitousContents:@
setCanDownloadUbiquitousContents :: IsNSOpenPanel nsOpenPanel => nsOpenPanel -> Bool -> IO ()
setCanDownloadUbiquitousContents nsOpenPanel value =
  sendMessage nsOpenPanel setCanDownloadUbiquitousContentsSelector value

-- | @- accessoryViewDisclosed@
accessoryViewDisclosed :: IsNSOpenPanel nsOpenPanel => nsOpenPanel -> IO Bool
accessoryViewDisclosed nsOpenPanel =
  sendMessage nsOpenPanel accessoryViewDisclosedSelector

-- | @- setAccessoryViewDisclosed:@
setAccessoryViewDisclosed :: IsNSOpenPanel nsOpenPanel => nsOpenPanel -> Bool -> IO ()
setAccessoryViewDisclosed nsOpenPanel value =
  sendMessage nsOpenPanel setAccessoryViewDisclosedSelector value

-- | @- showsContentTypes@
showsContentTypes :: IsNSOpenPanel nsOpenPanel => nsOpenPanel -> IO Bool
showsContentTypes nsOpenPanel =
  sendMessage nsOpenPanel showsContentTypesSelector

-- | @- setShowsContentTypes:@
setShowsContentTypes :: IsNSOpenPanel nsOpenPanel => nsOpenPanel -> Bool -> IO ()
setShowsContentTypes nsOpenPanel value =
  sendMessage nsOpenPanel setShowsContentTypesSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @openPanel@
openPanelSelector :: Selector '[] (Id NSOpenPanel)
openPanelSelector = mkSelector "openPanel"

-- | @Selector@ for @filenames@
filenamesSelector :: Selector '[] (Id NSArray)
filenamesSelector = mkSelector "filenames"

-- | @Selector@ for @beginSheetForDirectory:file:types:modalForWindow:modalDelegate:didEndSelector:contextInfo:@
beginSheetForDirectory_file_types_modalForWindow_modalDelegate_didEndSelector_contextInfoSelector :: Selector '[Id NSString, Id NSString, Id NSArray, Id NSWindow, RawId, Sel, Ptr ()] ()
beginSheetForDirectory_file_types_modalForWindow_modalDelegate_didEndSelector_contextInfoSelector = mkSelector "beginSheetForDirectory:file:types:modalForWindow:modalDelegate:didEndSelector:contextInfo:"

-- | @Selector@ for @beginForDirectory:file:types:modelessDelegate:didEndSelector:contextInfo:@
beginForDirectory_file_types_modelessDelegate_didEndSelector_contextInfoSelector :: Selector '[Id NSString, Id NSString, Id NSArray, RawId, Sel, Ptr ()] ()
beginForDirectory_file_types_modelessDelegate_didEndSelector_contextInfoSelector = mkSelector "beginForDirectory:file:types:modelessDelegate:didEndSelector:contextInfo:"

-- | @Selector@ for @runModalForDirectory:file:types:@
runModalForDirectory_file_typesSelector :: Selector '[Id NSString, Id NSString, Id NSArray] CLong
runModalForDirectory_file_typesSelector = mkSelector "runModalForDirectory:file:types:"

-- | @Selector@ for @runModalForTypes:@
runModalForTypesSelector :: Selector '[Id NSArray] CLong
runModalForTypesSelector = mkSelector "runModalForTypes:"

-- | @Selector@ for @URLs@
urLsSelector :: Selector '[] (Id NSArray)
urLsSelector = mkSelector "URLs"

-- | @Selector@ for @resolvesAliases@
resolvesAliasesSelector :: Selector '[] Bool
resolvesAliasesSelector = mkSelector "resolvesAliases"

-- | @Selector@ for @setResolvesAliases:@
setResolvesAliasesSelector :: Selector '[Bool] ()
setResolvesAliasesSelector = mkSelector "setResolvesAliases:"

-- | @Selector@ for @canChooseDirectories@
canChooseDirectoriesSelector :: Selector '[] Bool
canChooseDirectoriesSelector = mkSelector "canChooseDirectories"

-- | @Selector@ for @setCanChooseDirectories:@
setCanChooseDirectoriesSelector :: Selector '[Bool] ()
setCanChooseDirectoriesSelector = mkSelector "setCanChooseDirectories:"

-- | @Selector@ for @allowsMultipleSelection@
allowsMultipleSelectionSelector :: Selector '[] Bool
allowsMultipleSelectionSelector = mkSelector "allowsMultipleSelection"

-- | @Selector@ for @setAllowsMultipleSelection:@
setAllowsMultipleSelectionSelector :: Selector '[Bool] ()
setAllowsMultipleSelectionSelector = mkSelector "setAllowsMultipleSelection:"

-- | @Selector@ for @canChooseFiles@
canChooseFilesSelector :: Selector '[] Bool
canChooseFilesSelector = mkSelector "canChooseFiles"

-- | @Selector@ for @setCanChooseFiles:@
setCanChooseFilesSelector :: Selector '[Bool] ()
setCanChooseFilesSelector = mkSelector "setCanChooseFiles:"

-- | @Selector@ for @canResolveUbiquitousConflicts@
canResolveUbiquitousConflictsSelector :: Selector '[] Bool
canResolveUbiquitousConflictsSelector = mkSelector "canResolveUbiquitousConflicts"

-- | @Selector@ for @setCanResolveUbiquitousConflicts:@
setCanResolveUbiquitousConflictsSelector :: Selector '[Bool] ()
setCanResolveUbiquitousConflictsSelector = mkSelector "setCanResolveUbiquitousConflicts:"

-- | @Selector@ for @canDownloadUbiquitousContents@
canDownloadUbiquitousContentsSelector :: Selector '[] Bool
canDownloadUbiquitousContentsSelector = mkSelector "canDownloadUbiquitousContents"

-- | @Selector@ for @setCanDownloadUbiquitousContents:@
setCanDownloadUbiquitousContentsSelector :: Selector '[Bool] ()
setCanDownloadUbiquitousContentsSelector = mkSelector "setCanDownloadUbiquitousContents:"

-- | @Selector@ for @accessoryViewDisclosed@
accessoryViewDisclosedSelector :: Selector '[] Bool
accessoryViewDisclosedSelector = mkSelector "accessoryViewDisclosed"

-- | @Selector@ for @setAccessoryViewDisclosed:@
setAccessoryViewDisclosedSelector :: Selector '[Bool] ()
setAccessoryViewDisclosedSelector = mkSelector "setAccessoryViewDisclosed:"

-- | @Selector@ for @showsContentTypes@
showsContentTypesSelector :: Selector '[] Bool
showsContentTypesSelector = mkSelector "showsContentTypes"

-- | @Selector@ for @setShowsContentTypes:@
setShowsContentTypesSelector :: Selector '[Bool] ()
setShowsContentTypesSelector = mkSelector "setShowsContentTypes:"

