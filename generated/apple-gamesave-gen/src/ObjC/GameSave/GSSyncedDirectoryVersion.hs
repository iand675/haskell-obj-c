{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @GSSyncedDirectoryVersion@.
module ObjC.GameSave.GSSyncedDirectoryVersion
  ( GSSyncedDirectoryVersion
  , IsGSSyncedDirectoryVersion(..)
  , init_
  , new
  , isLocal
  , localizedNameOfSavingComputer
  , modifiedDate
  , url
  , description
  , descriptionSelector
  , initSelector
  , isLocalSelector
  , localizedNameOfSavingComputerSelector
  , modifiedDateSelector
  , newSelector
  , urlSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.GameSave.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsGSSyncedDirectoryVersion gsSyncedDirectoryVersion => gsSyncedDirectoryVersion -> IO (Id GSSyncedDirectoryVersion)
init_ gsSyncedDirectoryVersion =
  sendOwnedMessage gsSyncedDirectoryVersion initSelector

-- | @- new@
new :: IsGSSyncedDirectoryVersion gsSyncedDirectoryVersion => gsSyncedDirectoryVersion -> IO (Id GSSyncedDirectoryVersion)
new gsSyncedDirectoryVersion =
  sendOwnedMessage gsSyncedDirectoryVersion newSelector

-- | @YES@ if the directory version is local; otherwise @NO@.
--
-- ObjC selector: @- isLocal@
isLocal :: IsGSSyncedDirectoryVersion gsSyncedDirectoryVersion => gsSyncedDirectoryVersion -> IO Bool
isLocal gsSyncedDirectoryVersion =
  sendMessage gsSyncedDirectoryVersion isLocalSelector

-- | The localized name of the device that saved this version.
--
-- ObjC selector: @- localizedNameOfSavingComputer@
localizedNameOfSavingComputer :: IsGSSyncedDirectoryVersion gsSyncedDirectoryVersion => gsSyncedDirectoryVersion -> IO (Id NSString)
localizedNameOfSavingComputer gsSyncedDirectoryVersion =
  sendMessage gsSyncedDirectoryVersion localizedNameOfSavingComputerSelector

-- | The date that this version was last modified.
--
-- ObjC selector: @- modifiedDate@
modifiedDate :: IsGSSyncedDirectoryVersion gsSyncedDirectoryVersion => gsSyncedDirectoryVersion -> IO (Id NSDate)
modifiedDate gsSyncedDirectoryVersion =
  sendMessage gsSyncedDirectoryVersion modifiedDateSelector

-- | The URL of a directory where you read and write game-save data.
--
-- You define the format and structure of files you write in this directory.
--
-- ObjC selector: @- url@
url :: IsGSSyncedDirectoryVersion gsSyncedDirectoryVersion => gsSyncedDirectoryVersion -> IO (Id NSURL)
url gsSyncedDirectoryVersion =
  sendMessage gsSyncedDirectoryVersion urlSelector

-- | @- description@
description :: IsGSSyncedDirectoryVersion gsSyncedDirectoryVersion => gsSyncedDirectoryVersion -> IO (Id NSString)
description gsSyncedDirectoryVersion =
  sendMessage gsSyncedDirectoryVersion descriptionSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id GSSyncedDirectoryVersion)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id GSSyncedDirectoryVersion)
newSelector = mkSelector "new"

-- | @Selector@ for @isLocal@
isLocalSelector :: Selector '[] Bool
isLocalSelector = mkSelector "isLocal"

-- | @Selector@ for @localizedNameOfSavingComputer@
localizedNameOfSavingComputerSelector :: Selector '[] (Id NSString)
localizedNameOfSavingComputerSelector = mkSelector "localizedNameOfSavingComputer"

-- | @Selector@ for @modifiedDate@
modifiedDateSelector :: Selector '[] (Id NSDate)
modifiedDateSelector = mkSelector "modifiedDate"

-- | @Selector@ for @url@
urlSelector :: Selector '[] (Id NSURL)
urlSelector = mkSelector "url"

-- | @Selector@ for @description@
descriptionSelector :: Selector '[] (Id NSString)
descriptionSelector = mkSelector "description"

