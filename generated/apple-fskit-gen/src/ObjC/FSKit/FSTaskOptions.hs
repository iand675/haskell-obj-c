{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A class that passes command options to a task, optionally providing security-scoped URLs.
--
-- Generated bindings for @FSTaskOptions@.
module ObjC.FSKit.FSTaskOptions
  ( FSTaskOptions
  , IsFSTaskOptions(..)
  , init_
  , urlForOption
  , taskOptions
  , initSelector
  , taskOptionsSelector
  , urlForOptionSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.FSKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsFSTaskOptions fsTaskOptions => fsTaskOptions -> IO (Id FSTaskOptions)
init_ fsTaskOptions =
  sendOwnedMessage fsTaskOptions initSelector

-- | Retrieves a URL for a given option.
--
-- Some command-line options refer to paths that indicate a location in which the module needs access to a file outside of its container. FSKit passes these paths as a URL tagged by the option name.
--
-- For example, @"-B" "./someFile"@ returns the URL for @./someFile@ when passed an option @"B"@. To indicate that your module treats a given option as a path, include it in the @pathOptions@ dictionary within a command options dictionary (@FSActivatOptionSyntax@, @FSCheckOptionSyntax@, or @FSFormatOptionSyntax@). This dictionary uses the command option name as a key, and each entry has a value indicating what kind of entry to create.
--
-- - Parameter option: The option for which to retrieve the URL. This value doesn't include leading dashes.
--
-- ObjC selector: @- urlForOption:@
urlForOption :: (IsFSTaskOptions fsTaskOptions, IsNSString option) => fsTaskOptions -> option -> IO (Id NSURL)
urlForOption fsTaskOptions option =
  sendMessage fsTaskOptions urlForOptionSelector (toNSString option)

-- | An array of strings that represent command-line options for the task.
--
-- This property is equivalent to the @argv@ array of C strings passed to a command-line tool.
--
-- ObjC selector: @- taskOptions@
taskOptions :: IsFSTaskOptions fsTaskOptions => fsTaskOptions -> IO (Id NSArray)
taskOptions fsTaskOptions =
  sendMessage fsTaskOptions taskOptionsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id FSTaskOptions)
initSelector = mkSelector "init"

-- | @Selector@ for @urlForOption:@
urlForOptionSelector :: Selector '[Id NSString] (Id NSURL)
urlForOptionSelector = mkSelector "urlForOption:"

-- | @Selector@ for @taskOptions@
taskOptionsSelector :: Selector '[] (Id NSArray)
taskOptionsSelector = mkSelector "taskOptions"

