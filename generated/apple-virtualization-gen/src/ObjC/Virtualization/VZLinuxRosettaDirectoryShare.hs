{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Directory share to enable Rosetta support for Linux binaries.
--
-- This directory share exposes Rosetta within a shared directory in the guest. Linux can use it to translate x86_64 binaries.
--
-- See: VZDirectorySharingDeviceConfiguration
--
-- See: VZSharedDirectory
--
-- Generated bindings for @VZLinuxRosettaDirectoryShare@.
module ObjC.Virtualization.VZLinuxRosettaDirectoryShare
  ( VZLinuxRosettaDirectoryShare
  , IsVZLinuxRosettaDirectoryShare(..)
  , initWithError
  , installRosettaWithCompletionHandler
  , options
  , setOptions
  , availability
  , availabilitySelector
  , initWithErrorSelector
  , installRosettaWithCompletionHandlerSelector
  , optionsSelector
  , setOptionsSelector

  -- * Enum types
  , VZLinuxRosettaAvailability(VZLinuxRosettaAvailability)
  , pattern VZLinuxRosettaAvailabilityNotSupported
  , pattern VZLinuxRosettaAvailabilityNotInstalled
  , pattern VZLinuxRosettaAvailabilityInstalled

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Virtualization.Internal.Classes
import ObjC.Virtualization.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Initialize a Rosetta directory share if Rosetta support for Linux binaries is installed.
--
-- @error@ — Error object to store the error, if an error exists.
--
-- The call returns an error if Rosetta is not available for a directory share. To install Rosetta support, use +[VZLinuxRosettaDirectoryShare installRosettaIfNeeded:].
--
-- See: +[VZLinuxRosettaDirectoryShare installRosettaIfNeeded:]
--
-- ObjC selector: @- initWithError:@
initWithError :: (IsVZLinuxRosettaDirectoryShare vzLinuxRosettaDirectoryShare, IsNSError error_) => vzLinuxRosettaDirectoryShare -> error_ -> IO (Id VZLinuxRosettaDirectoryShare)
initWithError vzLinuxRosettaDirectoryShare error_ =
  sendOwnedMessage vzLinuxRosettaDirectoryShare initWithErrorSelector (toNSError error_)

-- | Download and install Rosetta support for Linux binaries if necessary.
--
-- @completionHandler@ — The completion handler gets called with a valid error on failure and a nil error on success. It will also be invoked on an arbitrary queue.
--
-- The call prompts the user through the download and install flow for Rosetta. This call is successful if the error is nil.
--
-- See: +[VZLinuxRosettaDirectoryShare availability]
--
-- ObjC selector: @+ installRosettaWithCompletionHandler:@
installRosettaWithCompletionHandler :: Ptr () -> IO ()
installRosettaWithCompletionHandler completionHandler =
  do
    cls' <- getRequiredClass "VZLinuxRosettaDirectoryShare"
    sendClassMessage cls' installRosettaWithCompletionHandlerSelector completionHandler

-- | Enable translation caching and configure the socket communication type for Rosetta.
--
-- ObjC selector: @- options@
options :: IsVZLinuxRosettaDirectoryShare vzLinuxRosettaDirectoryShare => vzLinuxRosettaDirectoryShare -> IO (Id VZLinuxRosettaCachingOptions)
options vzLinuxRosettaDirectoryShare =
  sendMessage vzLinuxRosettaDirectoryShare optionsSelector

-- | Enable translation caching and configure the socket communication type for Rosetta.
--
-- ObjC selector: @- setOptions:@
setOptions :: (IsVZLinuxRosettaDirectoryShare vzLinuxRosettaDirectoryShare, IsVZLinuxRosettaCachingOptions value) => vzLinuxRosettaDirectoryShare -> value -> IO ()
setOptions vzLinuxRosettaDirectoryShare value =
  sendMessage vzLinuxRosettaDirectoryShare setOptionsSelector (toVZLinuxRosettaCachingOptions value)

-- | Check the availability of Rosetta support for the directory share.
--
-- ObjC selector: @+ availability@
availability :: IO VZLinuxRosettaAvailability
availability  =
  do
    cls' <- getRequiredClass "VZLinuxRosettaDirectoryShare"
    sendClassMessage cls' availabilitySelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithError:@
initWithErrorSelector :: Selector '[Id NSError] (Id VZLinuxRosettaDirectoryShare)
initWithErrorSelector = mkSelector "initWithError:"

-- | @Selector@ for @installRosettaWithCompletionHandler:@
installRosettaWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
installRosettaWithCompletionHandlerSelector = mkSelector "installRosettaWithCompletionHandler:"

-- | @Selector@ for @options@
optionsSelector :: Selector '[] (Id VZLinuxRosettaCachingOptions)
optionsSelector = mkSelector "options"

-- | @Selector@ for @setOptions:@
setOptionsSelector :: Selector '[Id VZLinuxRosettaCachingOptions] ()
setOptionsSelector = mkSelector "setOptions:"

-- | @Selector@ for @availability@
availabilitySelector :: Selector '[] VZLinuxRosettaAvailability
availabilitySelector = mkSelector "availability"

