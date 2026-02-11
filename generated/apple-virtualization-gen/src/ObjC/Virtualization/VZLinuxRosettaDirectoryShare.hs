{-# LANGUAGE PatternSynonyms #-}
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
  , initWithErrorSelector
  , installRosettaWithCompletionHandlerSelector
  , optionsSelector
  , setOptionsSelector
  , availabilitySelector

  -- * Enum types
  , VZLinuxRosettaAvailability(VZLinuxRosettaAvailability)
  , pattern VZLinuxRosettaAvailabilityNotSupported
  , pattern VZLinuxRosettaAvailabilityNotInstalled
  , pattern VZLinuxRosettaAvailabilityInstalled

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
initWithError vzLinuxRosettaDirectoryShare  error_ =
  withObjCPtr error_ $ \raw_error_ ->
      sendMsg vzLinuxRosettaDirectoryShare (mkSelector "initWithError:") (retPtr retVoid) [argPtr (castPtr raw_error_ :: Ptr ())] >>= ownedObject . castPtr

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
    sendClassMsg cls' (mkSelector "installRosettaWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | Enable translation caching and configure the socket communication type for Rosetta.
--
-- ObjC selector: @- options@
options :: IsVZLinuxRosettaDirectoryShare vzLinuxRosettaDirectoryShare => vzLinuxRosettaDirectoryShare -> IO (Id VZLinuxRosettaCachingOptions)
options vzLinuxRosettaDirectoryShare  =
    sendMsg vzLinuxRosettaDirectoryShare (mkSelector "options") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Enable translation caching and configure the socket communication type for Rosetta.
--
-- ObjC selector: @- setOptions:@
setOptions :: (IsVZLinuxRosettaDirectoryShare vzLinuxRosettaDirectoryShare, IsVZLinuxRosettaCachingOptions value) => vzLinuxRosettaDirectoryShare -> value -> IO ()
setOptions vzLinuxRosettaDirectoryShare  value =
  withObjCPtr value $ \raw_value ->
      sendMsg vzLinuxRosettaDirectoryShare (mkSelector "setOptions:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Check the availability of Rosetta support for the directory share.
--
-- ObjC selector: @+ availability@
availability :: IO VZLinuxRosettaAvailability
availability  =
  do
    cls' <- getRequiredClass "VZLinuxRosettaDirectoryShare"
    fmap (coerce :: CLong -> VZLinuxRosettaAvailability) $ sendClassMsg cls' (mkSelector "availability") retCLong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithError:@
initWithErrorSelector :: Selector
initWithErrorSelector = mkSelector "initWithError:"

-- | @Selector@ for @installRosettaWithCompletionHandler:@
installRosettaWithCompletionHandlerSelector :: Selector
installRosettaWithCompletionHandlerSelector = mkSelector "installRosettaWithCompletionHandler:"

-- | @Selector@ for @options@
optionsSelector :: Selector
optionsSelector = mkSelector "options"

-- | @Selector@ for @setOptions:@
setOptionsSelector :: Selector
setOptionsSelector = mkSelector "setOptions:"

-- | @Selector@ for @availability@
availabilitySelector :: Selector
availabilitySelector = mkSelector "availability"

