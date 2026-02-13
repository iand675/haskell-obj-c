{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Caching options for an Abstract Socket.
--
-- This object configures Rosetta to communicate with the Rosetta daemon using an Abstract Socket.
--
-- See: VZLinuxRosettaCachingOptions
--
-- Generated bindings for @VZLinuxRosettaAbstractSocketCachingOptions@.
module ObjC.Virtualization.VZLinuxRosettaAbstractSocketCachingOptions
  ( VZLinuxRosettaAbstractSocketCachingOptions
  , IsVZLinuxRosettaAbstractSocketCachingOptions(..)
  , initWithName_error
  , name
  , maximumNameLength
  , initWithName_errorSelector
  , maximumNameLengthSelector
  , nameSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Virtualization.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Initialize options to be set on a VZLinuxRosettaDirectoryShare.
--
-- @name@ — The name of the Abstract Socket to be used to communicate with the Rosetta translation daemon. This cannot exceed maximumNameLength UTF-8 bytes long.
--
-- @error@ — If not nil, assigned with the error if the initialization failed.
--
-- Rosetta can be optionally configured to use cached translations from the Rosetta translation daemon communicating through an Abstract Socket.    If name exceeds maximumNameLength UTF-8 bytes, nil is returned and the error is set.
--
-- ObjC selector: @- initWithName:error:@
initWithName_error :: (IsVZLinuxRosettaAbstractSocketCachingOptions vzLinuxRosettaAbstractSocketCachingOptions, IsNSString name, IsNSError error_) => vzLinuxRosettaAbstractSocketCachingOptions -> name -> error_ -> IO (Id VZLinuxRosettaAbstractSocketCachingOptions)
initWithName_error vzLinuxRosettaAbstractSocketCachingOptions name error_ =
  sendOwnedMessage vzLinuxRosettaAbstractSocketCachingOptions initWithName_errorSelector (toNSString name) (toNSError error_)

-- | Name set by initWithName.
--
-- This is the name of the Abstract Socket to be used by Rosetta.
--
-- ObjC selector: @- name@
name :: IsVZLinuxRosettaAbstractSocketCachingOptions vzLinuxRosettaAbstractSocketCachingOptions => vzLinuxRosettaAbstractSocketCachingOptions -> IO (Id NSString)
name vzLinuxRosettaAbstractSocketCachingOptions =
  sendMessage vzLinuxRosettaAbstractSocketCachingOptions nameSelector

-- | The maximum allowed length of name, as defined by the sockaddr_un structure in Linux.
--
-- ObjC selector: @+ maximumNameLength@
maximumNameLength :: IO CULong
maximumNameLength  =
  do
    cls' <- getRequiredClass "VZLinuxRosettaAbstractSocketCachingOptions"
    sendClassMessage cls' maximumNameLengthSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithName:error:@
initWithName_errorSelector :: Selector '[Id NSString, Id NSError] (Id VZLinuxRosettaAbstractSocketCachingOptions)
initWithName_errorSelector = mkSelector "initWithName:error:"

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

-- | @Selector@ for @maximumNameLength@
maximumNameLengthSelector :: Selector '[] CULong
maximumNameLengthSelector = mkSelector "maximumNameLength"

