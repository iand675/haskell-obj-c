{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | OBEXFileTransferServices
--
-- Implements advanced OBEX operations in addition to simple PUT and GET.
--
-- All operations are asynchronous and will callback over a respective delegate 					method if the initial return value is successful.  The initial return value 					usually concerns the state of this object where as the delegate return value					reflects the response of the remote device.
--
-- Generated bindings for @BluetoothFileReference@.
module ObjC.IOBluetooth.BluetoothFileReference
  ( BluetoothFileReference
  , IsBluetoothFileReference(..)


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.IOBluetooth.Internal.Classes

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

